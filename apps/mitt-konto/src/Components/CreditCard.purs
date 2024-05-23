module MittKonto.Components.CreditCard where

import Prelude

import Bottega (BottegaError(..), bottegaErrorMessage)
import Bottega.Models (CreditCardRegister, CreditCardRegisterState(..), FailReason(..), RegisterCallback(MittKonto))
import Data.Either (Either(..))
import Data.Maybe (Maybe(..), fromMaybe, isNothing)
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Aff as Aff
import Effect.Aff.AVar as Aff.AVar
import Effect.AVar (AVar)
import Effect.AVar (empty, tryPut, tryTake) as AVar
import Effect.Class (liftEffect)
import Effect.Exception (error)
import KSF.Api.Subscription (Subsno)
import KSF.AsyncWrapper as AsyncWrapper
import KSF.CreditCard.Register (render, scaRequired) as Register
import KSF.Sentry as Sentry
import KSF.Spinner (loadingSpinner)
import KSF.User (getCreditCardRegister, registerCreditCardProcess, registerCreditCardForSubscription) as User
import KSF.User.Cusno (Cusno)
import MittKonto.Routes (CreditCardCallbackParams)
import MittKonto.Wrappers (AutoClose(..), SetRouteWrapperState)
import MittKonto.Wrappers.Elements as WrapperElements
import React.Basic (JSX)
import React.Basic.Hooks (Component, useState, useEffectOnce, (/\))
import React.Basic.Hooks as React
import React.Basic.DOM as DOM

foreign import openLocation :: String -> Effect Unit

type BaseProps =
  ( cusno       :: Cusno
  , logger      :: Sentry.Logger
  , subsno      :: Subsno
  , callback    :: Maybe CreditCardCallbackParams
  , cardsChanged :: Effect Unit
  )

type Props =
  { setWrapperState :: SetRouteWrapperState
  | BaseProps
  }

type Self =
  { state :: State
  , setState :: SetState
  , props :: Props
  }

type State =
  { asyncWrapperState :: AsyncWrapper.Progress JSX
  , updateState       :: UpdateState
  , poller            :: Aff.Fiber Unit
  , paymentTerminal   :: Maybe String
  , scaShown          :: Boolean
  }

type SetState = (State -> State) -> Effect Unit

data UpdateState
  = ChooseCreditCard
  | RegisterCreditCard
  | ScaRequired String

component :: Maybe CreditCardCallbackParams -> Component Props
component registerParams = do
  closed <- AVar.empty
  processResponse <- AVar.empty
  -- If we have the data just start it already.  It needs no auth.
  case registerParams of
    Nothing -> pure unit
    Just register | register.responseCode == "OK" -> Aff.launchAff_ do
      flip Aff.AVar.put processResponse =<< User.registerCreditCardProcess register.transactionId
    Just _ -> do
      _ <- flip AVar.tryPut processResponse $
           Left $ BottegaUnexpectedError "Nets response code was not OK"
      pure unit

  React.component "CreditCardUpdateView" \props -> React.do
    state /\ setState <- useState initialState

    let self = { state, setState, props}
    useEffectOnce do
      -- Just in case user navigates again after initial load, get it
      -- from props.
      case props.callback of
        Nothing -> do
          props.setWrapperState \s -> s { titleText = "Uppdatera ditt kredit- eller bankkort" }
          setState _ { asyncWrapperState = AsyncWrapper.Editing $ DOM.text
                                           "Du styrs strax till vår betalningsbehandlare Nets."
                     }
          Aff.launchAff_ $ registerCreditCard self
          pure mempty
        Just callbackParams -> do
          props.setWrapperState \s -> s { titleText = "Uppdatera ditt kredit- eller bankkort" }
          setState _ { asyncWrapperState = AsyncWrapper.Editing $ DOM.text "Vänligen vänta. Behandlas."
                     }

          _ <- AVar.tryTake closed
          Aff.launchAff_ $ callbackDone self closed processResponse callbackParams
          pure do
            _ <- AVar.tryPut unit closed
            pure unit
    pure $ render self

initialState :: State
initialState =
  { asyncWrapperState: AsyncWrapper.Ready
  , poller: pure unit
  , updateState: ChooseCreditCard
  , paymentTerminal: Nothing
  , scaShown: false
  }

render :: Self -> JSX
render { setState, state: { asyncWrapperState, updateState } } =
  asyncWrapper $ DOM.div
    { className: "credit-card-update--container"
    , children:
        [ case updateState of
            ChooseCreditCard -> mempty
            RegisterCreditCard -> Register.render
            ScaRequired url -> Register.scaRequired url
        ]
    }
  where
    asyncWrapper :: JSX -> JSX
    asyncWrapper content =
      AsyncWrapper.asyncWrapper
        { wrapperState: asyncWrapperState
        , readyView: content
        , editingView: (_ <> loadingSpinner)
        , successView: fromMaybe mempty
        , errorView: \err -> WrapperElements.errorWrapper onTryAgain err
        , loadingView: identity
        }
    onTryAgain :: Effect Unit
    onTryAgain = setState \s -> s { asyncWrapperState = AsyncWrapper.Ready }

-- Take user to Nets terminal
registerCreditCard :: Self -> Aff Unit
registerCreditCard self@{ props: { logger, subsno } } = do
  creditCardRegister <- User.registerCreditCardForSubscription (Just MittKonto) subsno
  case creditCardRegister of
    Right { terminalUrl: Just { paymentTerminalUrl: url } } -> do
      liftEffect $ openLocation url
    Right { terminalUrl: Nothing } -> do
      liftEffect do
        logger.log "No terminal url received" Sentry.Error
        onError self "2"
    Left err -> do
      liftEffect do
        let msg = bottegaErrorMessage err
        -- Sentry is flooded, may or may not show up
        logger.log ("Got the following error when registering credit card: " <> msg) Sentry.Error
        onError self $ "3, " <> msg

-- Tell Bottega we're done with Nets terminal and poll for a result
callbackDone :: Self -> AVar Unit -> AVar (Either BottegaError Unit) -> CreditCardCallbackParams -> Aff Unit
callbackDone self@{ state, props: { setWrapperState } } closed processResult register = do
  liftEffect $ setWrapperState _ { closeable = true }
  let newState = state { updateState = RegisterCreditCard }
  process <- Aff.AVar.read processResult
  case process of
    Right _ -> startRegisterPoller (self { state = newState }) closed register
    Left err -> liftEffect $ onError self $ "1, " <> bottegaErrorMessage err

killRegisterPoller :: State -> Aff Unit
killRegisterPoller state = Aff.killFiber (error "Canceled poller") state.poller

startRegisterPoller :: Self -> AVar Unit -> CreditCardCallbackParams -> Aff Unit
startRegisterPoller self@{ setState, state } closed {registerNumber, registerCardId} = do
  newPoller <- Aff.forkAff do
    killRegisterPoller state
    register <- User.getCreditCardRegister registerCardId registerNumber
    let delay = case (_.state <<< _.status) <$> register of
          Right CreditCardRegisterStarted -> true
          Right CreditCardRegisterCreated -> true
          _ -> false
    when delay $ Aff.delay $ Aff.Milliseconds 1000.0
    newPoller <- Aff.forkAff $ pollRegister self closed register
    Aff.joinFiber newPoller
  liftEffect $ setState _ { poller = newPoller }

pollRegister :: Self -> AVar Unit -> Either BottegaError CreditCardRegister -> Aff Unit
pollRegister self@{ props: { logger } } closed (Right register) = do
  case register.status.state of
    CreditCardRegisterStarted ->
      delayedPollRegister =<< User.getCreditCardRegister register.creditCardId register.number
    CreditCardRegisterCompleted -> liftEffect do
      onSuccess self
    CreditCardRegisterFailed reason -> liftEffect do
      case reason of
        NetsIssuerError -> self.setState _ { asyncWrapperState = AsyncWrapper.Error "Betalning nekades av kortutgivaren. Vänligen kontakta din bank." }
        _ -> onError self "4"
    CreditCardRegisterScaRequired -> liftEffect do
      case self.state.scaShown /\ self.state.paymentTerminal of
        false /\ Just url -> do
          self.setState _ { updateState = ScaRequired url, scaShown = true }
        _ -> pure unit
    CreditCardRegisterCanceled -> liftEffect do
      onCancel self
    CreditCardRegisterCreated -> delayedPollRegister =<< User.getCreditCardRegister register.creditCardId register.number
    CreditCardRegisterUnknownState -> liftEffect do
      logger.log "Server is in an unknown state" Sentry.Info
      onError self "5"
  where
    delayedPollRegister :: Either BottegaError CreditCardRegister -> Aff Unit
    delayedPollRegister eitherRegister = do
      componentOpen <- isNothing <$> Aff.AVar.tryRead closed
      when componentOpen do
        Aff.delay $ Aff.Milliseconds 1000.0
        pollRegister self closed eitherRegister

pollRegister self@{ props: { logger } } _ (Left err) = liftEffect do
  let msg = bottegaErrorMessage err
  logger.log ("Could not fetch register status: " <> msg) Sentry.Error
  onError self $ "6, " <> msg

onCancel :: Self -> Effect Unit
onCancel { props: { setWrapperState } } = setWrapperState _ { closeAutomatically = Immediate }

onLoading :: Self -> Effect Unit
onLoading { setState } = setState _ { asyncWrapperState = AsyncWrapper.Loading mempty }

onSuccess :: Self -> Effect Unit
onSuccess { setState, props: { setWrapperState, cardsChanged } } = do
  setState _ { asyncWrapperState = AsyncWrapper.Success $ Just $ WrapperElements.successWrapper Nothing "Betalningsinformationen har uppdaterats. Du styrs strax tillbaka till kontosidan." }
  setWrapperState _ { closeable = true
                    , closeAutomatically = Delayed 5000.0
                    , onClose = cardsChanged
                    }

onError :: Self -> String -> Effect Unit
onError { setState } errMsg = setState _ { asyncWrapperState = AsyncWrapper.Error $ "Något gick fel. Vänligen försök pånytt, eller ta kontakt med vår kundtjänst. Felkod: " <> show errMsg }
