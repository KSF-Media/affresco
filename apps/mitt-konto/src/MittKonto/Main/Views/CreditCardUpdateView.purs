module MittKonto.Main.CreditCardUpdateView where

import Prelude

import Bottega (BottegaError, bottegaErrorMessage)
import Bottega.Models (CreditCard, CreditCardRegister, CreditCardRegisterNumber(..), CreditCardRegisterState(..))
import Data.Either (Either(..))
import Data.Maybe (Maybe(..), fromMaybe, isNothing)
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Aff as Aff
import Effect.Aff.AVar (tryRead) as AVar
import Effect.AVar (AVar)
import Effect.AVar (empty, tryPut, tryTake) as AVar
import Effect.Class (liftEffect)
import Effect.Exception (error)
import KSF.Api.Subscription (Subsno)
import KSF.AsyncWrapper as AsyncWrapper
import KSF.CreditCard.Choice (choice) as Choice
import KSF.CreditCard.Register (register) as Register
import KSF.Sentry as Sentry
import KSF.User (PaymentTerminalUrl)
import KSF.User (getCreditCardRegister, registerCreditCardFromExisting) as User
import KSF.User.Cusno (Cusno)
import KSF.Tracking as Tracking
import MittKonto.Wrappers (AutoClose(..), SetRouteWrapperState)
import MittKonto.Wrappers.Elements as WrapperElements
import React.Basic (JSX)
import React.Basic.Hooks (Component, component, useState, useEffectOnce, (/\))
import React.Basic.Hooks as React
import React.Basic.DOM as DOM

type BaseProps =
  ( creditCards :: Array CreditCard
  , cusno       :: Cusno
  , logger      :: Sentry.Logger
  , subsno      :: Subsno
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
  }

type SetState = (State -> State) -> Effect Unit

data UpdateState
  = ChooseCreditCard
  | RegisterCreditCard PaymentTerminalUrl

creditCardUpdateView :: Component Props
creditCardUpdateView = do
  closed <- AVar.empty
  component "CreditCardUpdateView" \props@{ creditCards, logger } -> React.do
    state /\ setState <- useState initialState
    let self = { state, setState, props}
    useEffectOnce $ do
      props.setWrapperState \s -> s { titleText = "Uppdatera ditt kredit- eller bankkort" }
      setState _ { asyncWrapperState = AsyncWrapper.Loading mempty }
      Aff.launchAff_ do
        case creditCards of
          []       -> liftEffect do
            logger.log "No credit cards found" Sentry.Error
            onError self
          [ card ] -> registerCreditCard self closed card
          _        -> liftEffect $ setState _ { asyncWrapperState = AsyncWrapper.Ready }
      _ <- AVar.tryTake closed
      pure do
        _ <- AVar.tryPut unit closed
        pure unit
    pure $ render self closed

initialState :: State
initialState =
  { asyncWrapperState: AsyncWrapper.Ready
  , poller: pure unit
  , updateState: ChooseCreditCard
  }

render :: Self -> AVar Unit -> JSX
render self@{ setState, state: { asyncWrapperState, updateState }, props: { creditCards } } closed =
  asyncWrapper $ DOM.div
    { className: "credit-card-update--container"
    , children:
        [ case updateState of
            ChooseCreditCard       -> Choice.choice
                                        { creditCards: creditCards
                                        , onSubmit: \creditCard -> Aff.launchAff_ $ registerCreditCard self closed creditCard
                                        , onCancel: onCancel self
                                        }

            RegisterCreditCard url -> Register.register
                                        { terminalUrl: url
                                        }
        ]
    }
  where
    asyncWrapper :: JSX -> JSX
    asyncWrapper content =
      AsyncWrapper.asyncWrapper
        { wrapperState: asyncWrapperState
        , readyView: content
        , editingView: identity
        , successView: fromMaybe mempty
        , errorView: \err -> WrapperElements.errorWrapper onTryAgain err
        , loadingView: identity
        }
    onTryAgain :: Effect Unit
    onTryAgain = setState \s -> s { asyncWrapperState = AsyncWrapper.Ready }

registerCreditCard :: Self -> AVar Unit -> CreditCard -> Aff Unit
registerCreditCard self@{ setState, props: { logger, setWrapperState }, state } closed oldCreditCard@{ id } = do
  creditCardRegister <- User.registerCreditCardFromExisting id
  case creditCardRegister of
    Right register@{ terminalUrl: Just url } -> do
      let newState = state { updateState = RegisterCreditCard url }
      liftEffect do
        setState \_ -> newState
        setWrapperState _ { closeable = true }
      void $ Aff.forkAff $ startRegisterPoller self { state = newState } closed oldCreditCard register
    Right { terminalUrl: Nothing } ->
      liftEffect do
        logger.log "No terminal url received" Sentry.Error
        onError self
    Left err ->
      liftEffect do
        logger.log ("Got the following error when registering credit card: " <> bottegaErrorMessage err) Sentry.Error
        onError self

killRegisterPoller :: State -> Aff Unit
killRegisterPoller state = Aff.killFiber (error "Canceled poller") state.poller

startRegisterPoller :: Self -> AVar Unit -> CreditCard -> CreditCardRegister -> Aff Unit
startRegisterPoller self@{ setState, state } closed oldCreditCard creditCardRegister = do
  newPoller <- Aff.forkAff do
    killRegisterPoller state
    newPoller <- Aff.forkAff $ pollRegister self closed oldCreditCard (Right creditCardRegister)
    Aff.joinFiber newPoller
  liftEffect $ setState _ { poller = newPoller }

pollRegister :: Self -> AVar Unit -> CreditCard -> Either BottegaError CreditCardRegister -> Aff Unit
pollRegister self@{ props: { cusno, subsno, logger } } closed oldCreditCard (Right register) = do
  case register.status.state of
    CreditCardRegisterStarted ->
      delayedPollRegister =<< User.getCreditCardRegister register.creditCardId register.number
    CreditCardRegisterCompleted -> liftEffect do
      track "success"
      onSuccess self
    CreditCardRegisterFailed reason -> liftEffect do
      track $ "error:" <> show reason
      onError self
    CreditCardRegisterCanceled -> liftEffect do
      track "cancel"
      onCancel self
    CreditCardRegisterCreated -> delayedPollRegister =<< User.getCreditCardRegister register.creditCardId register.number
    CreditCardRegisterUnknownState -> liftEffect do
      track $ "error: unknown"
      logger.log "Server is in an unknown state" Sentry.Info
      onError self
  where
    delayedPollRegister :: Either BottegaError CreditCardRegister -> Aff Unit
    delayedPollRegister eitherRegister = do
      componentOpen <- isNothing <$> AVar.tryRead closed
      when componentOpen do
        Aff.delay $ Aff.Milliseconds 1000.0
        pollRegister self closed oldCreditCard eitherRegister

    track :: String -> Effect Unit
    track result = do
      Tracking.updateCreditCard cusno subsno (Tracking.readBottegaCreditCard oldCreditCard) (unRegisterNumber register.number) result

    unRegisterNumber :: CreditCardRegisterNumber -> String
    unRegisterNumber (CreditCardRegisterNumber number) = number

pollRegister self@{ props: { logger } } _ _ (Left err) = liftEffect do
  logger.log ("Could not fetch register status: " <> bottegaErrorMessage err) Sentry.Error
  onError self

onCancel :: Self -> Effect Unit
onCancel { props: { setWrapperState } } = setWrapperState _ { closeAutomatically = Immediate }

onLoading :: Self -> Effect Unit
onLoading { setState } = setState _ { asyncWrapperState = AsyncWrapper.Loading mempty }

onSuccess :: Self -> Effect Unit
onSuccess { setState, props: { setWrapperState } } = do
  setState _ { asyncWrapperState = AsyncWrapper.Success $ Just $ WrapperElements.successWrapper Nothing "Betalningsinformationen har uppdaterats. Du styrs strax tillbaka till kontosidan." }
  setWrapperState _ { closeable = true
                    , closeAutomatically = Delayed 5000.0
                    }

onError :: Self -> Effect Unit
onError { setState } = setState _ { asyncWrapperState = AsyncWrapper.Error "Något gick fel. Vänligen försök pånytt, eller ta kontakt med vår kundtjänst." }
