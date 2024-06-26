module MittKonto.Main.CreditCardUpdateView where

import Prelude

import Bottega (BottegaError, bottegaErrorMessage)
import Bottega.Models (CreditCard, CreditCardRegister, CreditCardRegisterNumber(..), CreditCardRegisterState(..), FailReason(..))
import Data.Either (Either(..))
import Data.Foldable (for_)
import Data.Maybe (Maybe(..), fromMaybe, isNothing)
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Aff as Aff
import Effect.Aff.AVar (tryRead) as AVar
import Effect.AVar (AVar)
import Effect.AVar (empty, tryPut, tryTake) as AVar
import Effect.Class (liftEffect)
import Effect.Exception (error)
import KSF.Api.Subscription (Subsno(..))
import KSF.AsyncWrapper as AsyncWrapper
import KSF.CreditCard.Register (render, scaRequired) as Register
import KSF.Sentry as Sentry
import KSF.User (getCreditCardRegister, registerCreditCardFromExisting) as User
import KSF.User.Cusno (Cusno)
import KSF.Tracking as Tracking
import KSF.Window (close)
import MittKonto.Wrappers (AutoClose(..), SetRouteWrapperState)
import MittKonto.Wrappers.Elements as WrapperElements
import React.Basic (JSX)
import React.Basic.Hooks (Component, component, useState, useEffectOnce, (/\))
import React.Basic.Hooks as React
import React.Basic.DOM as DOM
import Web.HTML as Web.HTML
import Web.HTML.Location as Web.HTML.Location
import Web.HTML.Window as Window
import Web.HTML.Window (Window)

type BaseProps =
  ( cusno       :: Cusno
  , logger      :: Sentry.Logger
  , window      :: Maybe Window
  , creditCard  :: Either BottegaError CreditCard
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

creditCardUpdateView :: Component Props
creditCardUpdateView = do
  closed <- AVar.empty
  component "CreditCardUpdateView" \props@{ creditCard } -> React.do
    state /\ setState <- useState initialState

    let self = { state, setState, props}
    useEffectOnce do
      props.setWrapperState \s -> s { titleText = "Uppdatera ditt kredit- eller bankkort" }
      case creditCard of
        Left err -> do
          onError self $ "1, " <> bottegaErrorMessage err
          pure mempty
        Right card -> do
          _ <- AVar.tryTake closed
          Aff.launchAff_ $ registerCreditCard self closed card
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
        , editingView: identity
        , successView: fromMaybe mempty
        , errorView: \err -> WrapperElements.errorWrapper onTryAgain err
        , loadingView: identity
        }
    onTryAgain :: Effect Unit
    onTryAgain = setState \s -> s { asyncWrapperState = AsyncWrapper.Ready }

registerCreditCard :: Self -> AVar Unit -> CreditCard -> Aff Unit
registerCreditCard self@{ setState, props: { logger, setWrapperState, window }, state } closed oldCreditCard@{ id } = do
  globalWindow <- liftEffect Web.HTML.window
  creditCardRegister <- User.registerCreditCardFromExisting id
  case creditCardRegister of
    Right register@{ terminalUrl: Just url } -> do
      let newState = state { updateState = RegisterCreditCard, paymentTerminal = Just url.paymentTerminalUrl }
      liftEffect do
        case window of
          Just w -> do
            l <- Window.location w
            Web.HTML.Location.setHref url.paymentTerminalUrl l
          Nothing -> do
            void $ Window.open url.paymentTerminalUrl "_blank" "noopener" globalWindow
        setState \_ -> newState
        setWrapperState _ { closeable = true }
      void $ Aff.forkAff $ startRegisterPoller self { state = newState } closed oldCreditCard register
    Right { terminalUrl: Nothing } -> do
      liftEffect $ for_ window close
      liftEffect do
        logger.log "No terminal url received" Sentry.Error
        onError self "2"
    Left err -> do
      liftEffect $ for_ window close
      liftEffect do
        let msg = bottegaErrorMessage err
        -- Sentry is flooded, may or may not show up
        logger.log ("Got the following error when registering credit card: " <> msg) Sentry.Error
        onError self $ "3, " <> msg

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
pollRegister self@{ props: { cusno, logger } } closed oldCreditCard (Right register) = do
  case register.status.state of
    CreditCardRegisterStarted ->
      delayedPollRegister =<< User.getCreditCardRegister register.creditCardId register.number
    CreditCardRegisterCompleted -> liftEffect do
      track "success"
      onSuccess self
    CreditCardRegisterFailed reason -> liftEffect do
      track $ "error:" <> show reason
      case reason of
        NetsIssuerError -> self.setState _ { asyncWrapperState = AsyncWrapper.Error "Betalning nekades av kortutgivaren. Vänligen kontakta din bank." }
        _ -> onError self "4"
    CreditCardRegisterScaRequired -> liftEffect do
      case self.state.scaShown /\ self.state.paymentTerminal of
        false /\ Just url -> do
          self.setState _ { updateState = ScaRequired url, scaShown = true }
        _ -> pure unit
    CreditCardRegisterCanceled -> liftEffect do
      track "cancel"
      onCancel self
    CreditCardRegisterCreated -> delayedPollRegister =<< User.getCreditCardRegister register.creditCardId register.number
    CreditCardRegisterUnknownState -> liftEffect do
      track $ "error: unknown"
      logger.log "Server is in an unknown state" Sentry.Info
      onError self "5"
  where
    delayedPollRegister :: Either BottegaError CreditCardRegister -> Aff Unit
    delayedPollRegister eitherRegister = do
      componentOpen <- isNothing <$> AVar.tryRead closed
      when componentOpen do
        Aff.delay $ Aff.Milliseconds 1000.0
        pollRegister self closed oldCreditCard eitherRegister

    track :: String -> Effect Unit
    track result = do
      -- Do we even have this anymore?
      Tracking.updateCreditCard cusno (Subsno 0) (Tracking.readBottegaCreditCard oldCreditCard) (unRegisterNumber register.number) result

    unRegisterNumber :: CreditCardRegisterNumber -> String
    unRegisterNumber (CreditCardRegisterNumber number) = number

pollRegister self@{ props: { logger } } _ _ (Left err) = liftEffect do
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
                    }
  cardsChanged

onError :: Self -> String -> Effect Unit
onError { setState } errMsg = setState _ { asyncWrapperState = AsyncWrapper.Error $ "Något gick fel. Vänligen försök pånytt, eller ta kontakt med vår kundtjänst. Felkod: " <> show errMsg }
