module MittKonto.Main.CreditCardUpdateView where

import Prelude

import Bottega (BottegaError, bottegaErrorMessage)
import Bottega.Models (CreditCard, CreditCardRegister, CreditCardRegisterState(..))
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Aff as Aff
import Effect.Class (liftEffect)
import Effect.Exception (error)
import KSF.AsyncWrapper as AsyncWrapper
import KSF.CreditCard.Choice (choice) as Choice
import KSF.CreditCard.Register (register) as Register
import KSF.Sentry as Sentry
import KSF.User (PaymentTerminalUrl)
import KSF.User (getCreditCardRegister, registerCreditCard, updateCreditCardSubscriptions) as User
import MittKonto.Wrappers (class ViewWrapperContent, AutoClose(..), ViewWrapperState, SetViewWrapperState, instantiate)
import MittKonto.Wrappers.Elements as WrapperElements
import React.Basic (JSX)
import React.Basic.Classic (element, make)
import React.Basic.Classic as React
import React.Basic.DOM as DOM
import React.Basic.Router as Router
import Record as Record

type BaseProps =
  ( creditCards :: Array CreditCard
  , logger      :: Sentry.Logger
  )

type Inputs = Record BaseProps

newtype ViewWrapperContentInputs = ViewWrapperContentInputs Inputs

type Props =
  { setWrapperState :: SetViewWrapperState
  | BaseProps
  }

type Self = React.Self Props State

type State =
  { asyncWrapperState :: AsyncWrapper.Progress JSX
  , updateState       :: UpdateState
  , poller            :: Aff.Fiber Unit
  }

type SetState = (State -> State) -> Effect Unit

data UpdateState
  = ChooseCreditCard
  | RegisterCreditCard PaymentTerminalUrl
  | Cancel

creditCardUpdateView :: Props -> JSX
creditCardUpdateView = make component { initialState, render, didMount }

instance viewWrapperContentCardUpdate :: ViewWrapperContent ViewWrapperContentInputs where
  instantiate (ViewWrapperContentInputs inputs) setWrapperState = do
    renderedContent <- pure $ creditCardUpdateView $ Record.merge inputs { setWrapperState }
    setWrapperState \s -> s { renderedContent = renderedContent }

initialState :: State
initialState =
  { asyncWrapperState: AsyncWrapper.Ready
  , poller: pure unit
  , updateState: ChooseCreditCard
  }

component :: React.Component Props
component = React.createComponent "CreditCardUpdateView"

didMount :: Self -> Effect Unit
didMount self@{ state, setState, props: { creditCards, logger, setWrapperState } } = do
  setWrapperState _ { titleText = "Uppdatera ditt kredit- eller bankkort", onCancel = onCancel self }
  Aff.launchAff_ do
    case creditCards of
      []       -> liftEffect $ do
        logger.log "No credit cards found" Sentry.Error
        onError self
      [ card ] -> do
        registerCreditCard self card
      _        -> pure unit

render :: Self -> JSX
render self@{ setState, state: { updateState }, props: { creditCards } } =
  asyncWrapper $ DOM.div
    { className: "clearfix credit-card-update--container"
    , children:
        [ case updateState of
            ChooseCreditCard       -> Choice.choice
                                        { creditCards: creditCards
                                        , onSubmit: \creditCard -> Aff.launchAff_ $ registerCreditCard self creditCard
                                        , onCancel: onCancel self
                                        }

            RegisterCreditCard url -> Register.register
                                        { terminalUrl: url
                                        }

            Cancel                 -> Router.redirect
                                        { to: { pathname: "/"
                                              , state: {}
                                              }
                                        , from: "/kreditkortt/uppdatera"
                                        , push: true
                                        }
        ]
    }
  where
    asyncWrapper :: JSX -> JSX
    asyncWrapper content =
      AsyncWrapper.asyncWrapper
        { wrapperState: AsyncWrapper.Ready
        , readyView: content
        , editingView: identity
        , successView: \msg -> WrapperElements.successWrapper msg
        , errorView: \err -> WrapperElements.errorWrapper onTryAgain err
        , loadingView: identity
        }
    onTryAgain :: Effect Unit
    onTryAgain = setState \s -> s { asyncWrapperState = AsyncWrapper.Ready }

registerCreditCard :: Self -> CreditCard -> Aff Unit
registerCreditCard self@{ setState, props: { logger, setWrapperState }, state } oldCreditCard = do
  creditCardRegister <- User.registerCreditCard
  case creditCardRegister of
    Right register@{ terminalUrl: Just url } -> do
      let newState = state { updateState = RegisterCreditCard url }
      liftEffect $ do
        setState \_ -> newState
        setWrapperState _ { closeable = false }
      void $ Aff.forkAff $ startRegisterPoller self { state = newState } oldCreditCard register
    Right register@{ terminalUrl: Nothing } ->
      liftEffect $ do
        logger.log "No terminal url received" Sentry.Error
        onError self
    Left err ->
      liftEffect $ do
        logger.log ("Got the following error when registering credit card: " <> bottegaErrorMessage err) Sentry.Error
        onError self

killRegisterPoller :: State -> Aff Unit
killRegisterPoller state = Aff.killFiber (error "Canceled poller") state.poller

startRegisterPoller :: Self -> CreditCard -> CreditCardRegister -> Aff Unit
startRegisterPoller self@{ setState, state } oldCreditCard creditCardRegister = do
  newPoller <- Aff.forkAff do
    killRegisterPoller state
    newPoller <- Aff.forkAff $ pollRegister self oldCreditCard (Right creditCardRegister)
    Aff.joinFiber newPoller
  liftEffect $ setState _ { poller = newPoller }

pollRegister :: Self -> CreditCard -> Either BottegaError CreditCardRegister -> Aff Unit
pollRegister self@{ setState, props: { logger }, state } oldCreditCard (Right register) = do
  Aff.delay $ Aff.Milliseconds 1000.0
  case register.status.state of
    CreditCardRegisterStarted ->
      pollRegister self oldCreditCard =<< User.getCreditCardRegister register.creditCardId register.number
    CreditCardRegisterCompleted -> do
      result <- User.updateCreditCardSubscriptions oldCreditCard.id register.creditCardId
      liftEffect $ case result of
        Left err -> do
          let errMsg = bottegaErrorMessage err
          logger.log
            ("Server encountered the following error while trying to update credit card's subscriptions: " <> errMsg)
            Sentry.Error
          onError self
        Right _  -> onSuccess self
    CreditCardRegisterFailed _ -> liftEffect $ onError self
    CreditCardRegisterCanceled -> liftEffect $ do
      onCancel self
      setState \_ -> self.state { updateState = Cancel }
    CreditCardRegisterCreated -> pollRegister self oldCreditCard =<< User.getCreditCardRegister register.creditCardId register.number
    CreditCardRegisterUnknownState -> liftEffect $ do
      logger.log "Server is in an unknown state" Sentry.Info
      onError self
pollRegister self@{ props: { logger } } _ (Left err) = liftEffect $ do
  logger.log ("Could not fetch register status: " <> bottegaErrorMessage err) Sentry.Error
  onError self

onCancel :: Self -> Effect Unit
onCancel self@{ setState } = setState _ { asyncWrapperState = AsyncWrapper.Ready }

onLoading :: Self -> Effect Unit
onLoading self@{ setState } = setState _ { asyncWrapperState = AsyncWrapper.Loading mempty }

onSuccess :: Self -> Effect Unit
onSuccess self@{ setState, props: { setWrapperState } } = do
  setState _ { asyncWrapperState = AsyncWrapper.Success $ Just "Uppdateringen av betalningsinformationen lyckades." }
  setWrapperState _ { closeAutomatically = On 3000.0 }

onError :: Self -> Effect Unit
onError self@{ setState } = setState _ { asyncWrapperState = AsyncWrapper.Error "Något gick fel. Vänligen försök pånytt, eller ta kontakt med vår kundtjänst." }