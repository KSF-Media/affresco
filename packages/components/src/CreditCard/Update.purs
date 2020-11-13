module KSF.CreditCard.Update where

import Prelude

import Bottega.Models (CreditCard, CreditCardRegister, CreditCardRegisterState(..))
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Aff as Aff
import Effect.Class (liftEffect)
import Effect.Exception (error)
import KSF.CreditCard.Choice (choice) as Choice
import KSF.Grid as Grid
import KSF.Sentry as Sentry
import KSF.User (PaymentTerminalUrl)
import KSF.User (getCreditCardRegister, registerCreditCard, updateCreditCardSubscriptions) as User
import React.Basic as React
import React.Basic (JSX, make)
import React.Basic.DOM as DOM
import React.Basic.Events (handler_)

type Props = 
  { creditCards :: Array CreditCard
  , logger      :: Sentry.Logger
  , onCancel    :: Effect Unit
  , onLoading   :: Effect Unit
  , onSuccess   :: Effect Unit
  , onError     :: Effect Unit
  }

type Self = React.Self Props State

type State = 
  { updateState      :: UpdateState
  , poller           :: Aff.Fiber Unit
  }

type SetState = (State -> State) -> Effect Unit

data UpdateState
  = ChooseCreditCard
  | RegisterCreditCard PaymentTerminalUrl

update :: Props -> JSX
update = make component { initialState, render, didMount }

initialState :: State
initialState =
  { poller: pure unit
  , updateState: ChooseCreditCard
  }

component :: React.Component Props
component = React.createComponent "update"

didMount :: Self -> Effect Unit
didMount self@{ state, setState, props: { creditCards, onError, logger } } =
  Aff.launchAff_ do
    case creditCards of
      []       -> liftEffect $ do 
        logger.log "No credit cards found" Sentry.Error
        onError
      [ card ] -> do
        registerCreditCard setState self.props state card
      _        -> pure unit

render :: Self -> JSX
render self@{ setState, state: { updateState }, props: { creditCards, onCancel } } = 
  DOM.div
    { className: "clearfix credit-card-update--container"
    , children:
        case updateState of
            ChooseCreditCard -> [ Choice.choice 
                                    { creditCards: creditCards
                                    , title: title
                                    , onSubmit: \creditCard -> Aff.launchAff_ $ registerCreditCard setState self.props self.state creditCard   
                                    , onCancel: onCancel     
                                    } 
                                ]
            RegisterCreditCard url -> [ title
                                      , netsTerminalIframe url
                                      ]
    }         
  where
    title :: JSX 
    title = DOM.h3_ [ DOM.text "Uppdatera ditt kredit- eller bankkort" ]

    netsTerminalIframe :: PaymentTerminalUrl -> JSX
    netsTerminalIframe { paymentTerminalUrl } =
      DOM.div 
        { className: "credit-card-update--register-wrapper"
        , children : [ DOM.iframe
                         { src: paymentTerminalUrl
                         , className: "credit-card-update--register-terminal"
                         }
                     ]
        }

registerCreditCard :: SetState -> Props -> State -> CreditCard -> Aff Unit
registerCreditCard setState props@{ logger, onError } state oldCreditCard = do
  creditCardRegister <- User.registerCreditCard
  case creditCardRegister of
    Right register@{ terminalUrl: Just url } -> do
      let newState = state { updateState = RegisterCreditCard url }
      liftEffect $ setState \_ -> newState
      void $ Aff.forkAff $ startRegisterPoller setState props newState oldCreditCard register
    Right register@{ terminalUrl: Nothing } -> 
      liftEffect $ do 
        logger.log "No terminal url received" Sentry.Error
        onError
    Left err ->
      liftEffect $ do
        logger.log ("Got the following error when registering credit card: " <> err) Sentry.Error
        onError

killRegisterPoller :: State -> Aff Unit
killRegisterPoller state = Aff.killFiber (error "Canceled poller") state.poller

startRegisterPoller :: SetState -> Props -> State -> CreditCard -> CreditCardRegister -> Aff Unit
startRegisterPoller setState props state oldCreditCard creditCardRegister = do
  newPoller <- Aff.forkAff do
    killRegisterPoller state
    newPoller <- Aff.forkAff $ pollRegister props oldCreditCard (Right creditCardRegister)
    Aff.joinFiber newPoller
  liftEffect $ setState _ { poller = newPoller }

pollRegister :: Props -> CreditCard -> Either String CreditCardRegister -> Aff Unit
pollRegister props@{ logger, onError, onSuccess, onCancel } oldCreditCard (Right register) = do
  Aff.delay $ Aff.Milliseconds 1000.0
  case register.status.state of
    CreditCardRegisterStarted ->
      pollRegister props oldCreditCard =<< User.getCreditCardRegister register.creditCardId register.number
    CreditCardRegisterCompleted -> do
      result <- User.updateCreditCardSubscriptions oldCreditCard.id register.creditCardId
      liftEffect $ case result of
        Left err -> do 
          logger.log ("Server encountered the following error while trying to update credit card's subscriptions: " <> err) Sentry.Error
          onError
        Right _  -> onSuccess
    CreditCardRegisterFailed _ -> liftEffect onError
    CreditCardRegisterCanceled -> liftEffect onCancel
    CreditCardRegisterCreated -> pollRegister props oldCreditCard =<< User.getCreditCardRegister register.creditCardId register.number
    CreditCardRegisterUnknownState -> liftEffect $ do
      logger.log "Server is in an unknown state" Sentry.Info
      onError
pollRegister props state (Left err) = liftEffect $ do 
  props.logger.log ("Could not fetch register status: " <> err) Sentry.Error
  props.onError