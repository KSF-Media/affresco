module KSF.CreditCard.Update where

import Prelude

import Bottega.Models (CreditCard, CreditCardRegister, CreditCardRegisterState (..), FailReason(..))
import Control.Monad.Except (throwError)
import Data.Array (length, head) as Array
import Data.DateTime (DateTime)
import Data.Either (Either(..), either, hush, note)
import Data.Maybe (Maybe(..), fromMaybe, isJust, maybe)
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Aff as Aff
import Effect.Class (liftEffect)
import Effect.Exception (Error, error, message)
import KSF.CreditCard.Menu (menu) as Menu
import KSF.JSError as Error
import KSF.Modal as Modal
import KSF.Spinner as Spinner
import KSF.User (PaymentTerminalUrl(..))
import KSF.User (getCreditCards, registerCreditCard, getCreditCardRegister, updateCreditCardSubscriptions) as User
import React.Basic as React
import React.Basic (JSX, make)
import React.Basic.DOM as DOM
import React.Basic.DOM.Events (targetValue)
import React.Basic.Events (handler)

type Props = 
  { onCancel  :: Effect Unit
  , onLoading :: Effect Unit
  , onSuccess :: Effect Unit
  , onError   :: Effect Unit
  }

type Self = React.Self Props State

type State = 
  { updateState      :: UpdateState
  , poller           :: Aff.Fiber Unit
  , creditCards      :: Array CreditCard
  , chosenCreditCard :: Maybe CreditCard
  }

type SetState = (State -> State) -> Effect Unit

data UpdateState
  = ChooseCreditCard
  | RegisterCreditCard PaymentTerminalUrl

data UpdateFailure
  = ServerError
  | NoExistingCreditCards
  | UnexpectedError String

update :: Props -> JSX
update = make component { initialState, render, didMount }

initialState :: State
initialState =
  { poller: pure unit
  , updateState: ChooseCreditCard
  , creditCards: []
  , chosenCreditCard: Nothing
  }

component :: React.Component Props
component = React.createComponent "update"

didMount :: Self -> Effect Unit
didMount self = do
  -- Before rendering the form, we need to fetch the user's credit cards
  Aff.launchAff_ do
    creditCards <- User.getCreditCards
    case creditCards of
      Left err    -> liftEffect self.props.onError
      Right cards -> case Array.head cards of
        Nothing   -> liftEffect self.props.onError
        Just card -> do
          let newState = self.state { chosenCreditCard = Just card }
          liftEffect $ self.setState \_ -> newState
          registerCreditCard self.setState self.props newState


render :: Self -> JSX
render self = 
  case self.state.updateState of
    ChooseCreditCard       -> Menu.menu 
                                { creditCards: self.state.creditCards
                                , chosenCard: Nothing
                                }
    RegisterCreditCard url -> netsTerminalIframe url
  where
    netsTerminalIframe :: PaymentTerminalUrl -> JSX
    netsTerminalIframe { paymentTerminalUrl } =
      DOM.div
        { className: "credit-card-update--register-wrapper"
        , children:
          [ DOM.iframe
            { src: paymentTerminalUrl
            , className: "credit-card-update--register-terminal"
            }
          ]
        }

registerCreditCard :: SetState -> Props -> State -> Aff Unit
registerCreditCard setState props state = do
  creditCardRegister <- startRegister
  case creditCardRegister of
    Right register@{ terminalUrl: Just url } -> do
      let newState = state { updateState = RegisterCreditCard url }
      liftEffect $ setState \_ -> newState
      void $ Aff.forkAff $ startRegisterPoller setState props newState register
    Left err ->
      case err of
        UnexpectedError e -> pure unit
        _ -> pure unit
    _ -> pure unit
  where
    startRegister :: Aff (Either UpdateFailure CreditCardRegister)
    startRegister =
      User.registerCreditCard >>= \eitherRegister ->
        pure $ case eitherRegister of
          Right register@{ terminalUrl: Just url } -> Right register
          Right _                                  -> Left $ UnexpectedError "No url"
          Left  err                                -> Left $ UnexpectedError err

killRegisterPoller :: State -> Aff Unit
killRegisterPoller state = Aff.killFiber (error "Canceled poller") state.poller

startRegisterPoller :: SetState -> Props -> State -> CreditCardRegister -> Aff Unit
startRegisterPoller setState props state creditCardRegister = do
  newPoller <- Aff.forkAff do
    killRegisterPoller state
    newPoller <- Aff.forkAff $ pollRegister props state (Right creditCardRegister)
    Aff.joinFiber newPoller
  liftEffect $ setState _ { poller = newPoller }

pollRegister :: Props -> State -> Either String CreditCardRegister -> Aff Unit
pollRegister props state (Right register) = do
  Aff.delay $ Aff.Milliseconds 1000.0
  case register.status.state of
    CreditCardRegisterStarted ->
      pollRegister props state =<< User.getCreditCardRegister register.creditCardId register.number
    CreditCardRegisterCompleted -> do
      case state.chosenCreditCard of
        Just card -> do
          result <- User.updateCreditCardSubscriptions card.id register.creditCardId
          liftEffect $ case result of
            Left err -> props.onError
            Right _  -> props.onSuccess
        Nothing -> liftEffect props.onError
    CreditCardRegisterFailed reason -> liftEffect props.onError
    CreditCardRegisterCanceled -> liftEffect props.onError
    CreditCardRegisterCreated -> pollRegister props state =<< User.getCreditCardRegister register.creditCardId register.number
    CreditCardRegisterUnknownState -> liftEffect props.onError
pollRegister props state (Left err) = liftEffect props.onError