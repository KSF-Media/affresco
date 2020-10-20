module KSF.CreditCard.Update where

import Prelude

import Bottega.Models (CreditCard, CreditCardRegister, CreditCardRegisterState (..), FailReason(..))
import Control.Monad.Except (throwError)
import Data.Array (length)
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

type Self = React.Self {} State

type State = 
  { isLoading        :: Boolean
  , loadingMessage   :: Maybe String
  , updateState      :: UpdateState
  , poller           :: Aff.Fiber Unit
  , creditCards      :: Array CreditCard
  , chosenCreditCard :: Maybe CreditCard
  }

type SetState = (State -> State) -> Effect Unit

data UpdateState
  = ChooseCreditCard
  | NewCreditCardUpdate
  | RegisterCreditCard PaymentTerminalUrl
  | ProcessCreditCard
  | Failed UpdateFailure
  | Completed

data UpdateFailure
  = ServerError
  | NoExistingCreditCards
  | UnexpectedError String

update :: {} -> JSX
update = make component { initialState, render, didMount }

initialState :: State
initialState =
  { isLoading: true
  , poller: pure unit
  , loadingMessage: Nothing
  , updateState: NewCreditCardUpdate
  , creditCards: []
  , chosenCreditCard: Nothing
  }

component :: React.Component {}
component = React.createComponent "update"

didMount :: Self -> Effect Unit
didMount self = do
  -- Before rendering the form, we need to fetch the user's credit cards
  Aff.launchAff_ do
    Aff.finally
      -- When credit cards have been fetched, hide loading spinner
      (liftEffect $ do 
        self.setState \s -> s { isLoading = false }
      )
      do
        creditCards <- User.getCreditCards
        liftEffect $ case creditCards of
          Left err    -> self.setState _ { updateState = Failed ServerError }
          Right cards -> case cards of
            []       -> self.setState _ { updateState = Failed NoExistingCreditCards }
            [ card ] -> do
              self.setState _ { updateState = NewCreditCardUpdate
                              , chosenCreditCard = Just card 
                              }
              registerCreditCard self
            cards'   -> self.setState _ { updateState = Failed $ UnexpectedError "" }
          

render :: Self -> JSX
render self = 
  if self.state.isLoading
  then Spinner.loadingSpinner
  else
    case self.state.updateState of
      NewCreditCardUpdate    -> Spinner.loadingSpinner
      ChooseCreditCard       -> Menu.menu 
                                  { creditCards: self.state.creditCards
                                  , chosenCard: Nothing
                                  }
      RegisterCreditCard url -> netsTerminalIframe url
      Failed _               -> DOM.text "FAILED"
      Completed              -> DOM.text "SUCCESS"
      _                      -> DOM.text "WIP"

netsTerminalIframe :: PaymentTerminalUrl -> JSX
netsTerminalIframe { paymentTerminalUrl } =
  DOM.div
    { className: "credit-card-change--register-wrapper"
    , children:
      [ DOM.iframe
        { src: paymentTerminalUrl
        , className: "credit-card-change--register-terminal"
        }
      ]
    }

registerCreditCard :: Self -> Effect Unit
registerCreditCard self = do
  Aff.launchAff_ $ do
    creditCardRegister <- startRegister
    case creditCardRegister of
      Right register@{ terminalUrl: Just url } -> liftEffect $ do 
        let newState = self.state { updateState = RegisterCreditCard url }
        self.setState \_ -> newState
        startRegisterPoller self.setState newState register
      Left err ->
        case err of
          UnexpectedError e -> pure unit
          _ -> pure unit
      _ -> pure unit
  where
    loadingWithMessage spinner = self.setState _
        { isLoading = true
        , loadingMessage =
            if isJust spinner
            then Just "Tack, vi skickar dig nu vidare till betalningsleverantören Nets."
            else Nothing
        }

    startRegister :: Aff (Either UpdateFailure CreditCardRegister)
    startRegister =
      User.registerCreditCard >>= \eitherRegister ->
        pure $ case eitherRegister of
          Right register@{ terminalUrl: Just url } -> Right register
          Right _                                  -> Left $ UnexpectedError "No url"
          Left  err                                -> Left $ UnexpectedError err

killRegisterPoller :: State -> Aff Unit
killRegisterPoller state = Aff.killFiber (error "Canceled poller") state.poller

startRegisterPoller :: SetState -> State -> CreditCardRegister -> Effect Unit
startRegisterPoller setState state creditCardRegister = do
  newPoller <- Aff.launchAff do
        killRegisterPoller state
        newPoller <- Aff.forkAff $ pollRegister setState state (Right creditCardRegister)
        Aff.joinFiber newPoller
  setState _ { poller = newPoller }

pollRegister :: SetState -> State -> Either String CreditCardRegister -> Aff Unit
pollRegister setState state (Right register) = do
  Aff.delay $ Aff.Milliseconds 1000.0
  case register.status.state of
    CreditCardRegisterStarted -> do
      liftEffect $ setState _ { updateState = ProcessCreditCard }
      pollRegister setState state =<< User.getCreditCardRegister register.creditCardId register.number
    CreditCardRegisterCompleted -> do
      case state.chosenCreditCard of
        Just card -> do
          result <- User.updateCreditCardSubscriptions card.id register.creditCardId
          liftEffect $ setState $ case result of
            Left err -> _ { updateState = Failed $ UnexpectedError "" }
            Right _  -> _ { updateState = Completed }
        Nothing -> liftEffect $ setState _ { updateState = Failed $ UnexpectedError "" }
    CreditCardRegisterFailed reason -> liftEffect $ setState _ { updateState = Failed $ UnexpectedError "" }
    CreditCardRegisterCanceled -> liftEffect do
      setState _ { updateState = NewCreditCardUpdate }
    CreditCardRegisterCreated -> pollRegister setState state =<< User.getCreditCardRegister register.creditCardId register.number
    CreditCardRegisterUnknownState -> liftEffect do
      setState _ { updateState = Failed ServerError }
pollRegister setState state (Left err) = liftEffect do
  setState _ { updateState = Failed ServerError }