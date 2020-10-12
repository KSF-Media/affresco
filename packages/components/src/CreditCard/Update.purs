module KSF.CreditCard.Update where

import Prelude

import Bottega.Models (CreditCard)
import Control.Monad.Except (throwError)
import Data.Array (length)
import Data.DateTime (DateTime)
import Data.Either (Either(..), either, hush, note)
import Data.Maybe (Maybe(..), fromMaybe, isJust, maybe)
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Aff as Aff
import Effect.Class (liftEffect)
import KSF.CreditCard.Menu (menu) as Menu
import KSF.Modal as Modal
import KSF.Spinner as Spinner
import KSF.User (PaymentTerminalUrl(..))
import KSF.User (getCreditCards, registerCreditCard) as User
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
  , creditCards      :: Array CreditCard
  , chosenCreditCard :: Maybe CreditCard
  }

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
update = make component { initialState, render }

initialState :: State
initialState =
  { isLoading: true
  , loadingMessage: Nothing
  , updateState: ChooseCreditCard
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
      (liftEffect $ self.setState \s -> s { isLoading = false })
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
            cards'   -> self.setState _ { updateState = ChooseCreditCard
                                        , creditCards = cards' 
                                        }
          

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
  Aff.launchAff_ $ Spinner.withSpinner loadingWithMessage do
    paymentTerminalUrl <- register
    case paymentTerminalUrl of
      Right url -> liftEffect $ self.setState _ { updateState = RegisterCreditCard url
                                                }
      Left err ->
        case err of
          UnexpectedError e -> pure unit
          _ -> pure unit
  where
    loadingWithMessage spinner = self.setState _
        { isLoading = true
        , loadingMessage =
            if isJust spinner
            then Just "Tack, vi skickar dig nu vidare till betalningsleverantören Nets."
            else Nothing
        }

    register :: Aff (Either UpdateFailure PaymentTerminalUrl)
    register =
      User.registerCreditCard >>= \eitherRegister ->
        pure $ case eitherRegister of
          Right register@{ terminalUrl: Just url } -> Right url
          Right _                                  -> Left $ UnexpectedError "No url"
          Left  err                                -> Left $ UnexpectedError err