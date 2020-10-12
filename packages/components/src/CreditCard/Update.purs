module KSF.CreditCard.Update where

import Prelude

import Bottega.Models (CreditCard)
<<<<<<< HEAD
import Data.DateTime (DateTime)
import Data.Maybe (Maybe(..))
=======
import Control.Monad.Except (throwError)
import Data.Array (length)
import Data.DateTime (DateTime)
import Data.Either (Either(..), either, hush, note)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Aff as Aff
import Effect.Class (liftEffect)
>>>>>>> d0ba3982f34a43e6e1a384b9a16511b0e6f8291f
import KSF.CreditCard.Menu (menu) as Menu
import KSF.Modal as Modal
import KSF.Spinner as Spinner
import KSF.User (PaymentTerminalUrl(..))
<<<<<<< HEAD
=======
import KSF.User (getCreditCards) as User
>>>>>>> d0ba3982f34a43e6e1a384b9a16511b0e6f8291f
import React.Basic as React
import React.Basic (JSX, make)
import React.Basic.DOM as DOM
import React.Basic.DOM.Events (targetValue)
import React.Basic.Events (handler)

type Self = React.Self {} State

type State = 
<<<<<<< HEAD
  { isLoading   :: Boolean
  , updateState :: ChangeState
  , creditCards :: Array CreditCard
  }

data ChangeState
  = ChooseCreditCard
  | CreateCreditCard
  | RegisterCreditCard
  | ProcessCard
  | Failed
  | Completed

=======
  { isLoading        :: Boolean
  , updateState      :: UpdateState
  , creditCards      :: Array CreditCard
  , chosenCreditCard :: Maybe CreditCard
  }

data UpdateState
  = ChooseCreditCard
  | CreateCreditCard
  | RegisterCreditCard
  | ProcessCreditCard
  | Failed UpdateFailure
  | Completed

data UpdateFailure
  = ServerError
  | NoExistingCreditCards

>>>>>>> d0ba3982f34a43e6e1a384b9a16511b0e6f8291f
update :: {} -> JSX
update = make component { initialState, render }

initialState :: State
initialState =
  { isLoading: true
  , updateState: ChooseCreditCard
  , creditCards: []
<<<<<<< HEAD
=======
  , chosenCreditCard: Nothing
>>>>>>> d0ba3982f34a43e6e1a384b9a16511b0e6f8291f
  }

component :: React.Component {}
component = React.createComponent "update"

<<<<<<< HEAD
=======
didMount :: Self -> Effect Unit
didMount self = do
  -- Before rendering the form, we need to fetch the user's credit cards
  Aff.launchAff_ do
    Aff.finally
      -- When credit cards have been fetched, hide loading spinner
      (liftEffect $ self.setState \s -> s { isLoading = false })
      do
        creditCards <- User.getCreditCards
        liftEffect $ self.setState $ case creditCards of
          Left err    -> _ { updateState = Failed ServerError }
          Right cards -> case cards of
            []       ->  _ { updateState = Failed NoExistingCreditCards }
            [ card ] ->  _ { updateState = RegisterCreditCard
                           , chosenCreditCard = Just card 
                           }
            cards'   ->  _ { updateState = ChooseCreditCard
                           , creditCards = cards' 
                           }
          

>>>>>>> d0ba3982f34a43e6e1a384b9a16511b0e6f8291f
render :: Self -> JSX
render self = 
  if self.state.isLoading
  then Spinner.loadingSpinner
  else
    case self.state.updateState of
      ChooseCreditCard   ->   Menu.menu 
<<<<<<< HEAD
                                { creditCards: []
=======
                                { creditCards: self.state.creditCards
>>>>>>> d0ba3982f34a43e6e1a384b9a16511b0e6f8291f
                                , chosenCard: Nothing
                                }
      RegisterCreditCard -> netsTerminalIframe { paymentTerminalUrl: "https://en.wikipedia.org/wiki/Main_Page" }
      _                  -> DOM.text "WIP"

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