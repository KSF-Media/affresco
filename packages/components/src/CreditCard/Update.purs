module KSF.CreditCard.Update where

import Prelude

import Bottega.Models (CreditCard)
import Data.DateTime (DateTime)
import Data.Maybe (Maybe(..))
import KSF.CreditCard.Menu (menu) as Menu
import KSF.Modal as Modal
import KSF.Spinner as Spinner
import KSF.User (PaymentTerminalUrl(..))
import React.Basic as React
import React.Basic (JSX, make)
import React.Basic.DOM as DOM
import React.Basic.DOM.Events (targetValue)
import React.Basic.Events (handler)

type Self = React.Self Props State

type State = 
  { updateState :: ChangeState
  , creditCards :: Array CreditCard
  }

type Props =
  { creditCardId :: Int
  }

data ChangeState
  = ChooseCreditCard
  | CreateCreditCard
  | RegisterCreditCard
  | ProcessCard
  | Failed
  | Completed

update :: Props -> JSX
update = make component { initialState, render }

initialState :: State
initialState =
  { updateState: CreateCreditCard
  , creditCards: []
  }

component :: React.Component Props
component = React.createComponent "update"

render :: Self -> JSX
render self = case self.state.updateState of
  ChooseCreditCard   ->  Modal.modal 
                          { content: Menu.menu 
                                       { creditCards: []
                                       , chosenCard: Nothing
                                       }
                          }
  CreateCreditCard   -> Spinner.loadingSpinner
  RegisterCreditCard -> Modal.modal 
                          { content: netsTerminalIframe { paymentTerminalUrl: "https://en.wikipedia.org/wiki/Main_Page" }
                          }
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