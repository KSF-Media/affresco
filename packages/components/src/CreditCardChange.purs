module KSF.CreditCardChange where

import Prelude

import Data.DateTime (DateTime)
import Data.Maybe (Maybe(..))
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
  { changeState :: ChangeState
  , creditCard  :: Maybe CreditCard
  }

type Props =
  { creditCardId :: Int
  }

data ChangeState
  = CreateCreditCard
  | RegisterCreditCard
  | ProcessCard
  | Failed
  | Completed

type CreditCard =
  { id          :: Int
  , panHash     :: String
  , maskedPan   :: String
  , expiryDate  :: DateTime
  }

creditCardChange :: Props -> JSX
creditCardChange = make component { initialState, render}

initialState :: State
initialState =
  { changeState: CreateCreditCard
  , creditCard: Nothing
  }

component :: React.Component Props
component = React.createComponent "CreditCardChange"

render :: Self -> JSX
render self = case self.state.changeState of
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