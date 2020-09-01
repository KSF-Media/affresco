module KSF.CreditCardChange where

import Prelude

import Data.DateTime (DateTime)
import KSF.InputField.Component (inputLabel)
import React.Basic as React
import React.Basic (JSX)
import React.Basic.DOM as DOM
import React.Basic.DOM.Events (targetValue)
import React.Basic.Events (handler)

-- type Self = React.Self Props State

type State = 
  { creditCard :: CreditCard
  }

type Props =
  { creditCardId :: Int
  }

type CreditCard =
  { id          :: Int
  , panHash     :: String
  , maskedPan   :: String
  , expiryDate  :: DateTime
  }