module Bottega.Models.CreditCard where

import Prelude

import Bottega.Models.FailReason (FailReason(..), parseFailReason)
import Bottega.Models.PaymentMethod (PaymentMethodId)
import Bottega.Models.PaymentTerminalUrl (PaymentTerminalUrl)
import Data.Maybe (Maybe, maybe)
import Data.UUID (UUID)

newtype CreditCardId = CreditCardId Int

type CreditCard =
  { id              :: CreditCardId
  , user            :: UUID
  , paymentMethodId :: PaymentMethodId
  , maskedPan       :: String
  , expiryDate      :: String
  }

newtype CreditCardRegisterNumber = CreditCardRegisterNumber String

type CreditCardRegister =
  { number       :: CreditCardRegisterNumber
  , user         :: UUID
  , creditCardId :: CreditCardId
  , terminalUrl  :: Maybe PaymentTerminalUrl
  , status       :: CreditCardRegisterStatus
  }

type CreditCardRegisterStatus =
  { state      :: CreditCardRegisterState
  , time       :: String
  }

data CreditCardRegisterState
  = CreditCardRegisterCreated
  | CreditCardRegisterStarted
  | CreditCardRegisterCompleted
  | CreditCardRegisterFailed FailReason
  | CreditCardRegisterCanceled
  | CreditCardRegisterUnknownState

parseCreditCardRegisterState :: String -> Maybe String -> CreditCardRegisterState
parseCreditCardRegisterState state maybeFailReason =
    case state of
      "created"   -> CreditCardRegisterCreated
      "started"   -> CreditCardRegisterStarted
      "completed" -> CreditCardRegisterCompleted
      "failed"    -> CreditCardRegisterFailed $ maybe UnknownReason parseFailReason maybeFailReason
      "canceled"  -> CreditCardRegisterCanceled
      _           -> CreditCardRegisterUnknownState
