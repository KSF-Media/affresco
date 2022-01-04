module Bottega.Models.CreditCard where

import Prelude

import Bottega.Models.FailReason (FailReason(..), parseFailReason)
import Bottega.Models.PaymentMethod (PaymentMethodId)
import Bottega.Models.PaymentTerminalUrl (PaymentTerminalUrl)
import Data.Argonaut.Decode (class DecodeJson, JsonDecodeError(..), decodeJson, (.!=), (.:), (.:?))
import Data.Either (note)
import Data.Maybe (Maybe (..), maybe)
import Data.UUID (UUID, parseUUID)

newtype CreditCardId = CreditCardId Int

instance decodeJsonCreditCardId :: DecodeJson CreditCardId where
  decodeJson = map CreditCardId <<< decodeJson

newtype CreditCard = CreditCard
  { id              :: CreditCardId
  , user            :: UUID
  , paymentMethodId :: PaymentMethodId
  , maskedPan       :: String
  , expiryDate      :: String
  }

instance decodeJsonCreditCard :: DecodeJson CreditCard where
  decodeJson json = do
    obj  <- decodeJson json
    id   <- obj .: "id"
    user <- note (TypeMismatch "Could not parse UUID of user!") <<< parseUUID =<< obj .: "user"
    paymentMethodId <- obj .: "paymentMethodId"
    maskedPan <- obj .: "maskedPan"
    expiryDate <- obj .: "expiryDate"
    pure $ CreditCard { id, user, paymentMethodId, maskedPan, expiryDate }

newtype CreditCardRegisterNumber = CreditCardRegisterNumber String

instance decodeJsonCreditCardRegisterNumber:: DecodeJson CreditCardRegisterNumber where
  decodeJson = map CreditCardRegisterNumber <<< decodeJson

newtype CreditCardRegister = CreditCardRegister
  { number       :: CreditCardRegisterNumber
  , user         :: UUID
  , creditCardId :: CreditCardId
  , terminalUrl  :: Maybe PaymentTerminalUrl
  , status       :: CreditCardRegisterStatus
  }

instance decodeJsonCreditCardRegister :: DecodeJson CreditCardRegister where
  decodeJson json = do
    obj  <- decodeJson json
    number <- obj .: "number"
    user <- note (TypeMismatch "Could not parse UUID of user!") <<< parseUUID =<< obj .: "user"
    creditCardId <- obj .: "creditCardId"
    terminalUrl <- obj .: "paymentTerminalUrl"
    status <- obj .: "status"
    pure $ CreditCardRegister { number, user, creditCardId, terminalUrl, status }

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

instance decodeJsonCreditCardRegisterState :: DecodeJson CreditCardRegisterState where
  decodeJson = map ((flip parseCreditCardRegisterState) Nothing) <<< decodeJson


parseCreditCardRegisterState :: String -> Maybe String -> CreditCardRegisterState
parseCreditCardRegisterState state maybeFailReason =
    case state of
      "created"   -> CreditCardRegisterCreated
      "started"   -> CreditCardRegisterStarted
      "completed" -> CreditCardRegisterCompleted
      "failed"    -> CreditCardRegisterFailed $ maybe UnknownReason parseFailReason maybeFailReason
      "canceled"  -> CreditCardRegisterCanceled
      _           -> CreditCardRegisterUnknownState
