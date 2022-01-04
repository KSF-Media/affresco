module Bottega.Models.PaymentTerminalUrl where

import Prelude
import Data.Argonaut.Decode (class DecodeJson, decodeJson, (.:))

newtype PaymentTerminalUrl = PaymentTerminalUrl String

instance decodeJsonPaymentTerminalUrl :: DecodeJson PaymentTerminalUrl where
  decodeJson json = do
    obj <- decodeJson json
    paymentTerminalUrl <- obj .: "paymentTerminalUrl"
    pure $ PaymentTerminalUrl paymentTerminalUrl
