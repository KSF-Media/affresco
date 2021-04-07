module KSF.JSError where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Effect.Exception (Error)

foreign import packageError       :: String -> Error
foreign import orderError         :: String -> Error
foreign import loginError         :: String -> Error
foreign import subscriptionError_ :: String -> Error
foreign import userError          :: String -> Error

data SubscriptionError
  = SubscriptionTemporaryAddressChange
  | SubscriptionPause
  | EditSubscriptionPause
  | SubscriptionReclamation

derive instance genericSubscriptionError :: Generic SubscriptionError _
instance showSubscriptionError :: Show SubscriptionError where
  show = genericShow

subscriptionError :: SubscriptionError -> String -> Error
subscriptionError subsErr errMsg = subscriptionError_ $ (show subsErr) <> ": " <> errMsg
