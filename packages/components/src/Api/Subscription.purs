module KSF.Api.Subscription where

import Prelude

import Data.Either (Either(..))
import Data.Generic.Rep (class Generic)
import Data.JSDate (JSDate)
import Data.Nullable (Nullable)
import KSF.Api.Package (Package, Campaign)
import Simple.JSON (class ReadForeign, readImpl)


type DeliveryAddress =
  { streetAddress :: Nullable String
  , zipcode       :: String
  , city          :: Nullable String
  , temporaryName :: Nullable String
  }

type PendingAddressChange =
  { address   :: DeliveryAddress
  , startDate :: JSDate
  , endDate   :: JSDate
  }

type Subscription =
  { subsno                :: Int
  , extno                 :: Int
  , cusno                 :: Int
  , paycusno              :: Int
  , kind                  :: String
  , state                 :: SubscriptionState
  , pricegroup            :: String
  , package               :: Package
  , dates                 :: SubscriptionDates
  , campaign              :: Campaign
  , paused                :: Nullable (Array PausedSubscription)
  , deliveryAddress       :: Nullable DeliveryAddress
  , pendingAddressChanges :: Nullable (Array PendingAddressChange)
  , paymentMethod         :: SubscriptionPaymentMethod
  }

newtype SubscriptionPaymentMethod = SubscriptionPaymentMethod String

type PausedSubscription =
  { startDate :: JSDate
  , endDate   :: Nullable JSDate
  }

newtype SubscriptionState = SubscriptionState String

derive instance genericSubscriptionState :: Generic SubscriptionState _
instance readForeignSubscriptionState :: ReadForeign SubscriptionState where
  readImpl f = map SubscriptionState (readImpl f)
derive instance eqSubscriptionState :: Eq SubscriptionState
instance ordSubscriptionState :: Ord SubscriptionState where
  compare =
    comparing
      \s@(SubscriptionState st) ->
        if isSubscriptionStateCanceled s
        then Right st
        else Left st

type SubscriptionDates =
  { lenMonths           :: Nullable Int
  , lenDays             :: Nullable Int
  , start               :: JSDate
  , end                 :: Nullable JSDate
  , unpaidBreak         :: Nullable JSDate
  , invoicingStart      :: Nullable JSDate
  , paidUntil           :: Nullable JSDate
  , suspend             :: Nullable JSDate
  }

isSubscriptionCanceled :: Subscription -> Boolean
isSubscriptionCanceled s = isSubscriptionStateCanceled s.state

isSubscriptionStateCanceled :: SubscriptionState -> Boolean
isSubscriptionStateCanceled (SubscriptionState "Canceled") = true
isSubscriptionStateCanceled _ = false
