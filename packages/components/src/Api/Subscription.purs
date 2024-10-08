module KSF.Api.Subscription where

import Prelude

import Bottega.Models.PaymentMethod (PaymentMethodId)
import Control.Alt ((<|>))
import Data.Date (Date)
import Data.Either (Either(..))
import Data.Foldable (elem)
import Data.Generic.Rep (class Generic)
import Data.Int as Int
import Data.JSDate (JSDate, toDate)
import Data.Maybe (Maybe, maybe)
import Data.Newtype (class Newtype)
import Data.Nullable (Nullable, toMaybe)
import Foreign (Foreign)
import Foreign.Generic.EnumEncoding (defaultGenericEnumOptions, genericDecodeEnum)
import KSF.Api.Package (Package, Campaign)
import KSF.User.Cusno (Cusno)
import Simple.JSON (class ReadForeign, readImpl)
import Simple.JSON as JSON

newtype Subsno = Subsno Int

instance eqSubsno :: Eq Subsno where
  eq (Subsno s1) (Subsno s2) = s1 == s2
derive instance newtypeSubsno :: Newtype Subsno _

toString :: Subsno -> String
toString (Subsno s) = show s

fromString :: String -> Maybe Subsno
fromString str = Subsno <$> Int.fromString str

type DeliveryAddress =
  { streetAddress :: Nullable String
  , zipcode       :: String
  , city          :: Nullable String
  , temporaryName :: Nullable String
  }

type PendingAddressChange =
  { address   :: DeliveryAddress
  , startDate :: JSDate
  , endDate   :: Nullable JSDate
  }

type Subscription = BaseSubscription SubscriptionPaymentMethod 

type BaseSubscription p =
  { subsno                :: Subsno
  , extno                 :: Int
  , cusno                 :: Cusno
  , paycusno              :: Cusno
  , kind                  :: String
  , state                 :: SubscriptionState
  , pricegroup            :: String
  , package               :: Package
  , dates                 :: SubscriptionDates
  , campaign              :: Campaign
  , paused                :: Nullable (Array PausedSubscription)
  , deliveryAddress       :: Nullable DeliveryAddress
  , receiver              :: Nullable String
  , pendingAddressChanges :: Nullable (Array PendingAddressChange)
  , deliveryTroubleEnd    :: Nullable JSDate
  , paymentMethod         :: p
  , paymentMethodId       :: Nullable PaymentMethodId
  }

data SubscriptionPaymentMethod
  = PaperInvoice
  | CreditCard
  | NetBank
  | ElectronicInvoice
  | DirectPayment
  | Email
  | UnknownPaymentMethod

newtype SleepType = SleepType String

derive instance newtypeSleepType :: Newtype SleepType _
derive newtype instance eqSleepType :: Eq SleepType

type PausedSubscription =
  { startDate :: JSDate
  , endDate   :: Nullable JSDate
  , sleepType :: SleepType
  }

isPause :: SleepType -> Boolean
isPause (SleepType s) = s == "Pause"

-- | Parse Foreign values of a 'raw' subscription into a Subscription
parseSubscription :: forall r. { paymentMethod :: Foreign | r } -> { paymentMethod :: SubscriptionPaymentMethod | r }
parseSubscription sub@{ paymentMethod } =
  let parsedPaymentMethod = case JSON.read paymentMethod of
        Left _  -> UnknownPaymentMethod
        Right p -> p
  in sub { paymentMethod = parsedPaymentMethod }

newtype SubscriptionState = SubscriptionState String

derive instance eqSubscriptionPaymentMethod :: Eq SubscriptionPaymentMethod

derive instance genericSubscriptionPaymentMethod :: Generic SubscriptionPaymentMethod _
instance readSubscriptionPaymentMethod :: ReadForeign SubscriptionPaymentMethod where
  readImpl p = genericDecodeEnum defaultGenericEnumOptions p <|> pure UnknownPaymentMethod

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

isSubscriptionPausable :: Subscription -> Boolean
isSubscriptionPausable = _.canPause <<< _.package

isSubscriptionTemporaryAddressChangable :: Subscription -> Boolean
isSubscriptionTemporaryAddressChangable = _.canTempAddr <<< _.package

isSubscriptionExpired :: Subscription -> Date -> Boolean
isSubscriptionExpired subs today =
  let end = toDate =<< toMaybe subs.dates.end
      suspend = toDate =<< toMaybe subs.dates.suspend
  in maybe false (_ < today) end || maybe false (_ <= today) suspend

-- Can the payment card be changed
isSubscriptionRenewable :: Subscription -> Boolean
isSubscriptionRenewable subs =
  (\(SubscriptionState x) -> x) subs.state `elem`
  [ "Active"
  , "Paused"
  , "RestartedAfterLatePayment"
  ]
