module KSF.Api.Subscription where

import Prelude

import Control.Alt ((<|>))
import Data.Date (Date)
import Data.Either (Either(..))
import Data.Generic.Rep (class Generic)
import Data.JSDate (JSDate, toDate)
import Data.Int as Int
import Data.Maybe (Maybe(..), maybe)
import Data.Nullable (Nullable, toMaybe)
import Data.Traversable (sequence)
import Foreign (Foreign)
import Foreign.Generic.EnumEncoding (defaultGenericEnumOptions, genericDecodeEnum)
import Foreign.Generic (defaultOptions, genericEncode, genericDecode)
import Foreign.Generic.Class (class Encode, class Decode)
import KSF.Api.Package (Package, Campaign, JSPackage, JSCampaign, fromJSPackage, fromJSCampaign)
import KSF.User.Cusno (Cusno)
import Simple.JSON (class ReadForeign, readImpl)
import Simple.JSON as JSON

newtype Subsno = Subsno Int

instance eqSubsno :: Eq Subsno where
  eq (Subsno s1) (Subsno s2) = s1 == s2

toString :: Subsno -> String
toString (Subsno s) = show s

fromString :: String -> Maybe Subsno
fromString str = Subsno <$> Int.fromString str

type DeliveryAddress =
  { streetAddress :: Maybe String
  , zipcode       :: String
  , city          :: Maybe String
  , temporaryName :: Maybe String
  }

type JSDeliveryAddress =
  { streetAddress :: Nullable String
  , zipcode       :: String
  , city          :: Nullable String
  , temporaryName :: Nullable String
  }

fromJSDeliveryAddress :: JSDeliveryAddress -> DeliveryAddress
fromJSDeliveryAddress j =
  { streetAddress: toMaybe j.streetAddress
  , zipcode: j.zipcode
  , city: toMaybe j.city
  , temporaryName: toMaybe j.temporaryName
  }

type PendingAddressChange =
  { address   :: DeliveryAddress
  , startDate :: Date
  , endDate   :: Maybe Date
  }

type JSPendingAddressChange =
  { address   :: JSDeliveryAddress
  , startDate :: JSDate
  , endDate   :: Nullable JSDate
  }

fromJSPendingAddressChange :: JSPendingAddressChange -> Maybe PendingAddressChange
fromJSPendingAddressChange j@{ address } = do
  startDate <- toDate j.startDate
  endDate <- maybe (pure Nothing) (map Just <<< toDate) $ toMaybe j.endDate
  pure { address: { streetAddress: toMaybe address.streetAddress
                  , zipcode: address.zipcode
                  , city: toMaybe address.city
                  , temporaryName: toMaybe address.temporaryName
                  }
       , startDate
       , endDate
       }

type Subscription =
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
  , paused                :: Maybe (Array PausedSubscription)
  , deliveryAddress       :: Maybe DeliveryAddress
  , receiver              :: Maybe String
  , pendingAddressChanges :: Maybe (Array PendingAddressChange)
  , paymentMethod         :: SubscriptionPaymentMethod
  }

type JSSubscription =
  { subsno                :: Subsno
  , extno                 :: Int
  , cusno                 :: Cusno
  , paycusno              :: Cusno
  , kind                  :: String
  , state                 :: SubscriptionState
  , pricegroup            :: String
  , package               :: JSPackage
  , dates                 :: JSSubscriptionDates
  , campaign              :: JSCampaign
  , paused                :: Nullable (Array JSPausedSubscription)
  , deliveryAddress       :: Nullable JSDeliveryAddress
  , receiver              :: Nullable String
  , pendingAddressChanges :: Nullable (Array JSPendingAddressChange)
  , paymentMethod         :: Foreign
  }

fromJSSubscription :: JSSubscription -> Maybe Subscription
fromJSSubscription j = do
  let maybeDate = maybe (pure Nothing) (map Just <<< toDate) <<< toMaybe
  package <- fromJSPackage j.package
  campaign <- fromJSCampaign j.campaign
  dates <- fromJSSubscriptionDates j.dates
{-
  dates <-
    { lenMonths: toMaybe j.dates.lenMonths
    , lenDays : toMaybe j.dates.lenDays
    , start: _
    , end: _
    , unpaidBreak: _
    , invoicingStart: _
    , paidUntil: _
    , suspend: _
    }
    <$> toDate j.dates.start
    <*> maybeDate j.dates.end
    <*> maybeDate j.dates.unpaidBreak
    <*> maybeDate j.dates.invoicingStart
    <*> maybeDate j.dates.paidUntil
    <*> maybeDate j.dates.suspend
-}
  paused <- maybe (pure Nothing)
            (Just <<< sequence <<< map (\p -> { startDate: _
                                              , endDate: _
                                              }
                                              <$> toDate p.startDate
                                              <*> maybeDate p.endDate
                                       )
            ) $ toMaybe j.paused
  pendingAddressChanges <- maybe (pure Nothing)
                           (map Just <<< sequence <<< map fromJSPendingAddressChange) $
                           toMaybe j.pendingAddressChanges
  pure { subsno: j.subsno
       , extno: j.extno
       , cusno: j.cusno
       , paycusno: j.paycusno
       , kind: j.kind
       , state: j.state
       , pricegroup: j.pricegroup
       , package
       , dates
       , campaign
       , paused
       , deliveryAddress: fromJSDeliveryAddress <$> toMaybe j.deliveryAddress
       , receiver: toMaybe j.receiver
       , pendingAddressChanges
       , paymentMethod: case JSON.read j.paymentMethod of
                          Left _  -> UnknownPaymentMethod
                          Right p -> p
       }

data SubscriptionPaymentMethod
  = PaperInvoice
  | CreditCard
  | NetBank
  | ElectronicInvoice
  | DirectPayment
  | Email
  | UnknownPaymentMethod

type PausedSubscription =
  { startDate :: Date
  , endDate   :: Maybe Date
  }

type JSPausedSubscription =
  { startDate :: JSDate
  , endDate   :: Nullable JSDate
  }

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

instance decodeSubscriptionState :: Decode SubscriptionState where
  decode = genericDecode defaultOptions

instance encodeSubscriptionState :: Encode SubscriptionState where
  encode = genericEncode defaultOptions

type SubscriptionDates =
  { lenMonths           :: Maybe Int
  , lenDays             :: Maybe Int
  , start               :: Date
  , end                 :: Maybe Date
  , unpaidBreak         :: Maybe Date
  , invoicingStart      :: Maybe Date
  , paidUntil           :: Maybe Date
  , suspend             :: Maybe Date
  }

type JSSubscriptionDates =
  { lenMonths           :: Nullable Int
  , lenDays             :: Nullable Int
  , start               :: JSDate
  , end                 :: Nullable JSDate
  , unpaidBreak         :: Nullable JSDate
  , invoicingStart      :: Nullable JSDate
  , paidUntil           :: Nullable JSDate
  , suspend             :: Nullable JSDate
  }

fromJSSubscriptionDates :: JSSubscriptionDates -> Maybe SubscriptionDates
fromJSSubscriptionDates dates =
  { lenMonths: toMaybe dates.lenMonths
  , lenDays : toMaybe dates.lenDays
  , start: _
  , end: _
  , unpaidBreak: _
  , invoicingStart: _
  , paidUntil: _
  , suspend: _
  }
  <$> toDate dates.start
  <*> maybeDate dates.end
  <*> maybeDate dates.unpaidBreak
  <*> maybeDate dates.invoicingStart
  <*> maybeDate dates.paidUntil
  <*> maybeDate dates.suspend
  where
    maybeDate = maybe (pure Nothing) (map Just <<< toDate) <<< toMaybe

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
  let end = subs.dates.end
      suspend = subs.dates.suspend
  in maybe false (_ < today) end || maybe false (_ <= today) suspend
