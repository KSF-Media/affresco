module KSF.Api.Subscription where

import Prelude

import Data.Argonaut.Decode (class DecodeJson, JsonDecodeError(..), decodeJson, (.!=), (.:), (.:?))
import Data.Argonaut.Core as Json
import Data.Identity (Identity (..))
import Bottega.Models.PaymentMethod (PaymentMethodId (..), toPaymentMethod)
import Control.Alt ((<|>))
import Data.Date (Date)
import Data.DateTime (DateTime, date)
import Data.Either (Either(..))
import Data.Generic.Rep (class Generic)
import Data.Int as Int
import Data.JSDate (JSDate, toDate)
import Data.Maybe (Maybe (..), maybe, fromMaybe)
import Data.Newtype (class Newtype, un, unwrap)
import Data.Nullable (Nullable, toMaybe)
import Data.String (toLower)
import Foreign (Foreign)
import Foreign.Generic.EnumEncoding (defaultGenericEnumOptions, genericDecodeEnum)
import KSF.Api.Package (Package, Campaign)
import KSF.Helpers (parseDateTime, jsonParseDateTime)
import KSF.User.Cusno (Cusno)
import Simple.JSON (class ReadForeign, readImpl)
import Simple.JSON as JSON
import KSF.User.Cusno (Cusno (..))

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

newtype PendingAddressChange = PendingAddressChange
  { address   :: DeliveryAddress
  , startDate :: DateTime
  , endDate   :: Maybe DateTime
  }

derive instance newtypePendingAddressChange :: Newtype PendingAddressChange _

instance decodeJsonPendingAddressChange :: DecodeJson PendingAddressChange where
  decodeJson json = do
    obj <- decodeJson json
    address <- obj .: "address"
    startDate <- jsonParseDateTime =<< obj .: "startDate"
    endDate <- map (parseDateTime =<< _) $ obj .:? "endDate"
    pure $ PendingAddressChange { startDate, endDate, address }

newtype Subscription = Subscription (BaseSubscription SubscriptionPaymentMethod)
derive instance newtypeSubscription :: Newtype Subscription _
instance decodeJsonSubscription :: DecodeJson Subscription where
  decodeJson json = do
    obj <- decodeJson json
    subsno <- Subsno <$> obj .: "subsno"
    extno  <- obj .: "extno"
    cusno <- Cusno <$> obj .: "cusno"
    paycusno <- Cusno <$> obj .: "paycusno"
    kind <- obj .: "kind"
    state <- SubscriptionState <$> obj .: "state"
    pricegroup <- obj .: "pricegroup"
    package <- obj .: "package"
    dates <- obj .: "dates"
    campaign <- obj .: "campaign"
    paused <- obj .:? "paused"
    deliveryAddress <- obj .:? "deliveryAddress"
    receiver <- obj .:? "receiver"
    pendingAddressChanges <- obj .:? "pendingAddresschanges"
    paymentMethod <- do
      x <-  obj .:? "paymentMethod"
      pure $ maybe UnknownPaymentMethod toSubscriptionPaymentMethod x
      -- (toSubscriptionPaymentMethod $ obj .:? "paymentMethod") .!= UnknownPaymentMethod
    paymentMethodId <- do
      (i :: Maybe Int) <- obj .:? "paymentMethodId"
      pure $ map PaymentMethodId i
    pure $ Subscription
      { subsno
      , extno
      , cusno
      , paycusno
      , kind
      , state
      , pricegroup
      , package
      , dates
      , campaign
      , paused
      , deliveryAddress
      , receiver
      , pendingAddressChanges
      , paymentMethod
      , paymentMethodId
      }

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
  , paused                :: Maybe (Array PausedSubscription)
  , deliveryAddress       :: Maybe DeliveryAddress
  , receiver              :: Maybe String
  , pendingAddressChanges :: Maybe (Array PendingAddressChange)
  , paymentMethod         :: p
  , paymentMethodId       :: Maybe PaymentMethodId
  }

data SubscriptionPaymentMethod
  = PaperInvoice
  | CreditCard
  | NetBank
  | ElectronicInvoice
  | DirectPayment
  | Email
  | UnknownPaymentMethod

toSubscriptionPaymentMethod :: String -> SubscriptionPaymentMethod
toSubscriptionPaymentMethod paymentMethod =
  case toLower paymentMethod of
    "paperinvoice"      -> PaperInvoice
    "creditcard"        -> CreditCard
    "netbank"           -> NetBank
    "electronicinvoice" -> ElectronicInvoice
    "directpayment"     -> DirectPayment
    "email"             -> Email
    _                   -> UnknownPaymentMethod

newtype PausedSubscription = PausedSubscription
  { startDate :: DateTime
  , endDate   :: Maybe DateTime
  }

derive instance newtypePausedSubscription :: Newtype PausedSubscription _

instance decodeJsonPausedSubscription :: DecodeJson PausedSubscription where
  decodeJson json = do
    obj <- decodeJson json
    startDate <- jsonParseDateTime =<< obj .: "startDate"
    endDate <- map (parseDateTime =<< _) $ obj .:? "endDate"
    pure $ PausedSubscription { startDate, endDate }


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

newtype SubscriptionDates = SubscriptionDates
  { lenMonths           :: Maybe Int
  , lenDays             :: Maybe Int
  , start               :: DateTime
  , end                 :: Maybe DateTime
  , unpaidBreak         :: Maybe DateTime
  , invoicingStart      :: Maybe DateTime
  , paidUntil           :: Maybe DateTime
  , suspend             :: Maybe DateTime
  }

instance decodeJsonSubscriptionDates :: DecodeJson SubscriptionDates where
  decodeJson json = do
    obj <- decodeJson json
    lenMonths <- obj .:? "lenMonths"
    lenDays <- obj .:? "lenDays"
    (start' :: String) <- obj .: "start"
    start <- do
      case parseDateTime start' of
        Just d -> pure d
        _ -> Left $ UnexpectedValue $ Json.fromString $ "Unexpected start date format: " <> start'
    end <- asd $ obj .:? "end"
    unpaidBreak <- asd $ obj .:? "unpaidBreak"
    invoicingStart <- asd $ obj .:? "invoicingStart"
    paidUntil <- asd $ obj .:? "paidUntil"
    suspend <- asd $ obj .:? "suspend"
    pure $ SubscriptionDates { lenMonths, lenDays, start, end, unpaidBreak, invoicingStart, paidUntil, suspend }
    where
      asd x = do
        (x' :: Maybe String) <- x
        pure $ parseDateTime =<< x'

isSubscriptionCanceled :: Subscription -> Boolean
isSubscriptionCanceled (Subscription s) = isSubscriptionStateCanceled s.state

isSubscriptionStateCanceled :: SubscriptionState -> Boolean
isSubscriptionStateCanceled (SubscriptionState "Canceled") = true
isSubscriptionStateCanceled _ = false

isSubscriptionPausable :: Subscription -> Boolean
isSubscriptionPausable = _.canPause <<< unwrap <<< _.package <<< unwrap

isSubscriptionTemporaryAddressChangable :: Subscription -> Boolean
isSubscriptionTemporaryAddressChangable = _.canTempAddr <<< unwrap <<< _.package <<< unwrap

isSubscriptionExpired :: Subscription -> Date -> Boolean
isSubscriptionExpired (Subscription { dates: SubscriptionDates dates }) today =
  let end = map date dates.end
      suspend = map date dates.suspend
  in maybe false (_ < today) end || maybe false (_ <= today) suspend
