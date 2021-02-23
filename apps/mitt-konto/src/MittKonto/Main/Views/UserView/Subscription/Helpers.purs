module MittKonto.Main.UserView.Subscription.Helpers where

import Prelude

import Data.DateTime (DateTime)
import Data.Formatter.DateTime (FormatterCommand(..), format)
import Data.JSDate (JSDate, toDateTime)
import Data.List (fromFoldable, intercalate)
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Nullable (toMaybe)
import KSF.Api.Subscription (SubscriptionPaymentMethod(..))
import KSF.User as User

formatDate :: JSDate -> Maybe String
formatDate date = format formatter <$> toDateTime date
  where
    dot = Placeholder "."
    formatter = fromFoldable
      [ DayOfMonthTwoDigits
      , dot
      , MonthTwoDigits
      , dot
      , YearFull
      ]

formatAddress :: User.DeliveryAddress -> String
formatAddress { temporaryName, streetAddress, zipcode, city } =
  (maybe "" (_ <> ", ") $ toMaybe temporaryName) <>
  intercalate ", " [ fromMaybe "-" $ toMaybe streetAddress, zipcode, fromMaybe "-" $ toMaybe city ]

-- | Translates English status to Swedish.
-- | Described in https://git.ksfmedia.fi/taco/faro/blob/master/kayak-api-details.md
translateStatus :: User.SubscriptionState -> String
translateStatus (User.SubscriptionState englishStatus) = do
  case englishStatus of
    "Upcoming"                  -> "Under behandling"
    "Active"                    -> "Aktiv"
    "Paused"                    -> "Uppehåll"
    "Ended"                     -> "Avslutad"
    "UnpaidAndCanceled"         -> "Obetald faktura, avslutad prenumeration."
    "Canceled"                  -> "Avbeställd"
    "CanceledWithLatePayment"   -> "Avslutad efter förfallen faktura."
    "RestartedAfterLatePayment" -> "Aktiverad"
    "DeactivatedRecently"       -> "Förnyad tillsvidare"
    "Unknown"                   -> "Okänd"
    _                           -> englishStatus

translatePaymentMethod :: SubscriptionPaymentMethod -> String
translatePaymentMethod paymentMethod =
  case paymentMethod of
    PaperInvoice         -> "Pappersfaktura"
    CreditCard           -> "Kreditkort"
    NetBank              -> "Netbank"
    ElectronicInvoice    -> "Nätfaktura"
    DirectPayment        -> "Direktbetalning"
    UnknownPaymentMethod -> "Okänd"

isPeriodExpired :: DateTime -> Maybe JSDate -> Boolean
isPeriodExpired baseDate endDate =
  case endDate of
    -- If there's no end date, the period is ongoing
    Nothing   -> false
    Just date ->
      let endDateTime = toDateTime date
      in maybe true (_ < baseDate) endDateTime

showPausedDates :: Array User.PausedSubscription -> Array String
showPausedDates pausedSubs =
  let formatDates { startDate, endDate } = formatDateString startDate $ toMaybe endDate
  in map (((<>) "Uppehåll: ") <<< formatDates) pausedSubs

showPendingAddressChange :: User.PendingAddressChange -> String
showPendingAddressChange { address, startDate, endDate } =
  let addressString = formatAddress address
      pendingPeriod = formatDateString startDate (toMaybe endDate)
  in addressString <> " (" <> pendingPeriod <> ")"

formatDateString :: JSDate -> Maybe JSDate -> String
formatDateString startDate endDate
  | Just startString <- formatDate startDate =
    let endString = fromMaybe "" $ formatDate =<< endDate
    in startString <> " – " <> endString
  | otherwise = mempty