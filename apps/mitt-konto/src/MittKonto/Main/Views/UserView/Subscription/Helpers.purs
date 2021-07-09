module MittKonto.Main.UserView.Subscription.Helpers where

import Prelude

import Data.Date (Date)
import Data.List (intercalate)
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import KSF.Api.Subscription (SubscriptionPaymentMethod(..))
import KSF.Helpers (formatDateDots)
import KSF.User as User

formatAddress :: User.DeliveryAddress -> String
formatAddress { temporaryName, streetAddress, zipcode, city } =
  (maybe "" (_ <> ", ") temporaryName) <>
  intercalate ", " [ fromMaybe "-" streetAddress, zipcode, fromMaybe "-" city ]

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
    NetBank              -> "Nätbank"
    ElectronicInvoice    -> "Nätfaktura"
    DirectPayment        -> "Direktbetalning"
    Email                -> "E-post"
    UnknownPaymentMethod -> "Okänd"

isPeriodExpired :: Boolean -> Date -> Maybe Date -> Boolean
isPeriodExpired excludeCurrentDay baseDate endDate =
  case endDate of
    -- If there's no end date, the period is ongoing
    Nothing   -> false
    Just date ->
      let op end = if excludeCurrentDay then end < baseDate else end <= baseDate
      in op date

formatDateString :: Date -> Maybe Date -> String
formatDateString startDate endDate =
  let startString = formatDateDots startDate
      endString = maybe "" formatDateDots endDate
  in startString <> " – " <> endString

successText :: Maybe String
successText = Just "Tack, åtgärden lyckades!"
