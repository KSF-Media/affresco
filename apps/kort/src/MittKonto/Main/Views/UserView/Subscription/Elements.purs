module MittKonto.Main.UserView.Subscription.Elements where

import Prelude

import Data.Array (filter, find)
import Data.Array as Array
import Data.Enum (enumFromTo)
import Data.Foldable (foldMap, for_)
import Data.JSDate (toDate)
import Data.List (intercalate)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Newtype (unwrap)
import Data.Nullable (toMaybe)
import Data.String (length, splitAt, trim)
import Data.Tuple (Tuple(..))
import Foreign (unsafeToForeign)
import KSF.Api.Subscription (SubscriptionPaymentMethod(..))
import KSF.Api.Subscription (toString) as Subsno
import KSF.DescriptionList as DescriptionList
import KSF.Grid as Grid
import KSF.Helpers (formatDateDots)
import KSF.User as User
import KSF.Window (clearOpener)
import MittKonto.Main.UserView.Subscription.Helpers as Helpers
import MittKonto.Main.UserView.Subscription.Types as Types
import React.Basic (JSX)
import React.Basic.DOM as DOM
import React.Basic.DOM.Events (preventDefault)
import React.Basic.Events (handler)
import Web.HTML as Web.HTML
import Web.HTML.Window as Window

receiverName :: Types.Self -> Array DescriptionList.Definition
receiverName { props: { subscription: { receiver } } } =
  foldMap (\r -> Array.singleton
              { term: "Mottagare:"
              , description: [ DOM.text r ]
              }) $ toMaybe receiver

deliveryAddress :: Types.Self -> Array DescriptionList.Definition
deliveryAddress { props: { subscription: { deliveryAddress: subDeliveryAddress, package }, user: { address: userAddress } } } =
  if package.digitalOnly
  then mempty
  else Array.singleton
         { term: "Leveransadress:"
         , description: [ DOM.text currentDeliveryAddress ]
         }
  where
    currentDeliveryAddress :: String
    currentDeliveryAddress
      | Just address <- toMaybe subDeliveryAddress
      = Helpers.formatAddress address
      | Just { streetAddress, zipCode, city } <- toMaybe userAddress
      = intercalate ", "
          [ streetAddress
          , fromMaybe "-" $ toMaybe zipCode
          , fromMaybe "-" $ toMaybe city
          ]
      | otherwise = "-"

paymentMethod :: Types.Self -> Array DescriptionList.Definition
paymentMethod { props: { subscription: { paymentMethod: method, paymentMethodId }, user: { creditCards } } } = Array.singleton
  { term: "Faktureringsmetod:"
  , description: [ DOM.div_ [ DOM.text $ Helpers.translatePaymentMethod method
                            , case method of
                                CreditCard -> subscriptionCreditCard
                                _ -> mempty
                            ]
                 ]
  }
  where
    subscriptionCreditCard :: JSX
    subscriptionCreditCard
      | Just id <- toMaybe paymentMethodId,
        Just card <- find (\c -> c.paymentMethodId == id) creditCards =
          DOM.ul_ [ DOM.li_ [ DOM.text $ "Nummer: " <> card.maskedPan ]
                  , DOM.li_ [ DOM.text $ "Utgångsdatum: " <> formatExpiryDate card.expiryDate ]
                  ]
      | otherwise = mempty

    formatExpiryDate :: String -> String
    formatExpiryDate expiryDate
      | (length expiryDate) == 4 =
          let { before: year, after: month } = splitAt 2 expiryDate
           in
            month <> "/" <> year
      | otherwise = ""

pendingAddressChanges :: Types.Self -> Array DescriptionList.Definition
pendingAddressChanges self@{ state: { pendingAddressChanges: pendingChanges }, props: { now } } =
  if Array.null filteredChanges then mempty else Array.singleton $
  { term: "Tillfällig adressändring:"
  , description: map (showPendingAddressChange self) $
      Array.zip (enumFromTo 1 $ Array.length filteredChanges) filteredChanges
  }
  where
    filteredChanges = foldMap filterExpiredPendingChanges pendingChanges
    filterExpiredPendingChanges :: Array User.PendingAddressChange -> Array User.PendingAddressChange
    filterExpiredPendingChanges changes =
      filter (not <<< Helpers.isPeriodExpired true now <<< toMaybe <<< _.endDate) changes

showPendingAddressChange :: Types.Self -> (Tuple Int User.PendingAddressChange) -> JSX
showPendingAddressChange self (Tuple n { address, startDate, endDate }) =
  let addressString = Helpers.formatAddress address
      pendingPeriod = Helpers.formatDateString startDate (toMaybe endDate)
  in DOM.div
       { children: [ DOM.text $ addressString <> " (" <> pendingPeriod <> ")"
                   ]
       , id: "subscription-" <> Subsno.toString self.props.subscription.subsno <> "-pending-address-change-" <> show n
       }

billingDateTerm :: Types.Self -> Array DescriptionList.Definition
billingDateTerm { props: { subscription: { dates: { end } } } } = foldMap
  (\e -> Array.singleton $
      { term: "Faktureringsperioden upphör:"
      , description: [ DOM.text $ e ]
      }
  ) $ trim <<< formatDateDots <$> (toDate =<< toMaybe end)

subscriptionUpdates :: Types.Self -> JSX
subscriptionUpdates self@{ props: props@{ subscription: sub@{ subsno } } } =
  Grid.row_ extraActions
  where
    extraActions =
      if sub.paymentMethod == CreditCard && sub.paycusno == props.user.cusno
        then [ creditCardUpdateIcon ]
        else mempty

    creditCardUpdateIcon =
      DOM.div
        { className: "subscription--action-item"
        , children: [ DOM.a
                        { onClick: handler preventDefault $ \_ -> do
                                    window <- Web.HTML.window
                                    w <- Window.open "" "_blank" "" window
                                    for_ w clearOpener
                                    self.props.updateWindow $ Just w
                                    props.router.pushState (unsafeToForeign {}) $ "/prenumerationer/" <> Subsno.toString subsno <> "/kreditkort/uppdatera"
                        , href: "/prenumerationer/" <> Subsno.toString subsno <> "/kreditkort/uppdatera"
                        , children: [ DOM.div
                                        { className: "subscription--action-link"
                                        , children: [ DOM.div
                                                        { className: "subscription--credit-card-update-icon circle"
                                                        }
                                                    , DOM.span
                                                        { className: "subscription--update-action-text"
                                                        , children:
                                                            [ DOM.u_ [ DOM.text "Uppdatera ditt kredit- eller bankkort" ] ]
                                                        }
                                                    ]
                                        }
                                    ]
                        }
                    , DOM.div
                        { className: "subscription--update-action-addtional-text"
                        , children: [DOM.text "(Öppnas i ett nytt fönster. Vid registreringen görs en täckningsreservation på 1 euro som inte debiteras från kortet.)"]
                        }
                    ]
        }

subscriptionEndTerm :: Types.Self -> Array DescriptionList.Definition
subscriptionEndTerm { props: { subscription: { dates: { suspend } } } } = foldMap
  (\s -> Array.singleton $
      { term: "Prenumerationens slutdatum:"
      , description: [ DOM.text s ]
      }
  ) $ trim <<< formatDateDots <$> (toDate =<< toMaybe suspend)

showPausedDates :: Array User.PausedSubscription -> Array JSX
showPausedDates =
  let formatDates { startDate, endDate } = Helpers.formatDateString startDate $ toMaybe endDate
      pauseLine pause =
        let pauseTypeText = case unwrap pause.sleepType of
              "Pause" -> "Uppehåll"
              "Rebate" -> "Kompensation"
              _ -> "Uppehåll"
            text = pauseTypeText <> ": " <> formatDates pause
        in case Tuple (toDate pause.startDate) (toDate =<< toMaybe pause.endDate) of
          Tuple (Just _startDate) (Just _endDate) ->
            DOM.div
              { children: [ DOM.text text
                          ]
              , className: "subscription--subscription-pause"
              }
          _ -> DOM.text text
  in map pauseLine
