module KSF.Subscription.Component where

import Prelude

import KSF.AsyncWrapper as AsyncWrapper
import Data.Array (filter)
import Data.Array as Array
import Data.DateTime (DateTime)
import Data.Foldable (foldMap)
import Data.Formatter.DateTime (FormatterCommand(..), format)
import Data.JSDate (JSDate, toDateTime)
import Data.List (fromFoldable, intercalate)
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Nullable (toMaybe)
import Data.String (trim)
import Effect (Effect)
import Effect.Now as Now
import KSF.DescriptionList.Component as DescriptionList
import KSF.Grid as Grid
import KSF.PauseSubscription.Component as PauseSubscription
import KSF.TemporaryAddressChange.Component as TemporaryAddressChange
import Persona (InvalidDateInput(..))
import Persona as Persona
import React.Basic (JSX, make)
import React.Basic as React
import React.Basic.DOM as DOM
import React.Basic.Events (handler_)

type Self = React.Self Props State

type Props =
  { subscription :: Persona.Subscription
  , user :: Persona.User
  }

type State =
  { wrapperProgress :: AsyncWrapper.Progress JSX
  , pausedSubscriptions :: Maybe (Array Persona.PausedSubscription)
  , pendingAddressChanges :: Maybe (Array Persona.PendingAddressChange)
  , now :: Maybe DateTime
  , updateAction :: Maybe SubscriptionUpdateAction
  }

data SubscriptionUpdateAction
  = PauseSubscription
  | TemporaryAddressChange

type Subscription =
  { package :: { name :: String
               , paper :: { name :: String }
               }
  , state :: String
  , dates :: Persona.SubscriptionDates
  }

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

component :: React.Component Props
component = React.createComponent "Subscription"

subscription :: Props -> JSX
subscription = make component
  { initialState:
      { wrapperProgress: AsyncWrapper.Ready
      , pausedSubscriptions: Nothing
      , pendingAddressChanges: Nothing
      , now: Nothing
      , updateAction: Nothing
      }
  , render
  , didMount
  }

didMount :: Self -> Effect Unit
didMount self = do
  now <- Now.nowDateTime
  self.setState _
    { now = Just now
    , pausedSubscriptions = toMaybe self.props.subscription.paused
    , pendingAddressChanges = toMaybe self.props.subscription.pendingAddressChanges
    }

render :: Self -> JSX
render self@{ props: props@{ subscription: sub@{ package } } } =
  Grid.row2
    (DescriptionList.descriptionList
         { definitions:
             [ { term: "Produkt:"
               , description: [ DOM.text package.name ]
               }
             , { term: "Pren.nr:"
               , description: [ DOM.text $ show sub.subsno ]
               }
             , { term: "Status:"
               , description:
                   [ DOM.text $ translateStatus props.subscription.state ]
                   <> (map DOM.text $ foldMap (showPausedDates <<< filterExpiredPausePeriods) $ self.state.pausedSubscriptions)
               }
             ]
             <> deliveryAddress
             <> foldMap pendingAddressChanges self.state.pendingAddressChanges
             <> foldMap billingDateTerm billingPeriodEndDate
         })
      (if package.digitalOnly
       then mempty
       else subscriptionUpdates)
      $ Just { extraClasses: [ "subscription--container" ] }
  where
    deliveryAddress =
       if package.digitalOnly
       then mempty
       else Array.singleton
              { term: "Leveransadress:"
              , description: [ DOM.text currentDeliveryAddress ]
              }

    pendingAddressChanges :: Array Persona.PendingAddressChange -> Array DescriptionList.Definition
    pendingAddressChanges pendingChanges = Array.singleton $
      { term: "Tillfällig adressändring:"
      , description: map (DOM.text <<< showPendingAddressChange) (filterExpiredPendingChanges pendingChanges)
      }

    billingDateTerm :: String -> Array DescriptionList.Definition
    billingDateTerm date = Array.singleton $
      { term: "Faktureringsperioden upphör:"
      , description: [ DOM.text date ]
      }

    filterExpiredPausePeriods :: Array Persona.PausedSubscription -> Array Persona.PausedSubscription
    filterExpiredPausePeriods pausedSubs =
      case self.state.now of
        Nothing  -> pausedSubs
        Just now -> filter (not isPeriodExpired now <<< toMaybe <<< _.endDate) pausedSubs

    filterExpiredPendingChanges :: Array Persona.PendingAddressChange -> Array Persona.PendingAddressChange
    filterExpiredPendingChanges pendingChanges =
      case self.state.now of
        Nothing  -> pendingChanges
        Just now -> filter (not isPeriodExpired now <<< Just <<< _.endDate) pendingChanges

    subscriptionUpdates :: JSX
    subscriptionUpdates =
        Grid.row_ [ asyncWrapper ]
        where
          asyncWrapper = AsyncWrapper.asyncWrapper
            { wrapperState: self.state.wrapperProgress
            , readyView: pauseContainer [ pauseIcon, temporaryAddressChangeIcon ]
            , editingView: identity
            , successView: pauseContainer [ DOM.div { className: "subscription--update-success check-icon" } ]
            , errorView: \err -> errorContainer [ errorMessage err, tryAgain ]
            }

          errorMessage msg =
            DOM.div
              { className: "error-text"
              , children: [ DOM.text msg ]
              }
          tryAgain =
            DOM.span
              { className: "subscription--try-update-again"
              , children: [ DOM.text "Försök igen" ]
              , onClick: handler_ $ self.setState _ { wrapperProgress = AsyncWrapper.Editing updateActionComponent }
              }
            where
              updateActionComponent =
                case self.state.updateAction of
                  Just PauseSubscription -> pauseSubscriptionComponent
                  Just TemporaryAddressChange -> temporaryAddressChangeComponent
                  Nothing -> mempty

    temporaryAddressChangeComponent =
      TemporaryAddressChange.temporaryAddressChange
        { subsno: props.subscription.subsno
        , userUuid: props.user.uuid
        , onCancel: self.setState _ { wrapperProgress = AsyncWrapper.Ready }
        , onLoading: self.setState _ { wrapperProgress = AsyncWrapper.Loading mempty }
        , onSuccess: \{ pendingAddressChanges: newPendingChanges } ->
                       self.setState _
                         { pendingAddressChanges = toMaybe newPendingChanges
                         , wrapperProgress = AsyncWrapper.Success
                         }

        , onError: \(err :: Persona.InvalidDateInput) ->
              let unexpectedError = "Något gick fel och vi kunde tyvärr inte genomföra den aktivitet du försökte utföra. Vänligen kontakta vår kundtjänst."
                  startDateError = "Din begäran om tillfällig adressändring i beställningen misslyckades. Tillfällig adressändring kan endast påbörjas fr.o.m. följande dag."
                  lengthError = "Din begäran om tillfällig adressändring i beställningen misslyckades, eftersom tillfällig adressändring perioden är för kort. Adressändringperioden bör vara åtminstone 7 dagar långt."
                  errMsg = case err of
                    InvalidStartDate   -> startDateError
                    InvalidLength      -> lengthError
                    _                  -> unexpectedError
              in self.setState _ { wrapperProgress = AsyncWrapper.Error errMsg }
        }

    pauseSubscriptionComponent =
        PauseSubscription.pauseSubscription
          { subsno: props.subscription.subsno
          , userUuid: props.user.uuid
          , onCancel: self.setState _ { wrapperProgress = AsyncWrapper.Ready }
          , onLoading: self.setState _ { wrapperProgress = AsyncWrapper.Loading mempty }
          , onSuccess: \pausedSubscription ->
                         self.setState _
                           { pausedSubscriptions = toMaybe pausedSubscription.paused
                           , wrapperProgress = AsyncWrapper.Success
                           }

          , onError: \err ->
              let unexpectedError = "Något gick fel och vi kunde tyvärr inte genomföra den aktivitet du försökte utföra. Vänligen kontakta vår kundtjänst."
                  startDateError = "Din begäran om uppehåll i beställningen misslyckades. Uppehåll kan endast påbörjas fr.o.m. följande dag."
                  lengthError = "Din begäran om uppehåll i beställningen misslyckades, eftersom uppehålls perioden är för kort eller lång. Uppehållsperioden bör vara mellan 7 dagar och 3 månader långt."
                  overlappingError = "Din begäran om uppehåll i beställningen misslyckades, eftersom uppehållet går över ett annat uppehåll. Det måste vara minst en vecka mellan uppehållsperioderna."
                  tooRecentError = "Din begäran om uppehåll i beställningen misslyckades, eftersom uppehållet är för nära en annan uppehållsperiod. Det måste vara minst en vecka mellan uppehållsperioderna."
                  errMsg = case err of
                    InvalidStartDate   -> startDateError
                    InvalidLength      -> lengthError
                    InvalidOverlapping -> overlappingError
                    InvalidTooRecent   -> tooRecentError
                    InvalidUnexpected  -> unexpectedError
              in self.setState _ { wrapperProgress = AsyncWrapper.Error errMsg }
          }

    pauseContainer children =
      DOM.div { className: "subscription--pause-container flex", children }

    errorContainer children =
      DOM.div { className: "subscription--error-container flex", children }

    loadingSpinner = [ DOM.div { className: "tiny-spinner" } ]

    pauseIcon =
      DOM.div
        { className: "subscription--action-item"
        , children:
          [ DOM.div
              { className: "subscription--pause-icon circle"
              , onClick: showPauseView
              }
          , DOM.span
              { className: "subscription--update-action-text"
              , children:
                  [ DOM.u_ [ DOM.text "Gör uppehåll" ] ]
              , onClick: showPauseView
              }
          ]
        }
      where
        showPauseView = handler_ $
          self.setState _
            { updateAction = Just PauseSubscription
            , wrapperProgress = AsyncWrapper.Editing pauseSubscriptionComponent
            }

    temporaryAddressChangeIcon =
      DOM.div
        { className: "subscription--action-item"
        , children:
            [ DOM.div
                { className: "subscription--temporary-address-change-icon circle"
                , onClick: showTemporaryAddressChange
                }
            , DOM.span
                { className: "subscription--update-action-text"
                , children:
                    [ DOM.u_ [ DOM.text "Gör tillfällig adressändring" ] ]
                , onClick: showTemporaryAddressChange
                }
            ]
        }
        where
          showTemporaryAddressChange = handler_ $ do
            self.setState _
              { updateAction = Just TemporaryAddressChange
              , wrapperProgress = AsyncWrapper.Editing temporaryAddressChangeComponent
              }

    billingPeriodEndDate =
          map trim $ formatDate =<< toMaybe props.subscription.dates.end

    currentDeliveryAddress :: String
    currentDeliveryAddress
      | Just address <- toMaybe props.subscription.deliveryAddress
      = formatAddress address
      | Just { streetAddress, zipCode, city } <- toMaybe props.user.address
      = intercalate ", "
          [ streetAddress
          , fromMaybe "-" $ toMaybe zipCode
          , fromMaybe "-" $ toMaybe city
          ]
      | otherwise = "-"

formatAddress :: Persona.DeliveryAddress -> String
formatAddress { streetAddress, zipcode, city } = intercalate ", " [ fromMaybe "-" $ toMaybe streetAddress, zipcode, fromMaybe "-" $ toMaybe city ]

-- | Translates English status to Swedish.
-- | Described in https://git.ksfmedia.fi/taco/faro/blob/master/kayak-api-details.md
translateStatus :: Persona.SubscriptionState -> String
translateStatus (Persona.SubscriptionState englishStatus) = do
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

isPeriodExpired :: DateTime -> Maybe JSDate -> Boolean
isPeriodExpired baseDate endDate =
  case endDate of
    -- If there's no end date, the period is ongoing
    Nothing   -> false
    Just date ->
      let endDateTime = toDateTime date
      in maybe true (_ < baseDate) endDateTime

showPausedDates :: Array Persona.PausedSubscription -> Array String
showPausedDates pausedSubs =
  let formatDates { startDate, endDate } = formatDateString startDate $ toMaybe endDate
  in map (((<>) "Uppehåll: ") <<< formatDates) pausedSubs

showPendingAddressChange :: Persona.PendingAddressChange -> String
showPendingAddressChange { address, startDate, endDate } =
  let addressString = formatAddress address
      pendingPeriod = formatDateString startDate (Just endDate)
  in addressString <> " (" <> pendingPeriod <> ")"

formatDateString :: JSDate -> Maybe JSDate -> String
formatDateString startDate endDate
  | Just startString <- formatDate startDate =
    let endString = fromMaybe "" $ formatDate =<< endDate
    in startString <> " – " <> endString
  | otherwise = mempty
