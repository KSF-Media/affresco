module KSF.Subscription.Component where

import Prelude

import AsyncWrapper as AsyncWrapper
import Data.Array (filter, mapMaybe)
import Data.Array as Array
import Data.DateTime (DateTime, adjust)
import Data.Foldable (foldMap)
import Data.Formatter.DateTime (FormatterCommand(..), format)
import Data.JSDate (JSDate, fromDateTime, toDateTime)
import Data.List (fromFoldable, intercalate)
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Nullable (Nullable, toMaybe)
import Data.Nullable as Nullable
import Data.String (trim)
import Data.Time.Duration (Days)
import Data.Time.Duration as Time.Duration
import Effect (Effect)
import Effect.Now as Now
import KSF.DescriptionList.Component as DescriptionList
import KSF.Grid as Grid
import KSF.PauseSubscription.Component as PauseSubscription
import KSF.TemporaryAddressChange.Component as TemporaryAddressChange
import Persona (InvalidPauseDateError(..))
import Persona as Persona
import React.Basic (JSX, StateUpdate(..), make, runUpdate)
import React.Basic as React
import React.Basic.DOM as DOM
import React.Basic.Events (handler_)
import React.Basic.Extended (Style)
import React.Basic.Extended as ReactExt

foreign import subscriptionStyles :: Style

type Self = React.Self Props State

type Props =
  { subscription :: Persona.Subscription
  , user :: Persona.User
  }

type State =
  { wrapperProgress :: AsyncWrapper.Progress JSX
  , pausedSubscriptions :: Maybe (Array Persona.PausedSubscription)
  , now :: Maybe DateTime
  , updateAction :: Maybe SubscriptionUpdateAction
  }

data SubscriptionUpdateAction
  = PauseSubscription
  | TemporaryAddressChange

data Action
  = SetWrapperProgress (AsyncWrapper.Progress JSX)
  | SetNow DateTime
  | SetPausedSubscriptions (Maybe (Array Persona.PausedSubscription))
  | SetTryAgainAction SubscriptionUpdateAction

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
      , now: Nothing
      , updateAction: Nothing
      }
  , render
  , didMount
  }

didMount :: Self -> Effect Unit
didMount self = do
  now <- Now.nowDateTime
  send self $ SetNow now
  send self $ SetPausedSubscriptions $ toMaybe self.props.subscription.paused

update :: Self -> Action -> StateUpdate Props State
update self = case _ of
  SetWrapperProgress progress -> Update $ self.state { wrapperProgress = progress }
  SetNow now -> Update $ self.state { now = Just now }
  SetPausedSubscriptions subs -> Update $ self.state { pausedSubscriptions = subs }
  SetTryAgainAction updateAction -> Update $ self.state { updateAction = Just updateAction }

send :: Self -> Action -> Effect Unit
send = runUpdate update

render :: Self -> JSX
render self@{ props: props@{ subscription: { package } } } =
  ReactExt.requireStyle
    subscriptionStyles
    $ Grid.row2
      (React.element
         DescriptionList.component
           { definitions:
               [ { term: "Produkt:"
                 , descriptions: [ package.name ]
                 }
               , { term: "Status:"
                 , descriptions:
                     [ translateStatus props.subscription.state ]
                     <> (foldMap (showPausedDates <<< filterExpiredPausePeriods) $ self.state.pausedSubscriptions)
                 }
               ]
               <> deliveryAddress
               <> foldMap pendingAddressChanges (toMaybe props.subscription.pendingAddressChanges)
               <> foldMap billingDateTerm nextBillingDate
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
              , descriptions: [ currentDeliveryAddress ]
              }

    pendingAddressChanges :: Array Persona.PendingAddressChange -> Array DescriptionList.Definition
    pendingAddressChanges pendingChanges = Array.singleton $
      { term: "Tillfällig  adressändringar:"
      , descriptions: map showPendingAddressChange pendingChanges
      }

    billingDateTerm :: String -> Array DescriptionList.Definition
    billingDateTerm date = Array.singleton $
      { term: "Nästa faktureringsdatum:"
      , descriptions: [ date ]
      }


    filterExpiredPausePeriods pausedSubs =
      case self.state.now of
        Nothing  -> pausedSubs
        Just now -> filter (not isPauseExpired now) pausedSubs

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
              , onClick: handler_ $ send self $ SetWrapperProgress (AsyncWrapper.Editing updateActionComponent)
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
        , onCancel: send self $ SetWrapperProgress AsyncWrapper.Ready
        , onLoading: send self $ SetWrapperProgress $ AsyncWrapper.Loading mempty
        , onSuccess: \_ -> send self $ SetWrapperProgress AsyncWrapper.Success
        , onError: \err -> send self $ SetWrapperProgress $ AsyncWrapper.Error "error :-("
        }

    pauseSubscriptionComponent =
        PauseSubscription.pauseSubscription
          { subsno: props.subscription.subsno
          , userUuid: props.user.uuid
          , onCancel: send self $ SetWrapperProgress AsyncWrapper.Ready
          , onLoading: send self $ SetWrapperProgress $ AsyncWrapper.Loading mempty
          , onSuccess: \pausedSubscription -> do
                         send self $ SetWrapperProgress AsyncWrapper.Success
                         send self $ SetPausedSubscriptions $ toMaybe pausedSubscription.paused

          , onError: \err ->
              let unexpectedError = "Något gick fel och vi kunde tyvärr inte genomföra den aktivitet du försökte utföra. Vänligen kontakta vår kundtjänst."
                  startDateError = "Din begäran om uppehåll i beställningen misslyckades. Uppehåll kan endast påbörjas fr.o.m. följande dag."
                  lengthError = "Din begäran om uppehåll i beställningen misslyckades, eftersom uppehålls perioden är för kort eller lång. Uppehållsperioden bör vara mellan 7 dagar och 3 månader långt."
                  overlappingError = "Din begäran om uppehåll i beställningen misslyckades, eftersom uppehållet går över ett annat uppehåll. Det måste vara minst en vecka mellan uppehållsperioderna."
                  tooRecentError = "Din begäran om uppehåll i beställningen misslyckades, eftersom uppehållet är för nära en annan uppehållsperiod. Det måste vara minst en vecka mellan uppehållsperioderna."
                  errMsg = case err of
                    PauseInvalidStartDate   -> startDateError
                    PauseInvalidLength      -> lengthError
                    PauseInvalidOverlapping -> overlappingError
                    PauseInvalidTooRecent   -> tooRecentError
                    PauseInvalidUnexpected  -> unexpectedError
              in send self $ SetWrapperProgress $ AsyncWrapper.Error errMsg
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
        showPauseView = handler_ $ do
          send self $ SetTryAgainAction TemporaryAddressChange
          send self $ SetWrapperProgress (AsyncWrapper.Editing pauseSubscriptionComponent)

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
            send self $ SetTryAgainAction TemporaryAddressChange
            send self $ SetWrapperProgress (AsyncWrapper.Editing temporaryAddressChangeComponent)

    nextBillingDate
      | Persona.isSubscriptionCanceled props.subscription = Nothing
      | otherwise =
          map trim $ formatDate =<< addOneDay props.subscription.dates.end

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
formatAddress { streetAddress, zipcode, city } = intercalate ", " [ streetAddress, zipcode, city ]

addOneDay :: Nullable JSDate -> Maybe JSDate
addOneDay date = do
  oneDayAdded <- adjust oneDay =<< toDateTime =<< Nullable.toMaybe date
  Just $ fromDateTime oneDayAdded
  where
    oneDay :: Days
    oneDay = Time.Duration.Days 1.0

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

isPauseExpired :: DateTime -> Persona.PausedSubscription -> Boolean
isPauseExpired baseDate { endDate } =
  case toMaybe endDate of
    -- If there's no end date, the pause is ongoing
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
