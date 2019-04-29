module KSF.Subscription.Component where

import Prelude

import AsyncWrapper as AsyncWrapper
import Data.Array (filter, mapMaybe)
import Data.DateTime (DateTime, adjust)
import Data.Foldable (foldMap)
import Data.Formatter.DateTime (FormatterCommand(..), format)
import Data.JSDate (JSDate, fromDateTime, toDateTime)
import Data.List (fromFoldable)
import Data.Maybe (Maybe(..), maybe)
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
  { wrapperProgress :: AsyncWrapper.Progress
  , now :: Maybe DateTime
  }

data Action
  = SetWrapperProgress AsyncWrapper.Progress
  | SetNow DateTime

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
  { initialState: { wrapperProgress: AsyncWrapper.Ready, now: Nothing }
  , render
  }

didMount :: Self -> Effect Unit
didMount self = do
  now <- Now.nowDateTime
  send self $ SetNow now

update :: Self -> Action -> StateUpdate Props State
update self = case _ of
  SetWrapperProgress progress -> Update $ self.state { wrapperProgress = progress }
  SetNow now -> Update $ self.state { now = Just now }

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
                     <> (foldMap (showPausedDates <<< filterExpiredPausePeriods) $ toMaybe props.subscription.paused)
                 }
               ]
               <> foldMap billingDateTerm nextBillingDate
           })
      (if package.digitalOnly
       then mempty
       else pauseSubscription)
      $ Just { extraClasses: [ "subscription--container" ] }
  where
    billingDateTerm date =
      [ { term: "Nästa faktureringsdatum:"
        , descriptions: [ date ]
        }
      ]

    filterExpiredPausePeriods pausedSubs =
      case self.state.now of
        Nothing  -> pausedSubs
        Just now -> filter (isPauseExpired now) pausedSubs

    pauseSubscription :: JSX
    pauseSubscription =
      DOM.div
        { className: ""
        , children: [ asyncWrapper ]
        }
        where
          asyncWrapper = AsyncWrapper.asyncWrapper
            { wrapperState: self.state.wrapperProgress
            , readyView: pauseContainer pauseIcon
            , editingView: pauseSubscriptionComponent
            , errorView: pauseContainer [ DOM.text "Error :(" ]
            }

    pauseSubscriptionComponent =
        PauseSubscription.pauseSubscription
          { subsno: props.subscription.subsno
          , userUuid: props.user.uuid
          , onCancel: send self $ SetWrapperProgress AsyncWrapper.Ready
          , onLoading: send self $ SetWrapperProgress AsyncWrapper.Loading
          , onSuccess: pure unit -- send self $ SetWrapperProgress PauseSubscriptionSuccess
          , onError: send self $ SetWrapperProgress AsyncWrapper.Error
          }

    pauseContainer children =
      DOM.div { className: "subscription--pause-container flex", children }

    loadingSpinner = [ DOM.div { className: "tiny-spinner" } ]

    pauseIcon =
      [ DOM.div
          { className: "subscription--pause-icon circle"
          , onClick: showPauseView
          }
      , DOM.span
          { className: "subscription--pause-text"
          , children:
              [ DOM.u_ [ DOM.text "Gör uppehåll" ] ]
          , onClick: showPauseView
          }
      ]
      where
        showPauseView = handler_ $ send self $ SetWrapperProgress AsyncWrapper.Editing

    nextBillingDate
      | Persona.isSubscriptionCanceled props.subscription = Nothing
      | otherwise =
          map trim $ formatDate =<< addOneDay props.subscription.dates.end

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
  let endDateTime = toDateTime endDate
  in maybe true (_ < baseDate) endDateTime

showPausedDates :: Array Persona.PausedSubscription -> Array String
showPausedDates pausedSubs =
  let formatDateString = \({ startDate, endDate }) -> do
        startString <- formatDate startDate
        endString   <- formatDate endDate
        Just $ "Uppehåll: " <> startString <> " – " <> endString
  in mapMaybe formatDateString pausedSubs
