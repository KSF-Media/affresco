module KSF.Subscription.Component where

import Prelude

import Data.DateTime (adjust)
import Data.Foldable (foldMap)
import Data.Formatter.DateTime (FormatterCommand(..), format)
import Data.JSDate (JSDate, fromDateTime, toDateTime)
import Data.List (fromFoldable)
import Data.Maybe (Maybe(..), isJust)
import Data.Nullable (Nullable)
import Data.Nullable as Nullable
import Data.String (trim)
import Data.Time.Duration (Days)
import Data.Time.Duration as Time.Duration
import Effect (Effect)
import KSF.DescriptionList.Component as DescriptionList
import KSF.Grid as Grid
import KSF.PauseSubscription.Component as PauseSubscription
import KSF.Subscription.View as View
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
  { pauseSubscriptionVisible :: Maybe Visible }

data Visible = Visible

data Action
  = ShowPauseSubscription (Maybe Visible)

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
  { initialState: { pauseSubscriptionVisible: Nothing }
  , render
  }

update :: Self -> Action -> StateUpdate Props State
update self = case _ of
  ShowPauseSubscription show -> Update $ self.state { pauseSubscriptionVisible = show }

send :: Self -> Action -> Effect Unit
send = runUpdate update

render :: Self -> JSX
render self@{ props } =
  ReactExt.requireStyle
    subscriptionStyles
    $ Grid.row2
      (React.element
         DescriptionList.component
           { definitions:
               [ { term: "Produkt:"
                 , descriptions: [ props.subscription.package.name ]
                 }
               , { term: "Status:"
                 , descriptions: [ translateStatus props.subscription.state ]
                 }
               ]
               <> foldMap billingDateTerm nextBillingDate
           })
      pauseSubscription
      $ Just { extraClasses: [ "subscription--container" ] }
  where
    billingDateTerm date =
      [ { term: "Nästa faktureringsdatum:"
        , descriptions: [ date ]
        }
      ]

    pauseSubscription :: JSX
    pauseSubscription =
      DOM.div
        { className: ""
        , children:
            [ if isJust self.state.pauseSubscriptionVisible
              then pauseSubscriptionComponent
              else pauseIcon
            ]
        }

    pauseSubscriptionComponent =
        PauseSubscription.pauseSubscription
          { subsno: props.subscription.subsno
          , userUuid: props.user.uuid
          , onCancel: send self $ ShowPauseSubscription Nothing
          }

    pauseIcon =
      DOM.div
        { className: "subscription--pause-container flex"
        , children:
            [ DOM.div
                { className: "subscription--pause-icon circle"
                , onClick: togglePauseView
                }
            , DOM.span
                { className: "subscription--pause-text"
                , children:
                    [ DOM.u_ [ DOM.text "Gör uppehåll" ] ]
                , onClick: togglePauseView
                }
            ]
         }
        where
          togglePauseView = handler_ $ send self
            $ case self.state.pauseSubscriptionVisible of
                Nothing -> ShowPauseSubscription $ Just Visible
                _       -> ShowPauseSubscription Nothing

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
