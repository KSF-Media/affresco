module KSF.Subscription.Component where

import Prelude

import Data.DateTime (adjust)
import Data.Foldable (foldMap)
import Data.Formatter.DateTime (FormatterCommand(..), format)
import Data.JSDate (JSDate, fromDateTime, toDateTime)
import Data.List (fromFoldable)
import Data.Maybe (Maybe(..))
import Data.Nullable (Nullable)
import Data.Nullable as Nullable
import Data.String (trim)
import Data.Time.Duration (Days)
import Data.Time.Duration as Time.Duration
import Effect (Effect)
import KSF.DescriptionList.Component as DescriptionList
import KSF.PauseSubscription.Component as PauseSubscription
import KSF.Subscription.View as View
import KSF.Grid as Grid
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
  { subscription :: Persona.Subscription }

type State =
  { pauseSubscription :: Boolean }

data Action
  = ShowPauseSubscription Boolean

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
  { initialState: { pauseSubscription: false }
  , render
  }

update :: Self -> Action -> StateUpdate Props State
update self = case _ of
  ShowPauseSubscription show -> Update $ self.state { pauseSubscription = show }

send :: Self -> Action -> Effect Unit
send = runUpdate update

render :: Self -> JSX
render self@{ props } =
  Grid.row_
       [ ReactExt.requireStyle
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
          Nothing
       ]

  where
    billingDateTerm date =
      [ { term: "Nästa faktureringsdatum:"
        , descriptions: [ date ]
        }
      ]

    pauseSubscription :: JSX
    pauseSubscription =
      DOM.div
        { className: "mt2"
        , children:
            [ if self.state.pauseSubscription
              then pauseSubscriptionComponent
              else pauseIcon
            ]
        }

    pauseSubscriptionComponent =
        PauseSubscription.pauseSubscription
          { subsno: props.subscription.subsno
          , onCancel: send self $ ShowPauseSubscription false
          }

    pauseIcon =
      DOM.div
        { className: "pause-subscription--pause-container flex"
        , children:
            [ DOM.div
                { className: "pause-subscription--pause-icon circle"
                , onClick: togglePauseView
                }
            , DOM.span
                { className: "pause-subscription--pause-text"
                , children:
                    [ DOM.u_ [ DOM.text "Gör uppehåll" ] ]
                , onClick: togglePauseView
                }
            ]
         }
        where
          togglePauseView = handler_ $ send self
            $ case self.state.pauseSubscription of
                false -> ShowPauseSubscription true
                true  -> ShowPauseSubscription false

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
