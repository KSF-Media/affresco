module MittKonto.Main.UserView.Subscription where

import Prelude

import Data.Array (concatMap, foldMap, filter)
import Data.Array as Array
import Data.Maybe (Maybe(..))
import Data.Nullable (toMaybe)
import Effect (Effect)
import Effect.Now as Now
import KSF.AsyncWrapper as AsyncWrapper
import KSF.DescriptionList.Component as DescriptionList
import KSF.Grid as Grid
import KSF.User as User
import MittKonto.Main.UserView.Subscription.Elements as Elements
import MittKonto.Main.UserView.Subscription.Helpers as Helpers
import MittKonto.Main.UserView.Subscription.Types as Types
import React.Basic (JSX)
import React.Basic.Classic (make)
import React.Basic.Classic as React
import React.Basic.DOM as DOM

component :: React.Component Types.Props
component = React.createComponent "Subscription"

subscription :: Types.Props -> JSX
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

didMount :: Types.Self -> Effect Unit
didMount self = do
  now <- Now.nowDateTime
  self.setState _
    { now = Just now
    , pausedSubscriptions = toMaybe self.props.subscription.paused
    , pendingAddressChanges = toMaybe self.props.subscription.pendingAddressChanges
    }
  self.props.logger.setUser $ Just self.props.user

render :: Types.Self -> JSX
render self@{ props: { subscription: sub@{ package, paymentMethod, state } } } =
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
                   [ DOM.text $ Helpers.translateStatus state ]
                   <> let pausedDates = foldMap (Helpers.showPausedDates <<< filterExpiredPausePeriods) $ self.state.pausedSubscriptions
                          descriptionText = if Array.null pausedDates
                                            then mempty
                                            else Elements.pauseDescription
                       in (map DOM.text pausedDates) <> [ descriptionText ]
               }
             ]
             <> concatMap (\f -> f self) [ Elements.receiverName
                                         , Elements.deliveryAddress
                                         , Elements.pendingAddressChanges
                                         , Elements.billingDateTerm
                                         , Elements.subscriptionEndTerm
                                         , Elements.paymentMethod
                                         ]
         })
      (if package.digitalOnly
       then mempty
       else Elements.subscriptionUpdates self)
      $ Just { extraClasses: [ "subscription--container" ] }
  where
    filterExpiredPausePeriods :: Array User.PausedSubscription -> Array User.PausedSubscription
    filterExpiredPausePeriods pausedSubs =
      case self.state.now of
        Nothing  -> pausedSubs
        Just now -> filter (not Helpers.isPeriodExpired now <<< toMaybe <<< _.endDate) pausedSubs