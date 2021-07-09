module MittKonto.Main.UserView.Subscription where

import Prelude

import Data.Array (concatMap, cons, filter)
import Data.Foldable (foldMap)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Effect (Effect)
import KSF.Api.Subscription (toString) as Subsno
import KSF.Api.Subscription (isSubscriptionExpired)
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
      , updateAction: Nothing
      }
  , render
  , didMount
  }

didMount :: Types.Self -> Effect Unit
didMount self = do
  self.setState _
    { pausedSubscriptions = self.props.subscription.paused
    , pendingAddressChanges = self.props.subscription.pendingAddressChanges
    }
  self.props.logger.setUser $ Just self.props.user

render :: Types.Self -> JSX
render self@{ props: { now, subscription: sub@{ package, state } } } =
  Grid.row2
    (DescriptionList.descriptionList
         { definitions:
             [ { term: "Produkt:"
               , description: DOM.text package.name `cons`
                              (pure <<< DOM.ul_ <<< map (DOM.li_ <<< pure <<< DOM.text))
                              package.info
               }
             , { term: "Pren.nr:"
               , description: [ DOM.text subsno ]
               }
             , { term: "Status:"
               , description:
                   [ DOM.text $ Helpers.translateStatus state ]
                   <> let pausedDates = foldMap filterExpiredPausePeriods $ self.state.pausedSubscriptions
                       in Elements.showPausedDates self pausedDates
               }
             ]
             <> concatMap (\f -> f self) [ Elements.receiverName
                                         , Elements.deliveryAddress
                                         , Elements.pendingAddressChanges
                                         , Elements.billingDateTerm
                                         , Elements.subscriptionEndTerm
                                         , if self.props.user.cusno == sub.paycusno
                                             then Elements.paymentMethod
                                             else mempty
                                         ]
         })
      (if expired then mempty else Elements.subscriptionUpdates self)
      { extraClasses: [ "subscription--container" ]
      , _data: [ Tuple "subsno" subsno ]
      , id: "subscription-" <> subsno
      }
  where
    filterExpiredPausePeriods :: Array User.PausedSubscription -> Array User.PausedSubscription
    filterExpiredPausePeriods pausedSubs =
      filter (not <<< Helpers.isPeriodExpired false now <<< _.endDate) pausedSubs
    expired = isSubscriptionExpired sub now
    subsno = Subsno.toString sub.subsno
