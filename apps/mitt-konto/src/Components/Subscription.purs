module MittKonto.Components.Subscription where

import Prelude

import Data.Array (concatMap, cons, filter)
import Data.Foldable (foldMap)
import Data.Maybe (Maybe(..))
import Data.Nullable (toMaybe)
import Data.Tuple (Tuple(..))
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
import React.Basic.Hooks as React
import React.Basic.Hooks (Component, useEffectOnce, useMemo, useState, (/\))
import React.Basic.DOM as DOM

component :: Component Types.Props
component = do
  React.component "Subscription" \props -> React.do
    useEffectOnce do
      props.logger.setUser $ Just props.user
      pure $ pure unit
    let initialState =
          { wrapperProgress: AsyncWrapper.Ready
          , pausedSubscriptions: toMaybe props.subscription.paused
          , pendingAddressChanges: toMaybe props.subscription.pendingAddressChanges
          , updateAction: Nothing
          }
    state /\ setState <- useState initialState
    let self = { props, state, setState }
    informationColumn <- useMemo (state.pausedSubscriptions /\ state.pendingAddressChanges) $ const $
                         renderInformationColumn self
    pure $ render self informationColumn

renderInformationColumn :: Types.Self -> JSX
renderInformationColumn self@{ props: { now, subscription: sub@{ package, state } } } =
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
               <> let pausedDates = foldMap filterExpiredPausePeriods self.state.pausedSubscriptions
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
  where
    subsno = Subsno.toString sub.subsno
    filterExpiredPausePeriods :: Array User.PausedSubscription -> Array User.PausedSubscription
    filterExpiredPausePeriods pausedSubs =
      filter (not <<< Helpers.isPeriodExpired true now <<< toMaybe <<< _.endDate) pausedSubs

render :: Types.Self -> JSX -> JSX
render self@{ props: { now, subscription: sub } } informationColumn =
  Grid.row2
    informationColumn
    (if expired then mempty else Elements.subscriptionUpdates self)
    { extraClasses: [ "subscription--container" ]
    , _data: [ Tuple "subsno" subsno ]
    , id: "subscription-" <> subsno
    }
  where
    expired = isSubscriptionExpired sub now
    subsno = Subsno.toString sub.subsno
