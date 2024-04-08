module MittKonto.Components.User where

import Prelude

import Data.Array (sortBy)
import Data.Foldable (fold)
import MittKonto.Components.Subscription (component) as Subscription
import MittKonto.Main.Helpers as Helpers
import MittKonto.Main.Types as Types
import KSF.Api.Subscription (isSubscriptionCanceled, isSubscriptionExpired) as Subscription
import KSF.Sentry as Sentry
import React.Basic.DOM as DOM
import React.Basic.Hooks as React
import React.Basic.Hooks (Component)
import Routing.PushState (PushStateInterface)

-- | User info page with profile info, subscriptions, etc.
component :: PushStateInterface -> Sentry.Logger ->  Component Types.UserView
component router logger = do
  subscriptionComponent <- Subscription.component
  React.component "UserView" \{ state: { now }, setState, user } -> React.do
    let subscriptionView subscription =
          subscriptionComponent { subscription, user, logger, now, router, updateWindow: \w -> setState _ {window = w} }
        subscriptionsView =
          Helpers.componentBlock "Mina prenumerationer:" $ subscriptions
          where
            subscriptions =
              -- Sort the canceled subscriptions to the end of the list
              case sortBy (comparing _.state) user.subs of
                []   -> [ Helpers.componentBlockContent "" noSubscriptionsText ]
                subs -> map subscriptionComponentBlockContent subs
            subscriptionComponentBlockContent subscription
              -- If the subscription has a canceled state, we want to add extra css to it.
              | Subscription.isSubscriptionCanceled subscription =
                Helpers.componentBlockContent " mitt-konto--canceled-subscription" $
                subscriptionView subscription
              | Subscription.isSubscriptionExpired subscription now =
                Helpers.componentBlockContent " mitt-konto--expired-subscription" $
                subscriptionView subscription
              | otherwise = Helpers.componentBlockContent "" $ subscriptionView subscription

    pure $ fold
      [ Helpers.classy DOM.div "mitt-konto--column" [ subscriptionsView ]
      ]

  where
    noSubscriptionsText =
      Helpers.classy DOM.div "mitt-konto--no-subscriptions"
        [ DOM.p_
            [ DOM.text "Har du redan en prenumeration? Kontakta vår "
            , Helpers.anchor "https://www.hbl.fi/kundservice/" "kundtjänst" []
            , DOM.text " och vi kopplar den till ditt konto."
            ]
        ]
