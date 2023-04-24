module MittKonto.Components.User where

import Prelude

import Data.Array (all, snoc, sortBy, (:))
import Data.Foldable (fold)
import Data.Maybe (Maybe(..), maybe)
import Data.Nullable as Nullable
import Data.String (contains)
import Data.String.Pattern (Pattern(..))
import MittKonto.Components.Mailinglists as Mailinglists
import MittKonto.Components.Subscription (component) as Subscription
import MittKonto.Main.UserView.AccountEdit as AccountEdit
import MittKonto.Main.UserView.IconAction as IconAction
import MittKonto.Main.Elements as Elements
import MittKonto.Main.Helpers as Helpers
import MittKonto.Main.Types as Types
import KSF.Api.Subscription (isSubscriptionCanceled, isSubscriptionExpired) as Subscription
import KSF.Profile.Component as Profile
import KSF.Sentry as Sentry
import KSF.User.Cusno as Cusno
import React.Basic (JSX)
import React.Basic.DOM as DOM
import React.Basic.Hooks as React
import React.Basic.Hooks (Component)
import Routing.PushState (PushStateInterface)

foreign import images :: { subscribe :: String }
foreign import _encodeURIComponent :: String -> String

-- | User info page with profile info, subscriptions, etc.
component :: PushStateInterface -> Sentry.Logger ->  Component Types.UserView
component router logger = do
  subscriptionComponent <- Subscription.component
  profile <- Profile.component
  mailinglists <- Mailinglists.component
  React.component "UserView" \{ state: { news, now }, setState, user } -> React.do
    let profileView =
          Helpers.componentBlock
          "Mina uppgifter:"
          [ profileComponentBlock
          , Elements.break
          , editAccountBlock
          , needHelp
          , Elements.disappearingBreak
          ]
          where
            profileComponentBlock = Helpers.componentBlockContent "" $ profile
              { profile: user
              , onUpdate: setState <<< Types.setActiveUser <<< Just
              , logger
              }
            editAccountBlock = DOM.div
              { className: "mitt-konto--edit-account"
              , children:
                  [ Helpers.componentHeader "Mina inställningar:"
                  , Helpers.componentBlockContent "" $ AccountEdit.accountEdit router
                  ]
              }

        subscriptionView subscription =
          subscriptionComponent { subscription, user, logger, now, router, updateWindow: \w -> setState _ {window = w} }
        subscriptionsView =
          Helpers.componentBlock "Mina prenumerationer:" $ subscriptions <> [ Elements.break, subscribeImage ]
          where
            subscriptions =
              -- Sort the canceled subscriptions to the end of the list
              case sortBy (comparing _.state) user.subs of
                []   -> [ Helpers.componentBlockContent "" noSubscriptionsText ]
                subs ->
                  (if all (\s -> Subscription.isSubscriptionCanceled s ||
                                 Subscription.isSubscriptionExpired s now) subs
                   then identity else flip snoc (cancelSubscription user)) $
                    map subscriptionComponentBlockContent subs
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
      [ Helpers.classy DOM.div "mitt-konto--column mitt-konto--profile" [ newsView news, profileView ]
      , Helpers.classy DOM.div "mitt-konto--column" [ subscriptionsView, mailinglists user ]
      ]

  where
    cancelSubscription user =
      DOM.div
        { className: "mitt-konto--cancel-subscription-icon-container"
        , children:
            [ IconAction.iconAction
                { iconClassName: "mitt-konto--cancel-subscription-icon"
                , description: "Avsluta din prenumeration"
                , onClick: IconAction.Href $ "https://form.jotform.com/221793422462051"
                  <> "?kundnummer=" <> Cusno.toString user.cusno
                  <> "&namn=" <> _encodeURIComponent name
                  <> (maybe "" (("&epost=" <> _) <<< _encodeURIComponent) email)
                  <> (maybe "" (("&telefonnummer=" <> _) <<< _encodeURIComponent) $ Nullable.toMaybe user.phone)
                , router
                }
            ]
        }
      where
        -- Jotform has a prefill decode bug which prevents from using
        -- emails with plus signs on them.  "%2B" will be interpreted
        -- as a space, just like "+".  Let's just make the customer
        -- input it if it happens.
        email = if contains (Pattern "+") user.email then Nothing else Just user.email
        name = case Nullable.toMaybe user.firstName, Nullable.toMaybe user.lastName of
                 Just f, Just l -> f <> " " <> l
                 Just f, _ -> f
                 _, Just l -> l
                 _, _ -> ""

    subscribeImage =
      DOM.div
        { className: "mitt-konto--subscribe-image flex"
        , children:
            [ Helpers.anchor "https://prenumerera.ksfmedia.fi/" "" [ DOM.img { src: images.subscribe } ] ]
        }

    noSubscriptionsText =
      Helpers.classy DOM.div "mitt-konto--no-subscriptions"
        [ DOM.p_
            [ DOM.text "Har du redan en prenumeration? Kontakta vår "
            , Helpers.anchor "https://www.hbl.fi/kundservice/" "kundtjänst" []
            , DOM.text " och vi kopplar den till ditt konto."
            ]
        ]

    needHelp :: JSX
    needHelp =
      DOM.div
        { className: "mitt-konto--need-help"
        , children:
            Helpers.componentHeader "Behöver du hjälp?"
            : frequentIssues
        }
      where
        frequentIssues =
          [ DOM.dl_
              [ DOM.dt_ [ DOM.text "Frågor och svar" ]
              , DOM.dd_ [ issueLink "HBL" "https://www.hbl.fi/sida/fragor-och-svar" ]
              , DOM.dd_ [ issueLink "Västra Nyland" "https://www.vastranyland.fi/sida/fragor-och-svar" ]
              , DOM.dd_ [ issueLink "Östnyland" "https://www.ostnyland.fi/sida/fragor-och-svar" ]
              ]
          ]
        issueLink description href =
          DOM.a
            { children: [ DOM.text description ]
            , href
            , target: "_blank"
            }

    newsView :: Maybe JSX -> JSX
    newsView Nothing = mempty
    newsView (Just n) =
      Helpers.componentBlock "Aktuellt:" [ Helpers.componentBlockContent "" n, Elements.break ]
