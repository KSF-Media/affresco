module MittKonto.Main.UserView where

import Prelude

import Data.Array (all, snoc, sortBy, (:))
import Data.Maybe (Maybe(..))
import Data.String (toUpper)
import MittKonto.Main.UserView.AccountEdit as AccountEdit
import MittKonto.Main.UserView.IconAction as IconAction
import MittKonto.Main.Elements as Elements
import MittKonto.Main.Helpers as Helpers
import MittKonto.Main.Types as Types
import MittKonto.Main.UserView.Subscription (subscription) as Subscription
import MittKonto.Main.UserView.Subscription.Types (RenewSubscription)
import KSF.Api.Subscription (isSubscriptionCanceled, isSubscriptionExpired) as Subscription
import KSF.Profile.Component as Profile
import KSF.Sentry as Sentry
import React.Basic (JSX)
import React.Basic.DOM as DOM
import React.Basic.Hooks as React
import React.Basic.Hooks (Component, component, useState', (/\))

foreign import images :: { subscribe :: String }

-- | User info page with profile info, subscriptions, etc.
userView :: Sentry.Logger -> (RenewSubscription -> JSX) -> Component Types.UserView
userView logger renewSubscription = do
  subscriptionComponent <- Subscription.subscription
  component "UserView" \{ state: { now, news }, setState, user } -> React.do
    renewingSubscription /\ setRenewingSubscription <- useState' Nothing
    let componentHeader title =
          Helpers.classy DOM.span "mitt-konto--component-heading" [ DOM.text $ toUpper title ]

        profileView =
          componentBlock
            "Mina uppgifter:"
              [ profileComponentBlock
              , Elements.break
              , editAccountBlock
              , needHelp
              , Elements.disappearingBreak
              ]
          where
            profileComponentBlock = componentBlockContent "" $ Profile.profile
              { profile: user
              , onUpdate: setState <<< Types.setActiveUser <<< Just
              , logger
              }
            editAccountBlock = DOM.div
              { className: "mitt-konto--edit-account"
              , children:
                  [ componentHeader "Mina inställningar:"
                  , componentBlockContent "" $ AccountEdit.accountEdit
                  ]
              }

        subscriptionView subscription =
          subscriptionComponent { subscription, user, logger, now, renewSubscription, setRenewingSubscription }

        subscriptionsView =
          componentBlock "Mina prenumerationer:" $ subscriptions <> [ Elements.break, subscribeImage ]
          where
            renewing subscription = if Just subscription.subsno == renewingSubscription then " renewing" else ""
            subscriptions =
              -- Sort the canceled subscriptions to the end of the list
              case sortBy (comparing _.state) user.subs of
                []   -> [ componentBlockContent "" noSubscriptionsText ]
                subs ->
                  (if all (\s -> Subscription.isSubscriptionCanceled s ||
                                 Subscription.isSubscriptionExpired s now) subs
                   then identity else flip snoc cancelSubscription) $
                    map subscriptionComponentBlockContent subs
            subscriptionComponentBlockContent subscription
              -- If the subscription has a canceled state, we want to add extra css to it.
              | Subscription.isSubscriptionCanceled subscription =
                componentBlockContent (" mitt-konto--canceled-subscription" <> renewing subscription) $
                subscriptionView subscription
              | Subscription.isSubscriptionExpired subscription now =
                componentBlockContent (" mitt-konto--expired-subscription" <> renewing subscription) $
                subscriptionView subscription
              | otherwise = componentBlockContent "" $ subscriptionView subscription

        cancelSubscription =
          DOM.div
            { className: "mt2"
            , children:
                [ IconAction.iconAction
                    { iconClassName: "mitt-konto--cancel-subscription-icon"
                    , description: "Avsluta din prenumeration"
                    , onClick: IconAction.Href "https://ksfmedia1.typeform.com/to/zbh3kU"
                    }
                ]
            }

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
                componentHeader "Behöver du hjälp?"
                : frequentIssues
            }
          where
            frequentIssues =
              [ DOM.dl_
                  [ DOM.dt_ [ DOM.text "Frågor och svar" ]
                  , DOM.dd_ [ issueLink "HBL" "https://www.hbl.fi/fragor-och-svar/" ]
                  , DOM.dd_ [ issueLink "Västra Nyland" "https://www.vastranyland.fi/fragor-och-svar/" ]
                  , DOM.dd_ [ issueLink "Östnyland" "https://www.ostnyland.fi/fragor-och-svar/" ]
                  ]
              ]
            issueLink description href =
              DOM.a
                { children: [ DOM.text description ]
                , href
                , target: "_blank"
                }

        newsView Nothing = mempty
        newsView (Just n) =
          componentBlock "Nyheter:" [ componentBlockContent "" n, Elements.break ]

        componentBlock headerText content =
          DOM.div
            { className: "mitt-konto--component-block-container"
            , children:
                componentHeader headerText
                : content
            }

        componentBlockContent extraClasses child =
          DOM.div
            { className: "mitt-konto--component-block-content" <> extraClasses
            , children: [ child ]
            }

    pure $ React.fragment
      [ Helpers.classy DOM.div "col col-12 md-col-6 lg-col-6 mitt-konto--profile" [ newsView news, profileView ]
      , Helpers.classy DOM.div "col col-12 md-col-6 lg-col-6" [ subscriptionsView ]
      ]
