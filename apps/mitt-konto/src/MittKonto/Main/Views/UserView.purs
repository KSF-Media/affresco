module MittKonto.Main.UserView where

import Prelude

import Data.Array (snoc, sortBy, (:))
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Data.String (toUpper)
import MittKonto.Main.UserView.AccountEdit as AccountEdit
import MittKonto.Main.UserView.IconAction as IconAction
import MittKonto.Main.Elements as Elements
import MittKonto.Main.Helpers as Helpers
import MittKonto.Main.Types as Types
import MittKonto.Main.UserView.Subscription (subscription) as Subscription
import KSF.Api.Subscription (isSubscriptionCanceled, isSubscriptionExpired) as Subscription
import KSF.Profile.Component as Profile
import KSF.Sentry as Sentry
import KSF.User (User)
import React.Basic (JSX)
import React.Basic.DOM as DOM
import React.Basic.Hooks as React
import Routing.PushState (PushStateInterface)

foreign import images :: { subscribe :: String }

-- | User info page with profile info, subscriptions, etc.
userView :: PushStateInterface -> Types.Self -> Sentry.Logger -> User -> JSX
userView router { state: { now, news }, setState } logger user = React.fragment
  [ Helpers.classy DOM.div "mitt-konto--column mitt-konto--profile" [ newsView news, profileView ]
  , Helpers.classy DOM.div "mitt-konto--column" [ subscriptionsView ]
  ]
  where
    componentHeader title =
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
        profileComponentBlock = componentBlockContent $ Profile.profile
          { profile: user
          , onUpdate: setState <<< Types.setActiveUser <<< Just
          , logger
          }
        editAccountBlock = DOM.div
          { className: "mitt-konto--edit-account"
          , children:
              [ componentHeader "Mina inställningar:"
              , componentBlockContent $ AccountEdit.accountEdit router
              ]
          }

    subscriptionsView =
      componentBlock "Mina prenumerationer:" $ subscriptions <> [ Elements.break, subscribeImage ]
      where
        subscriptions =
          -- Sort the canceled subscriptions to the end of the list
          case sortBy (comparing (_.state <<< unwrap)) user.subs of
            []   -> [ componentBlockContent noSubscriptionsText ]
            subs -> do
              map subscriptionComponentBlockContent subs `snoc` cancelSubscription
              where
                subscriptionView subscription = Subscription.subscription { subscription, user, logger, now, router }
                subscriptionComponentBlockContent subscription
                  -- If the subscription has a canceled state, we want to add extra css to it.
                  | Subscription.isSubscriptionCanceled subscription =
                      DOM.div
                        { className: "mitt-konto--canceled-subscription"
                        , children: [ componentBlockContent $ subscriptionView subscription ]
                        }
                  | Subscription.isSubscriptionExpired subscription now =
                      DOM.div
                        { className: "mitt-konto--expired-subscription"
                        , children: [ componentBlockContent $ subscriptionView subscription ]
                        }
                  | otherwise = componentBlockContent $ subscriptionView subscription

    cancelSubscription =
      DOM.div
        { className: "mitt-konto--cancel-subscription-icon-container"
        , children:
            [ IconAction.iconAction
                { iconClassName: "mitt-konto--cancel-subscription-icon"
                , description: "Avsluta din prenumeration"
                , onClick: IconAction.Href "https://ksfmedia1.typeform.com/to/zbh3kU"
                , router
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
      componentBlock "Nyheter:" [ componentBlockContent n, Elements.break ]

    componentBlock headerText content =
      DOM.div
        { className: "mitt-konto--component-block-container"
        , children:
            componentHeader headerText
            : content
        }

    componentBlockContent child =
      DOM.div
        { className: "mitt-konto--component-block-content"
        , children: [ child ]
        }
