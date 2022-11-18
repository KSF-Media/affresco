module MittKonto.Main.UserView where

import Prelude

import Data.Array (concatMap, snoc, sortBy, (:))
import Data.Either (Either(..))
import Data.Maybe (Maybe(..), maybe, fromMaybe)
import Data.Map as Map
import Data.Nullable as Nullable
import Data.String (contains, toUpper)
import Data.String.Pattern (Pattern(..))
import Data.Tuple (uncurry)
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)
import MittKonto.Main.UserView.AccountEdit as AccountEdit
import MittKonto.Main.UserView.IconAction as IconAction
import MittKonto.Main.Elements as Elements
import MittKonto.Main.Helpers as Helpers
import MittKonto.Main.Types as Types
import MittKonto.Main.UserView.Subscription (subscription) as Subscription
import KSF.AsyncWrapper (loadingSpinner)
import KSF.Api.Subscription (isSubscriptionCanceled, isSubscriptionExpired) as Subscription
import KSF.Paper as Paper
import KSF.Sentry as Sentry
import KSF.User (User)
import KSF.User as User
import KSF.User.Cusno as Cusno
import Persona (Newsletter, NewsletterSubscription)
import React.Basic (JSX)
import React.Basic.DOM as DOM
import React.Basic.Events (handler)
import React.Basic.DOM.Events (targetValue, capture_)
import React.Basic.Hooks as React
import Routing.PushState (PushStateInterface)

foreign import images :: { subscribe :: String }
foreign import _encodeURIComponent :: String -> String

-- | User info page with profile info, subscriptions, etc.
userView :: PushStateInterface -> Types.Self -> Sentry.Logger ->  JSX -> ((Types.State -> Types.State) -> Effect Unit) -> User -> JSX
userView router { state: { now, news, activeUserNewsletters, newslettersUpdated } } logger profileComponent setState user = React.fragment
  [ Helpers.classy DOM.div "mitt-konto--column mitt-konto--profile" [ newsView news, profileView ]
  , Helpers.classy DOM.div "mitt-konto--column" [ subscriptionsView, mailinglistView ]
  ]
  where
    updateNewsletters newsletters =
      liftEffect $ setState $ _ { activeUserNewsletters = Just newsletters }
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
        profileComponentBlock = componentBlockContent profileComponent
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
          case sortBy (comparing _.state) user.subs of
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
            componentHeader "Behöver du hjälp?"
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

    mailinglistView =
      componentBlock "Mina nyhetsbrev:" $ [ componentBlockContent $ contents ]
      where
        updateResultMessage :: JSX
        updateResultMessage = case newslettersUpdated of
            Types.NotUpdated -> mempty
            Types.Updating -> mempty
            Types.Updated -> DOM.p_ [ DOM.text "Uppdateringen av prenumerationen på nyhetsbrevet lyckades." ]
            Types.UpdateFailed -> DOM.p_ [ DOM.text "Något gick fel med uppdateringen av prenumerationen på nyhetsbrevet." ]

        contents :: JSX
        contents =
          DOM.div
            { className: "mitt-konto--newsletters"
            , children: case activeUserNewsletters of
                Nothing -> [ loadingSpinner ]
                Just allNewsletters ->
                  [ checkboxes allNewsletters
                  , Elements.break
                  , acceptChangesButton
                  ]
            }

        newsletterCheckbox :: (NewsletterSubscription -> Effect Unit) -> NewsletterSubscription -> JSX
        newsletterCheckbox updateSubs newsletter =
          DOM.dd_
            [ DOM.label_
                [ DOM.input
                    { type: "checkbox"
                    , checked: newsletter.subscribed
                    , onChange: handler targetValue \_ -> updateSubs (newsletter {subscribed = not newsletter.subscribed} )
                    }
                , DOM.text $ newsletter.listName ]]

        replaceMatching :: forall f a b. Functor f => Eq b => (a -> b) -> a -> f a -> f a
        replaceMatching fn replacement arr = map (\existing -> if fn existing == fn replacement then replacement else existing) arr

        paperSection :: Newsletter -> (Newsletter -> Effect Unit) -> Array JSX
        paperSection paperNewsletters updateNewsletter =
          DOM.dt_
            [ DOM.text $ Paper.paperName paperNewsletters.paper
            , DOM.text " "
            , legalLink paperNewsletters.paper
            ]
          : concatMap
              (uncurry
                (\categoryId newsletters ->
                  map (newsletterCheckbox
                        (\replacement ->
                          let subscriptions = replaceMatching _.id replacement newsletters
                              newSubs = Map.insert categoryId subscriptions paperNewsletters.subscriptions
                          in updateNewsletter $ paperNewsletters {subscriptions = newSubs})) newsletters))
              (Map.toUnfoldable paperNewsletters.subscriptions)

        checkboxes :: Array Newsletter -> JSX
        checkboxes allNewsletters =
          DOM.dl_ $
            concatMap
              (\paper -> paperSection paper (\replacement ->
               let replaced = replaceMatching _.listId replacement allNewsletters
               in launchAff_ $ updateNewsletters replaced
              ))
            allNewsletters


        legalLink :: Paper.Paper -> JSX
        legalLink paper = case paper of
          Paper.HBL -> link "https://www.hbl.fi/bruksvillkor"
          Paper.VN -> link "https://www.vastranyland.fi/bruksvillkor"
          Paper.ON -> link "https://www.ostnyland.fi/bruksvillkor"
          _ -> legalLink Paper.HBL
          where
            link :: String -> JSX
            link href = DOM.a { children: [DOM.text " (bruksvillkor)"], href, target: "_blank" }

        acceptChangesButton :: JSX
        acceptChangesButton =
          DOM.div_
            [ DOM.text "Hantera dina nyhetsbrev genom att kryssa för de nyhetsbrev du vill få och tryck på Spara. Genom att beställa godkänner du KSF Medias bruksvillkor."
            , DOM.br {}
            , DOM.button
                { className: "button-green newsletters--update-submit"
                , onClick: capture_ $ do
                    liftEffect $ setState $ _ { newslettersUpdated = Types.Updating }
                    launchAff_ $ do
                      res <- User.updateUserNewsletters user.uuid $ fromMaybe [] activeUserNewsletters
                      case res of
                        Left _ -> liftEffect $ setState $ _ { newslettersUpdated = Types.UpdateFailed }
                        Right _ -> liftEffect $ setState $ _ { newslettersUpdated = Types.Updated }
                , disabled: newslettersUpdated == Types.Updating
                , children:
                  [ DOM.div_ []
                  , DOM.text "Spara"
                  , DOM.div_ [ if newslettersUpdated == Types.Updating then loadingSpinner else mempty ]
                  ]
                }
            , updateResultMessage
            ]

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
