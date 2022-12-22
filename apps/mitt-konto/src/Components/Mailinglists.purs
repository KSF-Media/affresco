module MittKonto.Components.Mailinglists where

import Prelude

import Data.Array (concatMap, (:))
import Data.Either (isRight)
import Data.Foldable (foldMap)
import Data.Maybe (Maybe(..), maybe, fromMaybe)
import Data.Map as Map
import Data.Tuple (uncurry)
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)
import MittKonto.Main.Elements as Elements
import MittKonto.Main.Helpers as Helpers
import MittKonto.Main.Types as Types
import KSF.AsyncWrapper (loadingSpinner)
import KSF.Paper as Paper
import KSF.User (User)
import KSF.User as User
import Persona (Newsletter, NewsletterSubscription)
import React.Basic (JSX)
import React.Basic.DOM as DOM
import React.Basic.Events (handler)
import React.Basic.DOM.Events (targetValue, capture_)
import React.Basic.Hooks as React
import React.Basic.Hooks (Component, useEffectOnce, useState', (/\))

component :: Component User
component = do
  React.component "Mailinglists" $ \user -> React.do
    newslettersUpdated /\ setNewslettersUpdated <- useState' Types.NotUpdated
    activeUserNewsletters /\ setActiveUserNewsletters <- useState' Nothing

    useEffectOnce do
      launchAff_ do
        foldMap (liftEffect <<< setActiveUserNewsletters <<< Just) =<< User.getUserNewsletters user.uuid
      pure $ pure unit

    pure $ Helpers.componentBlock "Mina nyhetsbrev:" $ pure $ Helpers.componentBlockContent "" $
      DOM.div
        { className: "mitt-konto--newsletters"
        , children: flip (maybe [ loadingSpinner ]) activeUserNewsletters $ \allNewsletters ->
            [ checkboxes (setActiveUserNewsletters <<< Just) allNewsletters
            , Elements.break
            , acceptChangesButton user setNewslettersUpdated newslettersUpdated activeUserNewsletters
            ]
        }

  where
    updateResultMessage Types.NotUpdated = mempty
    updateResultMessage Types.Updating = mempty
    updateResultMessage Types.Updated = DOM.p_ [ DOM.text "Uppdateringen av prenumerationen på nyhetsbrevet lyckades." ]
    updateResultMessage Types.UpdateFailed = DOM.p_ [ DOM.text "Något gick fel med uppdateringen av prenumerationen på nyhetsbrevet." ]

    newsletterCheckbox :: (NewsletterSubscription -> Effect Unit) -> NewsletterSubscription -> JSX
    newsletterCheckbox updateSubs newsletter =
      DOM.dd_
        [ DOM.label_
            [ DOM.input
                { type: "checkbox"
                , checked: newsletter.subscribed
                , onChange: handler targetValue $ const $ updateSubs (newsletter { subscribed = not newsletter.subscribed })
                }
            , DOM.text $ newsletter.listName
            ]
        ]

    replaceMatching :: forall a b. Eq b => (a -> b) -> a -> a -> a
    replaceMatching f replacement existing = if f existing == f replacement then replacement else existing

    paperSection :: (Newsletter -> Effect Unit) -> Newsletter -> Array JSX
    paperSection updateNewsletter paperNewsletter =
      DOM.dt_
        [ DOM.text $ (Paper.paperName paperNewsletter.paper) <> " "
        , legalLink paperNewsletter.paper
        ]
      : concatMap
          (uncurry
             (\categoryId newsletters ->
               map (newsletterCheckbox
                      (\replacement ->
                        let subscriptions = map (replaceMatching _.id replacement) newsletters
                            newSubs = Map.insert categoryId subscriptions paperNewsletter.subscriptions
                        in updateNewsletter $ paperNewsletter { subscriptions = newSubs})) newsletters))
          (Map.toUnfoldable paperNewsletter.subscriptions)

    --checkboxes :: Array Newsletter -> JSX
    checkboxes updateNewsletters allNewsletters =
      DOM.dl_ $
        concatMap
          (paperSection $ \replacement -> updateNewsletters $
                                          map (replaceMatching _.listId replacement) allNewsletters)
        allNewsletters

    legalLink :: Paper.Paper -> JSX
    legalLink paper = case paper of
      Paper.HBL -> link "https://www.hbl.fi/bruksvillkor"
      Paper.VN -> link "https://www.vastranyland.fi/bruksvillkor"
      Paper.ON -> link "https://www.hbl.fi/bruksvillkor"
      _ -> legalLink Paper.HBL
      where
        link :: String -> JSX
        link href = DOM.a { children: [ DOM.text "(bruksvillkor)"], href, target: "_blank" }

    acceptChangesButton user setNewslettersUpdated newslettersUpdated newsletters =
      DOM.div_
        [ DOM.p_ [ DOM.text "Hantera dina nyhetsbrev genom att kryssa för de nyhetsbrev du vill få och tryck på Spara. Genom att beställa godkänner du KSF Medias bruksvillkor." ]
        , DOM.p_ [ DOM.text "Om du inte tidigare prenumererat på ett nyhetsbrev måste du först bekräfta din e-postadress. Vi skickar ett meddelande till din e-postadress där du kan bekräfta att adressen är rätt." ]
        , DOM.button
            { className: "button-green newsletters--update-submit"
            , onClick: capture_ do
                setNewslettersUpdated Types.Updating
                launchAff_ do
                  res <- User.updateUserNewsletters user.uuid $ fromMaybe [] newsletters
                  liftEffect $ setNewslettersUpdated $ if isRight res then Types.Updated else Types.UpdateFailed
            , disabled: newslettersUpdated == Types.Updating
            , children:
                [ DOM.div_ []
                , DOM.text "Spara"
                , DOM.div_ [ if newslettersUpdated == Types.Updating then loadingSpinner else mempty ]
                ]
            }
        , updateResultMessage newslettersUpdated
        ]
