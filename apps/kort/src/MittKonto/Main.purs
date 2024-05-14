module MittKonto.Main where

import Prelude

import Data.Either (Either(..), either, hush, isLeft)
import Data.Foldable (find, foldMap)
import Data.Maybe (Maybe(..), fromMaybe, isNothing, maybe)
import Data.Monoid (guard)
import Data.Time.Duration (class Duration, Days(..), convertDuration)
import Effect (Effect)
import Effect.Aff as Aff
import Effect.Class (liftEffect)
import Effect.Class.Console as Console
import Effect.Exception (error)
import Effect.Now as Now
import Effect.Unsafe (unsafePerformEffect)
import KSF.Api (AuthScope(..))
import KSF.Api.Subscription (SubscriptionPaymentMethod(CreditCard))
import KSF.News as News
import KSF.Paper (Paper(..))
import KSF.Sentry as Sentry
import KSF.Spinner as Spinner
import KSF.Timeout as Timeout
import KSF.Tracking as Tracking
import KSF.User as User
import KSF.User.Login as Login
import Foreign (unsafeToForeign)
import MittKonto.Components.User as Components.User
import MittKonto.Main.CreditCardUpdateView (creditCardUpdateView) as CreditCardUpdateView
import MittKonto.Main.Elements as Elements
import MittKonto.Main.Helpers as Helpers
import MittKonto.Main.Types as Types
import MittKonto.Main.Views (alertView, footerView, loginView, navbarWrapper) as Views
import MittKonto.Routes (MittKontoRoute(..), routes)
import MittKonto.Wrappers as Wrappers
import React.Basic (JSX)
import React.Basic.DOM as DOM
import React.Basic.Hooks (Component, component, useEffect, useEffectOnce, useState, useState', (/\))
import React.Basic.Hooks as React
import Routing.PushState (matchesWith, makeInterface)
import Routing.Duplex as Duplex

foreign import sentryDsn_ :: Effect String

app :: Component {}
app = do
  router <- makeInterface
  locationState <- router.locationState
  let fullPath = locationState.pathname <> locationState.search <> locationState.hash
      routeParse = Duplex.parse routes
  initialRoute <- maybe (router.pushState (unsafeToForeign {}) "/" *> pure MittKonto) pure $
                  hush $ routeParse fullPath
  sentryDsn <- sentryDsn_
  logger <- Sentry.mkLogger sentryDsn Nothing "mitt-konto"
  creditCardUpdate <- Wrappers.routeWrapper router CreditCardUpdateView.creditCardUpdateView
  now <- Now.nowDate
  loginComponent <- Login.login
  timeout <- Timeout.newTimer
  userComponent <- Components.User.component router logger
  navbarComponent <- Views.navbarWrapper router

  let initialState =
        { paper: KSF
        , adminMode: false
        , activeUser: Nothing
        , creditCards: Nothing
        -- Let's show the spinner while we try to magically login the user
        , loading: Just Spinner.Loading
        , showWelcome: true
        , alert: Nothing
        , payments: Nothing
        , now: now
        , news: News.render Nothing
        , loginComponent
        , window: Nothing
        }
  component "MittKonto" \_ -> React.do
    state /\ setState <- useState initialState
    route /\ setRoute <- useState' initialRoute
    cardsChanged /\ setCardsChanged <- useState 0
    useEffectOnce $ pure do
      Aff.launchAff_ $ Timeout.stopTimer timeout
    -- Load on need
    useEffect { user: map _.uuid state.activeUser
              , cardsChanged } do
      setState _ { creditCards = Nothing }
      case state.activeUser of
        Nothing -> pure mempty
        Just _ -> do
          fiber <- Aff.launchAff do
            cards <- User.getCreditCards
            liftEffect $ setState _ { creditCards = Just cards }
          pure $ Aff.launchAff_ $ Aff.killFiber (error "cancel") fiber

    let logout = do
          router.pushState (unsafeToForeign {}) "/"
          Aff.launchAff_ $ Spinner.withSpinner (setState <<< Types.setLoading) do
            User.logout \logoutResponse -> when (isLeft logoutResponse) $ Console.error "Logout failed"
            liftEffect do
              logger.setUser Nothing
              setState $ Types.setActiveUser Nothing
        setUser :: forall a. Duration a => Maybe a -> User.User -> Effect Unit
        setUser maybeDuration user = do
          let duration = fromMaybe (convertDuration $ Days 1.0) maybeDuration
          admin <- User.isAdminUser
          setState $ (Types.setActiveUser $ Just user) <<< (_ { adminMode = admin } )
          logger.setUser $ Just user
          Aff.launchAff_ $ Timeout.startTimer duration timeout do
            liftEffect logout

    useEffectOnce do
      let attemptMagicLogin = do
            User.magicLogin Nothing $ hush >>> case _ of
              Just user -> Aff.launchAff_ do
                -- User logged in via old session but check that it
                -- has sufficient scope for using Mitt Konto.
                validScope <- User.hasScope user.uuid UserWrite
                liftEffect $ case validScope of
                  Just _ -> do
                    Tracking.login (Just user.cusno) "magic login" "success"
                    setUser validScope user
                  Nothing -> do
                    pure unit
              Nothing -> pure unit
      Aff.runAff_ (setState <<< Types.setAlert <<< either Helpers.errorAlert (const Nothing))
                $ Spinner.withSpinner (setState <<< Types.setLoading) attemptMagicLogin
      matchesWith routeParse (const setRoute) router

    let creditCardUpdateInputs window creditCard user =
          { creditCard: creditCard
          , cusno: user.cusno
          , logger: logger
          , window: window
          , cardsChanged: setCardsChanged \s -> s + 1
          }
        creditCardUpdateView subsno creditCardId user = case state.creditCards of
          Nothing -> Spinner.loadingSpinner
          Just (Left err) -> fromMaybe mempty do
            w <- state.window
            pure $ creditCardUpdate
              { contentProps: creditCardUpdateInputs w (Left err) user
              , closeType: Wrappers.XButton
              , route: "/betalkort/uppdatera"
              , routeFrom: "/"
              }
          Just (Right cards) -> fromMaybe mempty do
            card <- find ((_ == creditCardId) <<< _.id) cards
            subs <- find ((_ == subsno) <<< _.subsno) user.subs
            w <- state.window
            guard (subs.paymentMethod == CreditCard) $ pure $
              creditCardUpdate
                { contentProps: creditCardUpdateInputs w (Right card) user
                , closeType: Wrappers.XButton
                , route: "/betalkort/uppdatera"
                , routeFrom: "/"
                }
        userView user =
          userComponent
            { user
            , state
            , setState
            }
        userContent = case route of
          MittKonto -> foldMap userView state.activeUser
          CreditCardUpdate subsno creditCardId -> foldMap (creditCardUpdateView subsno creditCardId) state.activeUser
        content = if isNothing state.activeUser
                  then Views.loginView { state, setState } (setUser (Nothing :: Maybe Days)) logger
                  else userContent
        navbarView = navbarComponent { state, logout, isPersonating: false }
    pure $ render state navbarView (foldMap Elements.loadingIndicator state.loading <> content)

jsApp :: {} -> JSX
jsApp = unsafePerformEffect app

render
  :: Types.State
  -> JSX
  -> JSX
  -> JSX
render state navbar content  =
  DOM.div_
    [ navbar
    , Helpers.classy DOM.div "mitt-konto--main-container-container"
        [ foldMap Views.alertView state.alert
        , Helpers.classy DOM.div "mitt-konto--main-container"
            [ Helpers.classy DOM.div "mitt-konto--container" [ content ]
            ]
        ]
    , Views.footerView
    ]
