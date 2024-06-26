module MittKonto.Main where

import Prelude

import Data.Either (Either(..), either, hush, isLeft)
import Data.Foldable (find, foldMap)
import Data.Maybe (Maybe(..), fromMaybe, isJust, isNothing, maybe)
import Data.Monoid (guard)
import Data.Time.Duration (class Duration, Days(..), convertDuration)
import Effect (Effect)
import Effect.Aff as Aff
import Effect.Class (liftEffect)
import Effect.Class.Console as Console
import Effect.Exception (error)
import Effect.Now as Now
import Effect.Unsafe (unsafePerformEffect)
import KSF.Alert as Alert
import KSF.Api (AuthScope(..))
import KSF.Api.Subscription (SubscriptionPaymentMethod(CreditCard))
import KSF.News as News
import KSF.Paper (Paper(..))
import KSF.Password.Reset as Reset
import KSF.Sentry as Sentry
import KSF.Spinner as Spinner
import KSF.Timeout as Timeout
import KSF.Tracking as Tracking
import KSF.User as User
import KSF.User.Login as Login
import Foreign (unsafeToForeign)
import MittKonto.Components.CreditCard as Components.CreditCard
import MittKonto.Components.Paywall as Components.Paywall
import MittKonto.Components.User as Components.User
import MittKonto.Main.Elements as Elements
import MittKonto.Main.Helpers as Helpers
import MittKonto.Main.Types as Types
import MittKonto.Main.Views (alertView, footerView, loginView, creditCardCallbackView, navbarWrapper) as Views
import MittKonto.Payment.PaymentAccordion as PaymentAccordion
import MittKonto.Payment.PaymentDetail as PaymentDetail
import MittKonto.Payment.Types as Payments
import MittKonto.Routes (MittKontoRoute(..), needsLogin, routes, creditCardCallbackParams)
import MittKonto.Search as Search
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
  Console.log $ "Initial path " <> fullPath
  initialRoute <- maybe (router.pushState (unsafeToForeign {}) "/" *> pure MittKonto) pure $
                  hush $ routeParse fullPath
  sentryDsn <- sentryDsn_
  logger <- Sentry.mkLogger sentryDsn Nothing "mitt-konto"
  search <- Search.search
  payments <- Wrappers.routeWrapper router PaymentAccordion.paymentAccordion
  paymentDetail <- Wrappers.routeWrapper router PaymentDetail.paymentDetail
  creditCardUpdate <- Wrappers.routeWrapper router $ Components.CreditCard.component $
                      creditCardCallbackParams initialRoute
  now <- Now.nowDate
  loginComponent <- Login.login
  timeout <- Timeout.newTimer
  userComponent <- Components.User.component router logger
  paywallComponent <- Components.Paywall.paywall router logger
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
  passwordReset <- Reset.resetPassword
  component "MittKonto" \_ -> React.do
    state /\ setState <- useState initialState
    loggingIn /\ setLoggingIn <- useState' true
    _ <- News.useNews $ \n -> setState _ { news = News.render n }
    isPersonating /\ setPersonating <- useState' false
    route /\ setRoute <- useState' initialRoute
    -- Display a nicer message to a user if they try to navigate back
    -- after changing their password.
    passwordChangeDone /\ setPasswordChangeDone <- useState' false
    cardsChanged /\ setCardsChanged <- useState 0
    useEffectOnce $ pure do
      Aff.launchAff_ $ Timeout.stopTimer timeout
    -- Load on need
    useEffect { user: if isPersonating then Nothing
                      else map _.uuid state.activeUser
              , cardsChanged } do
      setState _ { creditCards = Nothing }
      case (if isPersonating then Nothing else state.activeUser) of
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
          setLoggingIn false
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
                    setLoggingIn false
              Nothing -> setLoggingIn false
      Aff.runAff_ (setState <<< Types.setAlert <<< either Helpers.errorAlert (const Nothing))
                $ Spinner.withSpinner (setState <<< Types.setLoading) attemptMagicLogin
      matchesWith routeParse (const setRoute) router

    let -- The user data in the search results isn't quite complete.
        -- We do another fetch to get it all.
        setActive result = case result of
          Left _ -> setState $ Types.setAlert $ Just
                      { level: Alert.warning
                      , title: "Laddningen misslyckades."
                      , message: "Något gick fel."
                      }
          Right user -> do
            setState $ Types.setActiveUser (Just user) >>> Types.setAlert Nothing
            router.pushState (unsafeToForeign {}) "/"
            setPersonating true

        searchSelect uuid =
          Aff.runAff_
            setActive $ Spinner.withSpinner (setState <<< Types.setLoading)
              $ User.getUser Nothing uuid
        searchView :: JSX
        searchView = search { setActiveUser: searchSelect
                            , router
                            , now
                            }
        usePayments = Helpers.useLoadSpinner setState
                        (isJust state.payments /\ (_.cusno <$> state.activeUser))
                        (Payments.getPayments
                         (if isJust state.payments then Nothing else (_.uuid <$> state.activeUser))
                         (setState <<< Types.setPayments <<< Just))
        paymentProps = { usePayments: usePayments
                       , subscriptionPayments: state.payments
                       , detail: Nothing
                       , router
                       }
        paymentView =
          payments
            { contentProps: paymentProps
            , closeType: Wrappers.Back
            , route: "/fakturor"
            , routeFrom: "/"
            }
        paymentDetailView invno =
          paymentDetail
            { contentProps: paymentProps { detail = Just invno }
            , closeType: Wrappers.Back
            , route: "/fakturor/:invno"
            , routeFrom: "/fakturor"
            }
        creditCardUpdateInputs subsno callback user =
          { subsno
          , callback
          , cusno: user.cusno
          , logger: logger
          , cardsChanged: setCardsChanged \s -> s + 1
          }
        creditCardUpdateView subsno callback user = case state.creditCards of
          Nothing -> Spinner.loadingSpinner
          Just _ -> fromMaybe mempty do
            let subs = find ((_ == subsno) <<< _.subsno) user.subs
            guard ((_.paymentMethod <$> subs) == Just CreditCard) $ pure $
              creditCardUpdate
                { contentProps: creditCardUpdateInputs subsno callback user
                , closeType: Wrappers.XButton
                , route: "/betalkort/uppdatera"
                , routeFrom: "/"
                }
        navToMain = router.pushState (unsafeToForeign {}) "/"
        passwordResetView code = passwordReset { user: state.activeUser
                                               , code
                                               , passwordChangeDone
                                               , setPasswordChangeDone
                                               , navToMain
                                               }
        userView user =
          userComponent
            { user
            , state
            , setState
            }
        paywallView = paywallComponent {}
        userContent = case route of
          MittKonto -> foldMap userView state.activeUser
          Search -> guard state.adminMode searchView
          InvoiceList -> paymentView
          InvoiceDetail invno -> paymentDetailView invno
          PasswordRecovery -> passwordResetView Nothing
          PasswordRecovery2 -> passwordResetView Nothing
          PasswordRecovery3 -> passwordResetView Nothing
          PasswordRecoveryCode code -> passwordResetView $ Just code
          PasswordRecoveryCode2 code -> passwordResetView $ Just code
          CreditCardUpdate subsno -> foldMap (creditCardUpdateView subsno Nothing) state.activeUser
          CreditCardCallback subsno response ->
            maybe
            (Views.creditCardCallbackView loggingIn navToMain)
            (creditCardUpdateView subsno (Just response)) state.activeUser
          Paywall -> paywallView
        content = if isNothing state.activeUser && needsLogin route
                  then Views.loginView { state, setState } (setUser (Nothing :: Maybe Days)) logger
                  else userContent
        navbarView = navbarComponent { state, logout, isPersonating }
    pure $ render state navbarView (foldMap Elements.loadingIndicator state.loading <> content) isPersonating

jsApp :: {} -> JSX
jsApp = unsafePerformEffect app

render
  :: Types.State
  -> JSX
  -> JSX
  -> Boolean
  -> JSX
render state navbar content isPersonating =
  Helpers.classy DOM.div (if isPersonating then "mitt-konto--personating" else "") $
    [ navbar
    , Helpers.classy DOM.div "mitt-konto--main-container-container"
        [ foldMap Views.alertView state.alert
        , Helpers.classy DOM.div "mitt-konto--main-container"
            [ Helpers.classy DOM.div "mitt-konto--container" [ content ]
            ]
        ]
    , Views.footerView
    ]
