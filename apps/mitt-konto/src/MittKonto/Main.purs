module MittKonto.Main where

import Prelude

import Data.Either (Either(..), either)
import Data.Foldable (foldMap)
import Data.Maybe (Maybe(..), fromMaybe, isJust)
import Effect (Effect)
import Effect.Aff as Aff
import Effect.Now as Now
import Effect.Unsafe (unsafePerformEffect)
import KSF.Alert.Component as Alert
import KSF.News as News
import KSF.Paper (Paper(..))
import KSF.Password.Reset as Reset
import KSF.Search as Search
import KSF.Sentry as Sentry
import KSF.Spinner as Spinner
import KSF.Tracking as Tracking
import KSF.User as User
import KSF.User.Login as Login
import MittKonto.Main.CreditCardUpdateView (creditCardUpdateView) as CreditCardUpdateView
import MittKonto.Main.Elements as Elements
import MittKonto.Main.Helpers as Helpers
import MittKonto.Main.Types as Types
import MittKonto.Main.Views (alertView, footerView, loginView, navbarView, userView) as Views
import MittKonto.Payment.PaymentAccordion as PaymentAccordion
import MittKonto.Payment.PaymentDetail as PaymentDetail
import MittKonto.Payment.Types as Payments
import MittKonto.Wrappers as Wrappers
import React.Basic (JSX)
import React.Basic.DOM as DOM
import React.Basic.Hooks (Component, component, useEffectOnce, useState, useState', (/\))
import React.Basic.Hooks as React
import React.Basic.Router as Router
import Web.HTML (window) as HTML
import Web.HTML.Location (hash) as HTML
import Web.HTML.Window (location) as HTML

foreign import sentryDsn_ :: Effect String

type ViewComponents =
  { searchView :: JSX
  , paymentView :: JSX
  , paymentDetailView :: JSX
  , creditCardUpdateView :: User.User -> JSX
  , passwordResetView :: JSX
  }

app :: Component {}
app = do
  sentryDsn <- sentryDsn_
  logger <- Sentry.mkLogger sentryDsn Nothing "mitt-konto"
  search <- Search.search
  payments <- Wrappers.routeWrapper PaymentAccordion.paymentAccordion
  paymentDetail <- Wrappers.routeWrapper PaymentDetail.paymentDetail
  creditCardUpdate <- Wrappers.routeWrapper CreditCardUpdateView.creditCardUpdateView
  now <- Now.nowDate
  loginComponent <- Login.login
  location <- HTML.location =<< HTML.window
  initialHash <- HTML.hash location
  let initialState =
        { paper: KSF
        , adminMode: false
        , activeUser: Nothing
        -- Let's show the spinner while we try to magically login the user
        , loading: Just Spinner.Loading
        , showWelcome: true
        , alert: Nothing
        , payments: Nothing
        , now: now
        , news: News.render Nothing
        , loginComponent
        }
  passwordReset <- Reset.resetPassword location
  component "MittKonto" \_ -> React.do
    state /\ setState <- useState initialState
    _ <- News.useNews $ \n -> setState _ { news = News.render n }
    isPersonating /\ setPersonating <- useState' false

    useEffectOnce do
      let attemptMagicLogin =
            User.magicLogin Nothing \userResponse ->
              case userResponse of
                Right user -> do
                  Tracking.login (Just user.cusno) "magic login" "success"
                  setUser { state, setState } logger user
                _ -> pure unit
      Aff.runAff_ (setState <<< Types.setAlert <<< either Helpers.errorAlert (const Nothing))
                $ Spinner.withSpinner (setState <<< Types.setLoading) attemptMagicLogin
      pure mempty

    let self = { state, setState }
        -- The user data in the search results isn't quite complete.
        -- We do another fetch to get it all.
        setActive result = case result of
          Left _ -> setState $ Types.setAlert $ Just
                      { level: Alert.warning
                      , title: "Laddningen misslyckades."
                      , message: "Något gick fel."
                      }
          Right user -> do
            setState $ Types.setActiveUser $ Just user
            setPersonating true

        searchSelect uuid =
          Aff.runAff_
            setActive $ Spinner.withSpinner (setState <<< Types.setLoading)
              $ User.getUser Nothing uuid
        searchView :: JSX
        searchView = search { setActiveUser: searchSelect
                            , now
                            }
        usePayments = Helpers.useLoadSpinner setState
                        (isJust state.payments /\ (_.cusno <$> state.activeUser))
                        (Payments.getPayments
                         (if isJust state.payments then Nothing else (_.uuid <$> state.activeUser))
                         (setState <<< Types.setPayments <<< Just))
        paymentProps = { usePayments: usePayments
                       , subscriptionPayments: state.payments
                       }
        paymentView =
          payments
            { contentProps: paymentProps
            , closeType: Wrappers.Back
            , route: "/fakturor"
            , routeFrom: "/"
            }
        paymentDetailView =
          paymentDetail
            { contentProps: paymentProps
            , closeType: Wrappers.Back
            , route: "/fakturor/:invno"
            , routeFrom: "/fakturor"
            }
        creditCardUpdateInputs user =
          { creditCards: fromMaybe mempty $ state.activeUser <#> _.creditCards
          , cusno: user.cusno
          , logger: logger
          }
        creditCardUpdateView user =
          creditCardUpdate
            { contentProps: creditCardUpdateInputs user
            , closeType: Wrappers.XButton
            , route: "/kreditkort/uppdatera"
            , routeFrom: "/"
            }
        passwordResetView = passwordReset { user: state.activeUser }
        components =
          { searchView
          , paymentView
          , paymentDetailView
          , creditCardUpdateView
          , passwordResetView
          }

    pure $ render self logger components initialHash isPersonating

jsApp :: {} -> JSX
jsApp = unsafePerformEffect app

setUser :: Types.Self -> Sentry.Logger -> User.User -> Effect Unit
setUser self logger user = do
  admin <- User.isAdminUser
  self.setState $ (Types.setActiveUser $ Just user) <<< (_ { adminMode = admin } )
  logger.setUser $ Just user

render :: Types.Self -> Sentry.Logger -> ViewComponents -> String -> Boolean -> JSX
render self@{ state } logger components initialHash isPersonating =
  Helpers.classy DOM.div (if isPersonating then "mitt-konto--personating" else "")
    [ Views.navbarView self logger isPersonating
    , Helpers.classy DOM.div "mt3 mb4 clearfix"
        [ foldMap Views.alertView state.alert
        , Helpers.classy DOM.div "mitt-konto--main-container col-10 lg-col-7 mx-auto"
            [ case initialHash of
                 "#l%C3%B6senord" -> components.passwordResetView
                 _ -> Router.switch { children: routes }
            ]
        ]
    , Views.footerView
    ]
 where
   routes =
     [ defaultRouteElement "/fakturor/:invno" $ const components.paymentDetailView
     , defaultRouteElement "/fakturor" $ const components.paymentView
     , routeElement true false (Just "/sök") $ const components.searchView
     , simpleRoute "/#lösenord" components.passwordResetView
     , defaultRouteElement "/" $ Views.userView self logger
     , defaultRouteElement "/prenumerationer/:subsno/kreditkort/uppdatera" components.creditCardUpdateView
     , noMatchRoute
     ]
   defaultRouteElement = routeElement true true <<< Just
   routeElement exact allowAll path view =
     Router.route
       { exact: exact
       , path: path
       , render: const $ Helpers.classy DOM.div "mitt-konto--container clearfix"
           [ foldMap Elements.loadingIndicator state.loading
           , case state.activeUser /\ (state.adminMode || allowAll) of
               Just user /\ true -> view user
               _ -> Views.loginView self (setUser self logger) logger
           ]
       }
   simpleRoute path view =
     Router.route
       { exact: true
       , path: Just path
       , render: const view
       }
   noMatchRoute =
     Router.redirect
       { to: { pathname: "/"
             , state: {}
             }
       , from: "/*"
       , push: true
       }
