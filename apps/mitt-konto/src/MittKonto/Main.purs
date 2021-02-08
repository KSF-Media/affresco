module MittKonto.Main where

import Prelude

import Data.Either (Either(..))
import Data.Foldable (foldMap)
import Data.Maybe (Maybe(..), fromMaybe, isJust)
import Effect (Effect)
import Effect.Aff as Aff
import Effect.Unsafe (unsafePerformEffect)
import MittKonto.Main.Elements as Elements
import MittKonto.Main.Helpers as Helpers
import MittKonto.Main.Types as Types
import MittKonto.Main.Views (alertView, backwardLinkView, footerView, loginView, navbarView, userView) as Views
import MittKonto.Main.CreditCardUpdateView (RouteWrapperContentInputs (..)) as CreditCardUpdateView
import MittKonto.Wrappers as Wrappers
import KSF.Alert.Component as Alert
import KSF.Paper (Paper(..))
import KSF.Payment.Types as Payments
import KSF.Payment.PaymentAccordion as PaymentAccordion
import KSF.Payment.PaymentDetail as PaymentDetail
import KSF.Search as Search
import KSF.Sentry as Sentry
import KSF.Spinner as Spinner
import KSF.User as User
import React.Basic (JSX)
import React.Basic.DOM as DOM
import React.Basic.Hooks (Component, component, useState, useState', (/\))
import React.Basic.Hooks as React
import React.Basic.Router as Router

foreign import sentryDsn_ :: Effect String

app :: Component {}
app = do
  sentryDsn <- sentryDsn_
  logger <- Sentry.mkLogger sentryDsn Nothing "mitt-konto"
  search <- Search.search
  payments <- Views.backwardLinkView "PaymentView" "/" PaymentAccordion.paymentAccordion
  paymentDetail <- Views.backwardLinkView "PaymentDetailView" "/fakturor" PaymentDetail.paymentDetail
  routeWrapper <- Wrappers.routeWrapper (\(CreditCardUpdateView.RouteWrapperContentInputs inputs) -> inputs.creditCards)
  let initialState =
        { paper: KSF
        , adminMode: false
        , activeUser: Nothing
        , loading: Nothing
        , showWelcome: true
        , alert: Nothing
        , payments: Nothing
        }
  component "MittKonto" \_ -> React.do
    state /\ setState <- useState initialState
    isPersonating /\ setPersonating <- useState' false
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
        searchSelect user =
          Aff.runAff_
            setActive $ Spinner.withSpinner (setState <<< Types.setLoading)
              $ User.getUser Nothing user.uuid
        searchView :: JSX
        searchView = search { setActiveUser: searchSelect }
        usePayments = Helpers.useLoadSpinner setState
                        (isJust state.payments /\ (_.cusno <$> state.activeUser))
                        (Payments.getPayments
                         (if isJust state.payments then Nothing else (_.uuid <$> state.activeUser))
                         (setState <<< Types.setPayments <<< Just))
        paymentProps = { usePayments: usePayments
                       , subscriptionPayments: state.payments
                       }
        paymentView = payments paymentProps
        paymentDetailView = paymentDetail paymentProps
        creditCardUpdateInputs =
          CreditCardUpdateView.RouteWrapperContentInputs
            { creditCards: fromMaybe mempty $ state.activeUser <#> _.creditCards
            , logger: logger
            }
        updateCreditCardRoute =
          Router.route
            { exact: true
            , path: Just "/kreditkort/uppdatera"
            , render: const $
                routeWrapper
                  { content: creditCardUpdateInputs
                  , closeType: Wrappers.XButton
                  , route: "/kreditkort/uppdatera"
                  , routeFrom: "/"
                  }
            }

    pure $ render self logger searchView paymentView paymentDetailView updateCreditCardRoute isPersonating

jsApp :: {} -> JSX
jsApp = unsafePerformEffect app

render :: Types.Self -> Sentry.Logger -> JSX -> JSX -> JSX -> JSX -> Boolean -> JSX
render self@{ state, setState } logger searchView paymentView paymentDetailView updateCreditCardRoute isPersonating =
  Helpers.classy DOM.div (if isPersonating then "mitt-konto--personating" else "")
    [ Views.navbarView self logger isPersonating
    , Helpers.classy DOM.div "mt3 mb4 clearfix"
        [ foldMap Views.alertView state.alert
        , Helpers.classy DOM.div "mitt-konto--main-container col-10 lg-col-7 mx-auto"
            [ Router.switch { children: routes } ]
        ]
    , Views.footerView
    ]
 where
   routes =
     [ defaultRouteElement "/fakturor/:invno" $ const paymentDetailView
     , defaultRouteElement "/fakturor" $ const paymentView
     , routeElement true false (Just "/sök") $ const searchView
     , defaultRouteElement "/" $ Views.userView self logger
     , updateCreditCardRoute
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
               _ -> Views.loginView self logger
           ]
       }
   noMatchRoute =
     Router.redirect
       { to: { pathname: "/"
             , state: {}
             }
       , from: "/*"
       , push: true
       }
