module MittKonto.Main where

import Prelude

import Data.Foldable (foldMap)
import Data.Maybe (Maybe(..))
import Data.Nullable (toNullable)
import Effect (Effect)
import Effect.Unsafe (unsafePerformEffect)
import MittKonto.Main.Elements as Elements
import MittKonto.Main.Helpers as Helpers
import MittKonto.Main.Types as Types
import MittKonto.Main.Views (alertView, footerView, loginView, navbarView, paymentView, userView) as Views
import KSF.Paper (Paper(..))
import KSF.Sentry as Sentry
import React.Basic (JSX, element)
import React.Basic.DOM as DOM
import React.Basic.Hooks (Component, component, useState, (/\))
import React.Basic.Hooks as React
import React.Basic.Router as Router

foreign import sentryDsn_ :: Effect String

app :: Component {}
app = do
  sentryDsn <- sentryDsn_
  logger <- Sentry.mkLogger sentryDsn Nothing "mitt-konto"
  let initialState =
        { paper: KSF
        , loggedInUser: Nothing
        , loading: Nothing
        , showWelcome: true
        , alert: Nothing
        , payments: Nothing
        , creditCards: []
        }
  component "MittKonto" \_ -> React.do
    state /\ setState <- useState initialState
    let self = { state, setState }
    pure $ render self logger

jsApp :: {} -> JSX
jsApp = unsafePerformEffect app

render :: Types.Self -> Sentry.Logger -> JSX
render self@{ state, setState } logger =
  React.fragment
    [ Views.navbarView self logger
    , Helpers.classy DOM.div "mt4 mb4"
        [ foldMap Views.alertView state.alert ]
    , Helpers.classy DOM.div "mt4 mb4 clearfix"
        [ Helpers.classy DOM.div "mitt-konto--main-container col-10 lg-col-7 mx-auto"
            [ element Router.switch { children: [ paymentListRoute, mittKontoRoute, noMatchRoute ] } ]
        ]
    , Views.footerView
    ]
 where
   mittKontoRoute =
     element
       Router.route
         { exact: true
         , path: toNullable $ Just "/"
         , render: const mittKontoView
         }
   mittKontoView =
      Helpers.classy DOM.div "mitt-konto--container clearfix"
        [ foldMap Elements.loadingIndicator state.loading
        , case state.loggedInUser of
            Just user -> Views.userView self logger user
            Nothing   -> Views.loginView self logger
        ]
   paymentListRoute =
     element
       Router.route
         { exact: true
         , path: toNullable $ Just "/fakturor"
         , render: const
             $ Helpers.classy DOM.div "mitt-konto--container clearfix"
                 [ foldMap Elements.loadingIndicator state.loading
                 , case state.loggedInUser of
                     Just user -> Views.paymentView self user
                     Nothing   -> Views.loginView self logger
                 ]
         }
   noMatchRoute =
     -- TODO: Use Redirect when supported!
      element
        Router.route
          { exact: false
          , path: toNullable Nothing
          , render: const mittKontoView
          }
