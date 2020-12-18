module MittKonto.Main where

import Prelude

import Data.Either (Either(..))
import Data.Foldable (foldMap)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Aff as Aff
import Effect.Unsafe (unsafePerformEffect)
import KSF.AsyncWrapper as AsyncWrapper
import KSF.CreditCard.Update as CreditCard
import KSF.Paper (Paper(..))
import KSF.Sentry as Sentry
import MittKonto.Main.Elements as Elements
import MittKonto.Main.Helpers as Helpers
import MittKonto.Main.Types as Types
import MittKonto.Main.Views (alertView, footerView, loginView, navbarView, paymentView, userView) as Views
import KSF.Alert.Component as Alert
import KSF.Paper (Paper(..))
import KSF.Search as Search
import KSF.Sentry as Sentry
import KSF.Spinner as Spinner
import KSF.User as User
import React.Basic (JSX)
import MittKonto.Wrappers as Wrappers
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
  let initialState =
        { paper: KSF
        , adminMode: false
        , activeUser: Nothing
        , loading: Nothing
        , showWelcome: true
        , alert: Nothing
        , payments: Nothing
        , creditCards: []
        , wrapperProgress: AsyncWrapper.Ready
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
    pure $ render self logger searchView isPersonating

jsApp :: {} -> JSX
jsApp = unsafePerformEffect app

render :: Types.Self -> Sentry.Logger -> JSX -> Boolean -> JSX
render self@{ state, setState } logger searchView isPersonating =
  Helpers.classy DOM.div (if isPersonating then "mitt-konto--personating" else "")
    [ Views.navbarView self logger isPersonating
    , Helpers.classy DOM.div "mt3 mb4 clearfix"
        [ foldMap Views.alertView state.alert
        , Helpers.classy DOM.div "mitt-konto--main-container col-10 lg-col-7 mx-auto"
            [ Router.switch { children: [ paymentListRoute, search, mittKontoRoute, updateCreditCard, noMatchRoute ] } ]
        ]
    , Views.footerView
    ]
 where
   mittKontoRoute =
     Router.route
       { exact: true
       , path: Just "/"
       , render: const mittKontoView
       }
   mittKontoView =
      Helpers.classy DOM.div "mitt-konto--container clearfix"
        [ foldMap Elements.loadingIndicator state.loading
        , case state.activeUser of
            Just user -> Views.userView self logger user
            Nothing   -> Views.loginView self logger
        ]
   paymentListRoute =
     Router.route
       { exact: true
       , path: Just "/fakturor"
       , render: const
           $ Helpers.classy DOM.div "mitt-konto--container clearfix"
               [ foldMap Elements.loadingIndicator state.loading
               , case state.activeUser of
                   Just user -> Views.paymentView self user
                   Nothing   -> Views.loginView self logger
               ]
       }
   updateCreditCard =
      element
        Router.route
          { exact: true
          , path: toNullable $ Just "/kortt/uppdatera"
          , render: \_ -> Wrappers.viewWrapper
              { content: creditCardUpdateComponent
              , wrapperState: state.wrapperProgress
              , closeType: Wrappers.Countdown
              , onTryAgain: pure unit
              }
          }
      where
        creditCardUpdateComponent = CreditCard.update
          { creditCards: fromMaybe mempty $ state.loggedInUser <#> _.creditCards
          , logger: logger
          , onCancel: setState _ { wrapperProgress = AsyncWrapper.Ready }
          , onLoading: setState _ { wrapperProgress = AsyncWrapper.Loading mempty }
          , onSuccess: setState _ { wrapperProgress = AsyncWrapper.Success $ Just "Uppdateringen av betalningsinformationen lyckades." }
          , onError: setState _ { wrapperProgress = AsyncWrapper.Error "Något gick fel. Vänligen försök pånytt, eller ta kontakt med vår kundtjänst." }
          }
   search =
     Router.route
       { exact: true
       , path: Just "/sök"
       , render:
         \_ -> Helpers.classy DOM.div "mitt-konto--container clearfix"
                 [ foldMap Elements.loadingIndicator state.loading
                 , case state.activeUser /\ state.adminMode of
                     Just user /\ true -> searchView
                     _ -> Views.loginView self logger
                 ]
       }
   noMatchRoute =
     -- TODO: Use Redirect when supported!
     Router.route
       { exact: false
       , path: Nothing
       , render: const mittKontoView
       }