module MittKonto.Main where

import Prelude

import Bottega.Models (CreditCard)
import Data.Array (snoc, sortBy, (:))
import Data.Either (Either(..), either, isLeft)
import Data.Foldable (foldMap, oneOf)
import Data.JSDate (JSDate, parse)
import Data.Maybe (Maybe(..))
import Data.Nullable (toNullable)
import Data.Set as Set
import Data.String (toUpper)
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Aff as Aff
import Effect.Class (liftEffect)
import Effect.Class.Console as Console
import Effect.Exception (Error, error, message)
import Effect.Unsafe (unsafePerformEffect)
import MittKonto.AccountEdit as AccountEdit
import MittKonto.IconAction as IconAction
import MittKonto.Main.LoginView as LoginView
import MittKonto.Main.Helpers as Helpers
import MittKonto.Main.Types as Types
import KSF.Alert.Component (Alert)
import KSF.Alert.Component as Alert
import KSF.Api.Subscription (isSubscriptionCanceled) as Subscription
import KSF.Error as KSF.Error
import KSF.Footer.Component as Footer
import KSF.JSError as Error
import KSF.Paper (Paper(..))
import KSF.Navbar.Component as Navbar
import KSF.PaymentAccordion as PaymentAccordion
import KSF.Profile.Component as Profile
import KSF.Sentry as Sentry
import KSF.Subscription.Component (subscription) as Subscription
import KSF.User (User, UserError(..), SubscriptionPayments)
import KSF.User (logout, getPayments) as User
import KSF.User.Login (login) as Login
import React.Basic (JSX, element)
import React.Basic.DOM as DOM
import React.Basic.Hooks (Component, component, useState, (/\))
import React.Basic.Hooks as React
import React.Basic.Router as Router

foreign import images :: { subscribe :: String }
foreign import sentryDsn_ :: Effect String

type State =
  { paper :: Paper
  , loggedInUser :: Maybe User
  , loading :: Maybe Types.Loading
  , showWelcome :: Boolean
  , alert :: Maybe Alert
  , payments :: Maybe (Array SubscriptionPayments)
  , creditCards :: Array CreditCard
  }

setLoading :: Maybe Types.Loading -> State -> State
setLoading loading = _ { loading = loading }

setLoggedInUser :: Maybe User -> State -> State
setLoggedInUser loggedInUser = _ { loggedInUser = loggedInUser }

setAlert :: Maybe Alert -> State -> State
setAlert alert = _ { alert = alert }

setPayments :: Maybe (Array SubscriptionPayments) -> State -> State
setPayments payments = _ { payments = payments }

type Self =
  { state :: State
  , setState :: (State -> State) -> Effect Unit
  }

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

render :: Self -> Sentry.Logger -> JSX
render self@{ state, setState } logger =
  React.fragment
    [ navbarView self logger
    , Helpers.classy DOM.div "mt4 mb4"
        [ foldMap alertView state.alert ]
    , Helpers.classy DOM.div "mt4 mb4 clearfix"
        [ Helpers.classy DOM.div "mitt-konto--main-container col-10 lg-col-7 mx-auto"
            [ element Router.switch { children: [ paymentListRoute, mittKontoRoute, noMatchRoute ] } ]
        ]
    , footerView
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
        [ foldMap loadingIndicator state.loading
        , case state.loggedInUser of
            Just user -> userView self logger user
            Nothing   -> LoginView.loginView self logger
        ]
   paymentListRoute =
     element
       Router.route
         { exact: true
         , path: toNullable $ Just "/fakturor"
         , render: const
             $ Helpers.classy DOM.div "mitt-konto--container clearfix"
                 [ foldMap loadingIndicator state.loading
                 , case state.loggedInUser of
                     Just user -> paymentView self user
                     Nothing   -> LoginView.loginView self logger
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

loadingIndicator :: Types.Loading -> JSX
loadingIndicator Types.Loading =
  DOM.div
      { className: "mitt-konto--loading flex items-center"
      , children:
          [ DOM.div
              { className: "clearfix mx-auto"
              , children: [ DOM.div { className: "mitt-konto--loading-image", children: [] } ]
              }
          ]
      }

-- | Navbar with logo, contact info, logout button, language switch, etc.
navbarView :: Self -> Sentry.Logger -> JSX
navbarView self@{ state, setState } logger =
    Navbar.navbar
      { paper: state.paper
      , loggedInUser: state.loggedInUser
      , logout: do
          Aff.launchAff_ $ Helpers.withSpinner (setState <<< setLoading) do
            User.logout \logoutResponse -> when (isLeft logoutResponse) $ Console.error "Logout failed"
            liftEffect do
              logger.setUser Nothing
              setState $ setLoggedInUser Nothing
      }

alertView :: Alert -> JSX
alertView alert =
  Helpers.classy DOM.div "col-4 mx-auto center"
    [ Alert.alert alert ]

footerView :: React.JSX
footerView = Footer.footer {}

-- | User info page with profile info, subscriptions, etc.
userView :: Self -> Sentry.Logger -> User -> JSX
userView { setState, state: { creditCards } } logger user = React.fragment
  [ Helpers.classy DOM.div "col col-12 md-col-6 lg-col-6 mitt-konto--profile" [ profileView ]
  , Helpers.classy DOM.div "col col-12 md-col-6 lg-col-6" [ subscriptionsView ]
  ]
  where
    componentHeader title =
      Helpers.classy DOM.span "mitt-konto--component-heading" [ DOM.text $ toUpper title ]

    profileView =
      componentBlock
        "Mina uppgifter:"
        [ profileComponentBlock
        , break
        , editAccountBlock
        , needHelp
        , disappearingBreak
        ]
      where
        profileComponentBlock = componentBlockContent $ Profile.profile
          { profile: user
          , onUpdate: setState <<< setLoggedInUser <<< Just
          , logger
          }
        editAccountBlock = DOM.div
          { className: "mitt-konto--edit-account"
          , children:
              [ componentHeader "Mina inställningar:"
              , componentBlockContent $ AccountEdit.accountEdit
                  { logger: logger
                  , creditCards
                  , setCreditCards: \cards -> setState _ { creditCards = cards }
                  }
              ]
          }

    subscriptionsView =
      componentBlock "Mina prenumerationer:" $ subscriptions <> [ break, subscribeImage ]
      where
        subscriptions =
          -- Sort the canceled subscriptions to the end of the list
          case sortBy (comparing _.state) user.subs of
            []   -> [ componentBlockContent noSubscriptionsText ]
            subs -> do
              map subscriptionComponentBlockContent subs `snoc` cancelSubscription
              where
                subscriptionView subscription = Subscription.subscription { subscription, user, logger }
                subscriptionComponentBlockContent subscription
                  -- If the subscription has a canceled state, we want to add extra css to it.
                  | Subscription.isSubscriptionCanceled subscription =
                      DOM.div
                        { className: "mitt-konto--canceled-subscription"
                        , children: [ componentBlockContent $ subscriptionView subscription ]
                        }
                  | otherwise = componentBlockContent $ subscriptionView subscription

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
          ,  DOM.dl_
              [ DOM.dt_ [ DOM.text "Ingen tidning?" ]
              , DOM.dd_ [ issueLink "HBL" "https://www.hbl.fi/ingen-tidning/" ]
              , DOM.dd_ [ issueLink "Västra Nyland" "https://www.vastranyland.fi/ingen-tidning/" ]
              , DOM.dd_ [ issueLink "Östnyland" "https://www.ostnyland.fi/ingen-tidning/" ]
              ]
          ]
        issueLink description href =
          DOM.a
            { children: [ DOM.text description ]
            , href
            , target: "_blank"
            }

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

-- | Specialized view with user's payment list
paymentView :: Self -> User -> JSX
paymentView self user = DOM.div_
  [ element
      Router.link
        { to: { pathname: "/", state: {} }
        , children: [ ]
        , className: "mitt-konto--backwards"
        }
  , PaymentAccordion.payments { paymentsLoad }
  ]
  where
    paymentsLoad =
      case self.state.payments of
        Just payments -> pure payments
        Nothing ->
          Helpers.withSpinner (self.setState <<< setLoading) do
            p <- User.getPayments user.uuid
            case p of
              Right payments -> do
                liftEffect $ self.setState _ { payments = Just payments }
                pure payments
              Left _         -> do
                liftEffect $ self.setState $ setAlert $ Just
                  { level: Alert.warning
                  , title: "Laddningen misslyckades."
                  , message: "Något gick fel, ta kontakt med kundservice."
                  }
                pure []

break :: JSX
break = DOM.hr { className: "mitt-konto--break" }

-- | This break will appear only in narrower views
disappearingBreak :: JSX
disappearingBreak =
  DOM.div
    { className: "mitt-konto--disappearing-break"
    , children: [ break ]
    }

-- I know, but it's just for accessing locale
readJSDate :: String -> JSDate
readJSDate date = unsafePerformEffect $ parse date
