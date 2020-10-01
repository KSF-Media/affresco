module MittKonto.Main where

import Prelude

import Data.Array (snoc, sortBy, (:))
import Data.Either (Either(..), either, isLeft)
import Data.Foldable (foldMap, oneOf)
import Data.JSDate (JSDate, parse)
import Data.Maybe (Maybe(..))
import Data.Set as Set
import Data.String (toUpper)
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Aff as Aff
import Effect.Class (liftEffect)
import Effect.Class.Console as Console
import Effect.Exception (Error, error, message)
import Effect.Unsafe (unsafePerformEffect)
import KSF.Alert.Component (Alert)
import KSF.Alert.Component as Alert
import KSF.Api.Subscription (isSubscriptionCanceled) as Subscription
import KSF.CreditCard.Update as CreditCardUpdate
import KSF.Error as KSF.Error
import KSF.Footer.Component as Footer
import KSF.JSError as Error
import KSF.Navbar.Component (Paper(..))
import KSF.Navbar.Component as Navbar
import KSF.Profile.Component as Profile
import KSF.Sentry as Sentry
import KSF.Subscription.Component (subscription) as Subscription
import KSF.User (User, UserError(..))
import KSF.User (logout) as User
import KSF.User.Login (login) as Login
import React.Basic (JSX, make)
import React.Basic as React
import React.Basic.DOM as DOM
import React.Basic.Events (handler, handler_)
import Tracking as Tracking

foreign import images :: { subscribe :: String }
foreign import sentryDsn_ :: Effect String

type Props =
  {}

type State =
  { paper :: Navbar.Paper
  , loggedInUser :: Maybe User
  , loading :: Maybe Loading
  , showWelcome :: Boolean
  , alert :: Maybe Alert
  , logger :: Sentry.Logger
  }

setLoading :: Maybe Loading -> State -> State
setLoading loading = _ { loading = loading }

setLoggedInUser :: Maybe User -> State -> State
setLoggedInUser loggedInUser = _ { loggedInUser = loggedInUser }

setAlert :: Maybe Alert -> State -> State
setAlert alert = _ { alert = alert }

data Loading = Loading

type Self = React.Self Props State

component :: React.Component Props
component = React.createComponent "MittKonto"

app :: Props -> JSX
app = make component
  { initialState:
      { paper: KSF
      , loggedInUser: Nothing
      , loading: Nothing
      , showWelcome: true
      , alert: Nothing
      , logger: Sentry.emptyLogger
      }
  , didMount
  , render
  }
  
didMount :: Self -> Effect Unit
didMount self = do
  sentryDsn <- sentryDsn_
  logger <- Sentry.mkLogger sentryDsn Nothing "mitt-konto"
  self.setState _ { logger = logger }

render :: Self -> JSX
render self@{ state, setState } =
  React.fragment
    [ navbarView self
    , classy DOM.div "mt4 mb4"
        [ foldMap alertView state.alert ]
    , classy DOM.div "mt4 mb4 clearfix"
        [ classy DOM.div "mitt-konto--main-container col-10 lg-col-7 mx-auto"
            [ mittKonto ]
        ]
    , footerView
    ]
 where
   mittKonto =
     classy DOM.div "mitt-konto--container clearfix"
       [ foldMap loadingIndicator state.loading
       , case state.loggedInUser of
           Just user -> userView self user
           Nothing   -> loginView self
       ]

loadingIndicator :: Loading -> JSX
loadingIndicator Loading =
  DOM.div
      { className: "mitt-konto--loading flex items-center"
      , children:
          [ DOM.div
              { className: "clearfix mx-auto"
              , children: [ DOM.div { className: "mitt-konto--loading-image", children: [] } ]
              }
          ]
      }

-- | Allows to run the asynchronous action while showing the loading indicator
--   and handling the result.
withSpinner :: forall a. (Maybe Loading -> Effect Unit) -> Aff a -> Aff a
withSpinner setLoadingState action = do
   let timeoutDelay = Aff.Milliseconds $ 30.0 * 1000.0
       flickerDelay = Aff.Milliseconds $ 200.0
   -- The "loading" thread turns the spinner on (when started) and off (when killed).
   -- Prevent the spinner from flickering.
   loadingFiber <-
     Aff.forkAff $ (do
       -- delay turning on the spinner, in case if the action is "instantanious"
       Aff.delay flickerDelay
       -- invincibly sleep for a bit more (would still wait if killed here)
       Aff.invincible $ do
         -- turn the spinner on
         liftEffect $ setLoadingState $ Just Loading
         Aff.delay flickerDelay
       -- in the end we sleep indefinetely. When killed, remove the spinner
       Aff.never) `Aff.cancelWith` Aff.effectCanceler (setLoadingState Nothing)
   -- The "action" thread runs the asynchronous task
   actionFiber <- Aff.forkAff action
   -- The "timeout" thread would kill the "action" thread after the delay.
   timeoutFiber <- Aff.forkAff do
     Aff.delay timeoutDelay
     Aff.killFiber (error "Timeout reached") actionFiber
     Aff.killFiber (error "Timeout reached") loadingFiber
   Aff.joinFiber actionFiber # Aff.finally do
     -- finally in the end, when the action has been completed
     -- we kill all other threads and switch the loading off
     Aff.killFiber (error "Action is done") timeoutFiber
     Aff.killFiber (error "Action is done") loadingFiber

-- | Navbar with logo, contact info, logout button, language switch, etc.
navbarView :: Self -> JSX
navbarView self@{ state, setState } =
    Navbar.navbar
      { paper: state.paper
      , loggedInUser: state.loggedInUser
      , logout: do
          Aff.launchAff_ $ withSpinner (setState <<< setLoading) do
            User.logout \logoutResponse -> when (isLeft logoutResponse) $ Console.error "Logout failed"
            liftEffect do
              self.state.logger.setUser Nothing
              setState $ setLoggedInUser Nothing
      }

alertView :: Alert -> JSX
alertView alert =
  classy DOM.div "col-4 mx-auto center"
    [ Alert.alert alert ]

footerView :: React.JSX
footerView = Footer.footer {}

-- | User info page with profile info, subscriptions, etc.
userView :: Self -> User -> JSX
userView { setState, state: { logger } } user = React.fragment
  [ classy DOM.div "col col-12 md-col-6 lg-col-6 mitt-konto--profile" [ profileView ]
  , classy DOM.div "col col-12 md-col-6 lg-col-6" [ subscriptionsView ]
  ]
  where
    componentHeader title =
      classy DOM.span "mitt-konto--component-heading" [ DOM.text $ toUpper title ]

    profileView =
      componentBlock
        "Mina uppgifter:"
        [ profileComponentBlock
        , break
        , editAccount
        , needHelp
        , disappearingBreak
        ]
      where
        profileComponentBlock = componentBlockContent $ Profile.profile
          { profile: user
          , onUpdate: setState <<< setLoggedInUser <<< Just
          , logger
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
            [ formatIconLink
                { href: "https://ksfmedia1.typeform.com/to/zbh3kU"
                , description: "Avsluta din prenumeration"
                , className: "mitt-konto--cancel-subscription"
                }
            ]
        }

    subscribeImage =
      DOM.div
        { className: "mitt-konto--subscribe-image flex"
        , children:
            [ anchor "https://prenumerera.ksfmedia.fi/" "" [ DOM.img { src: images.subscribe } ] ]
        }

    noSubscriptionsText =
      classy DOM.div "mitt-konto--no-subscriptions"
        [ DOM.p_
            [ DOM.text "Har du redan en prenumeration? Kontakta vår "
            , anchor "https://www.hbl.fi/kundservice/" "kundtjänst" []
            , DOM.text " och vi kopplar den till ditt konto."
            ]
        ]

    editAccount :: JSX
    editAccount =
      DOM.div
        { className: "mitt-konto--edit-account"
        , children:
            componentHeader "Mina inställningar:"
            : accountEditLinks
        }

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

    accountEditLinks :: Array JSX
    accountEditLinks =
      [ formatIconLink
          { href: "https://www.hbl.fi/losenord"
          , description: "Byt lösenord"
          , className: passwordChangeClass
          }
      ]
      where
        passwordChangeClass = "mitt-konto--password-change"

    formatIconLink :: { href :: String, description :: String, className :: String } -> JSX
    formatIconLink { href, description, className } =
      classy DOM.div "clearfix mitt-konto--account-edit-container"
        [ classy DOM.div "col-12 mt1"
            [ accountEditAnchor href
              $ classy DOM.div "mitt-konto--icon-container col circle"
                  [ classy DOM.div className [] ]
            , classy DOM.div "col col-8 pl1 pt2"
                [ accountEditAnchor href $ DOM.text description ]
            ]
        ]

    accountEditAnchor :: String -> JSX -> JSX
    accountEditAnchor href children =
      DOM.a
        { href
        , className: ""
        , children: [ children ]
        , target: "_blank"
        }

-- | Login page with welcoming header, description text and login form.
loginView :: Self -> JSX
loginView self@{ state: state@{ logger }, setState } = React.fragment
  [ DOM.div_
      case state.showWelcome of
        false -> []
        true  ->
          [ classy DOM.div "pb2 center" [ heading ]
          , classy DOM.div "center"     [ pageDescription ]
          ]
  , classy DOM.div "center" [ loginForm ]
  ]
  where
    loginForm =
        Login.login
          { onMerge:             setState \s -> s { showWelcome = false }
          , onMergeCancelled:    setState \s -> s { showWelcome = true }
          , onRegister:          setState \s -> s { showWelcome = false }
          , onRegisterCancelled: setState \s -> s { showWelcome = true }
          , onUserFetch:
            case _ of
              Left err -> do
                case err of
                  SomethingWentWrong -> logger.error $ Error.loginError $ show err
                  UnexpectedError e  -> logger.error $ Error.loginError $ message e
                  -- If any other UserError occurs, only send an Info event of it
                  _ -> logger.log (show err) Sentry.Info
                self.setState $ setLoggedInUser Nothing
              Right user -> do
                setState $ setLoggedInUser $ Just user
                logger.setUser $ Just user
          , launchAff_:
              Aff.runAff_ (setState <<< setAlert <<< either errorAlert (const Nothing))
                <<< withSpinner (setState <<< setLoading)
          , disableSocialLogins: Set.empty
          }

    heading =
      classy DOM.h1 "mitt-konto--heading"
        [ DOM.text "Välkommen till KSF Media’s Mitt Konto" ]

    frequentIssues =
      classy DOM.p "mitt-konto--faq"
        [ DOM.a
            { href: "https://www.hbl.fi/fragor-och-svar/"
            , children: [ DOM.text "Frågor och svar" ]
            , target: "_blank"
            }
        , DOM.text " * "
        , DOM.a
            { href: "https://www.hbl.fi/ingen-tidning/"
            , children: [ DOM.text "Ingen tidning" ]
            , target: "_blank"
            }
        , DOM.text " * "
        , DOM.a
            { href: "https://www.hbl.fi/epaper/"
            , children: [ DOM.text "Läs e-tidning" ]
            , target: "_blank"
            }
        ]

    pageDescription =
      classy DOM.div "mitt-konto--description"
        [ frequentIssues
        , DOM.p_
            [ DOM.text
                """Här kan du göra tillfällig eller permanent
                   adressändring eller göra uppehåll i tidningsutdelningen.
                   Dessutom kan du få allmän information som är
                   relevant för dig som prenumerant.
                """
            ]
        , DOM.p_
            [ DOM.text "Allt du behöver göra för att komma igång är att logga in!" ]
        , DOM.p_
            [ DOM.text "Behöver du hjälp? "
            , anchor "https://www.hbl.fi/kundservice/" "Kundservice" []
            ]
        ]

errorAlert :: Error -> Maybe Alert
errorAlert err = oneOf
  [ do { method, url } <- KSF.Error.networkError err
       pure
         { level: Alert.danger
         , title: "Anslutningen misslyckades."
         , message: "Vänligen kontrollera din internetanslutning och försök om en stund igen."
         }
  , pure
      { level: Alert.warning
      , title: "Något gick fel vid inloggningen."
      , message: "Vänligen försök om en stund igen."
      }
  ]

break :: JSX
break = DOM.hr { className: "mitt-konto--break" }

-- | This break will appear only in narrower views
disappearingBreak :: JSX
disappearingBreak =
  DOM.div
    { className: "mitt-konto--disappearing-break"
    , children: [ break ]
    }

classy
  :: ({ className :: String, children :: Array JSX} -> JSX)
  -> String
  -> (Array JSX -> JSX)
classy element className children = element { className, children }

anchor :: String -> String -> Array JSX -> JSX
anchor href description children = DOM.a { href, children: DOM.text description : children, target: "_blank" }

-- I know, but it's just for accessing locale
readJSDate :: String -> JSDate
readJSDate date = unsafePerformEffect $ parse date
