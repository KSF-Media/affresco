module MittKonto.Main where

import Prelude

import Data.Array ((:))
import Data.Either (Either(..))
import Data.Foldable (foldMap)
import Data.JSDate (JSDate, parse)
import Data.Maybe (Maybe(..))
import Data.String (toUpper)
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Aff as Aff
import Effect.Class (liftEffect)
import Effect.Class.Console (log)
import Effect.Exception (error)
import Effect.Unsafe (unsafePerformEffect)
import KSF.Footer.Component as Footer
import KSF.Login.Component as Login
import KSF.Navbar.Component (Paper(..))
import KSF.Navbar.Component as Navbar
import KSF.Profile.Component as Profile
import KSF.Subscription.Component as Subscription
import Persona as Persona
import React.Basic (JSX)
import React.Basic as React
import React.Basic.DOM as DOM
import Tracking as Tracking

foreign import images :: { subscribe :: String }

type Props =
  {}

type State =
  { paper :: Navbar.Paper
  , loggedInUser :: Maybe Persona.User
  , loading :: Maybe Loading
  , showWelcome :: Boolean
  }

setLoading :: Maybe Loading -> State -> State
setLoading loading = _ { loading = loading }

setLoggedInUser :: Maybe Persona.User -> State -> State
setLoggedInUser loggedInUser = _ { loggedInUser = loggedInUser }

data Loading = Loading

type SetState = (State -> State) -> Effect Unit

app :: React.Component Props
app = React.component
  { displayName: "App"
  , initialState:
      { paper: KSF
      , loggedInUser: Nothing
      , loading: Nothing
      , showWelcome: true
      }
  , receiveProps
  , render
  }
  where
    receiveProps _ = do
      tracker <- Tracking.newTracker
      Tracking.pushPageLoad tracker
    render { state, setState } =
      React.fragment
        [ navbarView { state, setState }
        , classy DOM.div "clearfix"
            [ classy DOM.div "mitt-konto--main-container col-10 lg-col-7 mx-auto"
                [ mittKonto ]
            ]
        , footerView
        ]
     where
       mittKonto =
         classy DOM.div "mitt-konto--container clearfix mt4"
           [ foldMap loadingIndicator state.loading
           , case state.loggedInUser of
               Just user -> userView { user }
               Nothing   -> loginView { state, setState }
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
   let timeoutSeconds = 30.0
   -- The "loading" thread sleeps a bit and then switches turns the spinner on.
   -- This is to prevent flickering in case if the action completes instantly.
   loadingFiber <- Aff.forkAff do
     Aff.delay $ Aff.Milliseconds 100.0
     liftEffect $ setLoadingState $ Just Loading
   -- The "action" thread runs the asynchronous task
   actionFiber <- Aff.forkAff action
   -- The "timeout" thread would kill the "action" thread after the delay.
   timeoutFiber <- Aff.forkAff do
     Aff.delay $ Aff.Milliseconds $ timeoutSeconds * 1000.0
     Aff.killFiber (error "Timeout reached") actionFiber
   Aff.joinFiber actionFiber # Aff.finally do
     -- finally in the end, when the action has been completed
     -- we kill all other threads and switch the loading off
     Aff.killFiber (error "Action is done") timeoutFiber
     Aff.killFiber (error "Action is done") loadingFiber
     liftEffect $ setLoadingState Nothing

-- | Navbar with logo, contact info, logout button, language switch, etc.
navbarView :: { state :: State, setState :: SetState } -> JSX
navbarView { state, setState } =
  React.element
    Navbar.component
      { paper: state.paper
      , loggedInUser: state.loggedInUser
      , logout: do
          Aff.launchAff_ $ withSpinner (setState <<< setLoading)
            (Login.logout (setState <<< setLoggedInUser))
      }

footerView :: React.JSX
footerView =
  React.element
    Footer.component {}

-- | User info page with profile info, subscriptions, etc.
userView :: { user :: Persona.User } -> JSX
userView { user } = React.fragment
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
        , break
        , feedbackForm
        , disappearingBreak
        ]
      where
        profileComponentBlock = componentBlockContent $ React.element Profile.component { profile: user }

    subscriptionsView =
      componentBlock "Mina prenumerationer:" blockContent
      where
        noSubscriptions =
          case user.subs of
            [] -> componentBlockContent noSubscriptionsText
            _  -> React.empty

        subscriptionBlocks = map (componentBlockContent <<< subscriptionView) user.subs

        blockContent = noSubscriptions : subscriptionBlocks <> [ break, subscribeImage ]

    subscriptionView subscription =
      React.element Subscription.component { subscription }

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

    feedbackForm :: JSX
    feedbackForm =
      formatIconLink
        { href: "https://goo.gl/forms/DAEPPgskTofooCFF3"
        , description: "Ge oss feedback om Mitt konto"
        , className: "mitt-konto--feedback"
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
      , formatIconLink
          { href: "https://www.hbl.fi/uppehall/"
          , description: "Gör uppehåll"
          , className: pauseSubscriptionClass
          }
      , formatIconLink
          { href: "https://www.hbl.fi/tillfallig-adressandring/"
          , description: "Gör tillfällig adressändring"
          , className: temporaryAddressChangeClass
          }
      , formatIconLink
          { href: "https://www.hbl.fi/permanent-adressandring/"
          , description: "Gör permanent adressändring"
          , className: permanentAddressChangeClass
          }
      ]
      where
        passwordChangeClass         = "mitt-konto--password-change"
        pauseSubscriptionClass      = "mitt-konto--pause-subscription"
        temporaryAddressChangeClass = "mitt-konto--temporary-address-change"
        permanentAddressChangeClass = "mitt-konto--permanent-address-change"

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
loginView :: { state :: State, setState :: SetState } -> JSX
loginView { state, setState } = React.fragment
  [ DOM.div_
      case state.showWelcome of
        false -> []
        true  ->
          [ classy DOM.div "pb3 center" [ heading ]
          , classy DOM.div "center"     [ pageDescription ]
          ]
  , classy DOM.div "center" [ loginForm ]
  ]
  where
    loginForm =
      React.element
        Login.component
          { onMerge:          setState \s -> s { showWelcome = false }
          , onMergeCancelled: setState \s -> s { showWelcome = true }
          , onUserFetch:
            case _ of
              Left err -> do
                log "Fetching user failed"
                setState $ setLoggedInUser Nothing
              Right user -> do
                log "Fetching user succeeded"
                setState $ setLoggedInUser $ Just user
          , launchAff_: Aff.launchAff_ <<< withSpinner (setState <<< setLoading)
          }

    heading =
      classy DOM.h1 "mitt-konto--heading"
        [ DOM.text "Välkommen till KSF Media’s Mitt Konto" ]

    pageDescription =
      classy DOM.div "mitt-konto--description"
        [ DOM.p_
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
