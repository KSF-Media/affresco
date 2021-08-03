module MittKonto.Main.Views
  ( module Views
  , alertView
  , footerView
  , navbarView
  )
  where

import Prelude

import Data.Array (mapMaybe)
import Data.Either (isLeft)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Monoid (guard)
import Data.Nullable (toMaybe)
import Data.String as String
import Effect.Aff as Aff
import Effect.Class (liftEffect)
import Effect.Class.Console as Console
import Foreign (unsafeToForeign)
import KSF.Alert.Component (Alert)
import KSF.Alert.Component as Alert
import KSF.Footer.Component as Footer
import KSF.Navbar.Component as Navbar
import KSF.Sentry as Sentry
import KSF.Spinner as Spinner
import KSF.User (logout) as User
import KSF.User.Cusno as Cusno
import MittKonto.Main.Helpers as Helpers
import MittKonto.Main.CreditCardUpdateView (creditCardUpdateView) as Views
import MittKonto.Main.LoginView (loginView) as Views
import MittKonto.Main.Types as Types
import MittKonto.Main.UserView (userView) as Views
import React.Basic (JSX)
import React.Basic.DOM as DOM
import React.Basic.DOM.Events (preventDefault)
import React.Basic.Events (handler, handler_)
import Routing.PushState (PushStateInterface)

-- | Navbar with logo, contact info, logout button, language switch, etc.
navbarView :: Types.Self -> Sentry.Logger -> PushStateInterface -> Boolean -> JSX
navbarView { state, setState } logger router isPersonating =
    Navbar.navbar
      { paper: state.paper
      , specialHelp: guard state.adminMode $ Just
        (DOM.div
           { className: "nav--logout-limpet"
           , children:
               [ DOM.a
                   { onClick: (handler preventDefault $
                               const $ router.pushState (unsafeToForeign {}) "/sök")
                   , href: "/sök"
                   , children: [ DOM.text "Sök kund" ]
                   }
               ] <>
               ( fromMaybe [] $ guard isPersonating $
                 (\user -> [ DOM.div_ [ DOM.strong_ [ DOM.text "Aktiv kund" ] ]
                           , userLink user
                           ]) <$> state.activeUser
               )
           }
        )
      , activeUser: state.activeUser
      , logoutWrapper: Just $
          \x -> DOM.div
                  { onClick: handler_ $ router.pushState (unsafeToForeign {}) "/"
                  , children: [ x ]
                  }
      , logout: do
          Aff.launchAff_ $ Spinner.withSpinner (setState <<< Types.setLoading) do
            User.logout \logoutResponse -> when (isLeft logoutResponse) $ Console.error "Logout failed"
            liftEffect do
              logger.setUser Nothing
              setState $ Types.setActiveUser Nothing
      }
  where
    userLink user =
      DOM.a
        { href: "/"
        , onClick: handler preventDefault $ const $ router.pushState (unsafeToForeign {}) "/"
        , children: [ DOM.text $ Cusno.toString user.cusno <> " - " <>
                        ( String.joinWith " " $
                          mapMaybe toMaybe [ user.firstName, user.lastName ] )
                    ]
        }

alertView :: Alert -> JSX
alertView alert =
  Helpers.classy DOM.div "col-4 mx-auto center"
    [ Alert.alert alert ]

footerView :: JSX
footerView = Footer.footer {}
