module MittKonto.Main.Views
  ( module Views
  , alertView
  , footerView
  , navbarView
  )
  where

import Prelude

import Data.Array (mapMaybe)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Monoid (guard)
import Data.Nullable (toMaybe)
import Data.String as String
import Effect (Effect)
import Foreign (unsafeToForeign)
import KSF.Alert.Component (Alert)
import KSF.Alert.Component as Alert
import KSF.Footer.Component as Footer
import KSF.Navbar.Component as Navbar
import KSF.User.Cusno as Cusno
import MittKonto.Main.Helpers as Helpers
import MittKonto.Main.CreditCardUpdateView (creditCardUpdateView) as Views
import MittKonto.Main.LoginView (loginView) as Views
import MittKonto.Main.Types as Types
import MittKonto.Main.UserView (userView) as Views
import React.Basic (JSX)
import React.Basic.DOM as DOM
import React.Basic.DOM.Events (preventDefault)
import React.Basic.Events (handler)
import Routing.PushState (PushStateInterface)

-- | Navbar with logo, contact info, logout button, language switch, etc.
navbarView :: Types.Self -> PushStateInterface -> Effect Unit -> Boolean -> JSX
navbarView { state } router logout isPersonating =
    Navbar.navbar
      { paper: state.paper
      , specialHelp: guard state.adminMode $ state.activeUser *> Just
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
      , logout
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
  Helpers.classy DOM.div "mitt-konto--alert"
    [ Alert.alert alert ]

footerView :: JSX
footerView = Footer.footer {}
