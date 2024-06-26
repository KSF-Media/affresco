module MittKonto.Main.Views
  ( module Views
  , alertView
  , creditCardCallbackView
  , footerView
  , navbarWrapper
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
import KSF.Alert (Alert)
import KSF.Alert as Alert
import KSF.Footer as Footer
import KSF.Navbar.Component as Navbar
import KSF.User.Cusno as Cusno
import MittKonto.Main.Helpers as Helpers
import MittKonto.Main.LoginView (loginView) as Views
import MittKonto.Main.Types as Types
import React.Basic (JSX)
import React.Basic.DOM as DOM
import React.Basic.DOM.Events (preventDefault)
import React.Basic.Events (handler)
import React.Basic.Hooks (Component, component)
import Routing.PushState (PushStateInterface)

type Props =
  { state :: Types.State
  , logout :: Effect Unit
  , isPersonating :: Boolean
  }

-- | Navbar with logo, contact info, logout button, language switch, etc.
navbarWrapper :: PushStateInterface -> Component Props
navbarWrapper router = do
  navbarComponent <- Navbar.component
  component "NavbarWrapper" $ \ { state, logout, isPersonating } -> pure $ navbarComponent
    { paper: state.paper
    , specialHelp: guard state.adminMode $ state.activeUser *> Just
      (DOM.div
         { className: "nav--logout-limpet"
         , children:
             [ DOM.a
                 { onClick: (handler preventDefault $
                             const $ router.pushState (unsafeToForeign {}) "/betalvägg")
                 , href: "/betalvägg"
                 , children: [ DOM.text "Hantera betalvägg" ]
                 , style: DOM.css { margin: "1rem" }
                 }
             , DOM.a
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
    [ Alert.render alert ]

footerView :: JSX
footerView = Footer.render

creditCardCallbackView :: Boolean -> Effect Unit -> JSX
creditCardCallbackView true navToMain =
  DOM.div_
    [ DOM.text "Vänligen vänta"
    , DOM.div_
        [ DOM.a
            { href: "/"
            , onClick: handler preventDefault $ const navToMain
            , children: [ DOM.text "Eller avbryt" ]
            }
        ]
    ]
creditCardCallbackView false navToMain =
  DOM.div_
    [ DOM.text "Återställning av användarsession misslyckades"
    , DOM.div_
        [ DOM.a
            { href: "/"
            , onClick: handler preventDefault $ const navToMain
            , children: [ DOM.text "Avbryt" ]
            }
        ]
    ]
