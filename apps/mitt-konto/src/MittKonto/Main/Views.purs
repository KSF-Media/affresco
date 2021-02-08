module MittKonto.Main.Views
  ( module Views
  , alertView
  , footerView
  , navbarView
  )
  where

import Prelude

import Data.Either (isLeft)
import Data.Maybe (Maybe(..))
import Effect.Aff as Aff
import Effect.Class (liftEffect)
import Effect.Class.Console as Console
import KSF.Alert.Component (Alert)
import KSF.Alert.Component as Alert
import KSF.Footer.Component as Footer
import KSF.Navbar.Component as Navbar
import KSF.Sentry as Sentry
import KSF.Spinner as Spinner
import KSF.User (logout) as User
import MittKonto.Main.Helpers as Helpers
import MittKonto.Main.CreditCardUpdateView (creditCardUpdateView) as Views
import MittKonto.Main.LoginView (loginView) as Views
import MittKonto.Main.PaymentView (paymentView) as Views
import MittKonto.Main.Types as Types
import MittKonto.Main.UserView (userView) as Views
import React.Basic (JSX)
import React.Basic.DOM as DOM
import React.Basic.Hooks as React


-- | Navbar with logo, contact info, logout button, language switch, etc.
navbarView :: Types.Self -> Sentry.Logger -> Boolean -> JSX
navbarView self@{ state, setState } logger isPersonating =
    Navbar.navbar
      { paper: state.paper
      , adminMode: state.adminMode
      , isPersonating: isPersonating
      , activeUser: state.activeUser
      , logout: do
          Aff.launchAff_ $ Spinner.withSpinner (setState <<< Types.setLoading) do
            User.logout \logoutResponse -> when (isLeft logoutResponse) $ Console.error "Logout failed"
            liftEffect do
              logger.setUser Nothing
              setState $ Types.setActiveUser Nothing
      }

alertView :: Alert -> JSX
alertView alert =
  Helpers.classy DOM.div "col-4 mx-auto center"
    [ Alert.alert alert ]

footerView :: React.JSX
footerView = Footer.footer {}
