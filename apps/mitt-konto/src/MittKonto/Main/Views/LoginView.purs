module MittKonto.Main.LoginView where

import Prelude

import Data.Either (Either(..), either)
import Data.Maybe (Maybe(..))
import Data.Set as Set
import Effect.Aff as Aff
import Effect.Exception (message)
import KSF.JSError as Error
import KSF.Sentry as Sentry
import KSF.Spinner as Spinner
import KSF.User (UserError(..))
import KSF.User.Login (login) as Login
import MittKonto.Main.Helpers as Helpers
import MittKonto.Main.Types as Types
import React.Basic (JSX)
import React.Basic.DOM as DOM
import React.Basic.Hooks as React

-- | Login page with welcoming header, description text and login form.
loginView :: Types.Self-> Sentry.Logger -> JSX
loginView self@{ state, setState } logger = React.fragment
  [ DOM.div_
      case state.showWelcome of
        false -> []
        true  ->
          [ Helpers.classy DOM.div "pb2 center" [ heading ]
          , Helpers.classy DOM.div "center"     [ pageDescription ]
          ]
  , Helpers.classy DOM.div "center" [ loginForm ]
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
                self.setState $ Helpers.setLoggedInUser Nothing
              Right user -> do
                setState $ Helpers.setLoggedInUser $ Just user
                logger.setUser $ Just user
          , launchAff_:
              Aff.runAff_ (setState <<< Helpers.setAlert <<< either Helpers.errorAlert (const Nothing))
                <<< Spinner.withSpinner (setState <<< Helpers.setLoading)
          , disableSocialLogins: Set.empty
          }

    heading =
      Helpers.classy DOM.h1 "mitt-konto--heading"
        [ DOM.text "Välkommen till KSF Media’s Mitt Konto" ]

    frequentIssues =
      Helpers.classy DOM.p "mitt-konto--faq"
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
      Helpers.classy DOM.div "mitt-konto--description"
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
            , Helpers.anchor "https://www.hbl.fi/kundservice/" "Kundservice" []
            ]
        ]