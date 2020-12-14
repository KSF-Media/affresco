module MittKonto.Main.LoginView where

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
import MittKonto.Main.Helpers as Helpers
import MittKonto.Main.Types as Types
import React.Basic (JSX, element)
import React.Basic.DOM as DOM
import React.Basic.Hooks (Component, component, useState, (/\))
import React.Basic.Hooks as React
import React.Basic.Router as Router

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
                <<< Helpers.withSpinner (setState <<< Helpers.setLoading)
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