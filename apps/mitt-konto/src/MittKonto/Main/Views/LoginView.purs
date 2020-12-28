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
import KSF.User (UserError(..), isAdminUser)
import KSF.User.Login (login) as Login
import MittKonto.Main.Helpers as Helpers
import MittKonto.Main.Types as Types
import React.Basic (JSX)
import React.Basic.DOM as DOM

-- | Login page with welcoming header, description text and login form.
loginView :: Types.Self-> Sentry.Logger -> JSX
loginView self@{ state, setState } logger =
  Helpers.classy DOM.div "mitt-konto--frontpage" $
    case state.showWelcome of
        false -> []
        true  ->
          [ logos
          , heading
          , description
          ]
    <> [ Helpers.classy DOM.div "center login-form" [ loginForm ] ]
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
                self.setState $ Types.setActiveUser Nothing
              Right user -> do
                admin <- isAdminUser
                setState $ (Types.setActiveUser $ Just user) <<< (_ { adminMode = admin } )
                logger.setUser $ Just user
          , launchAff_:
              Aff.runAff_ (setState <<< Types.setAlert <<< either Helpers.errorAlert (const Nothing))
                <<< Spinner.withSpinner (setState <<< Types.setLoading)
          , disableSocialLogins: Set.empty
          }

    logos = Helpers.classy DOM.div "logos"
              [ DOM.div { className: "vn-logo" }
              , DOM.div { className: "hbl-logo" }
              , DOM.div { className: "on-logo" }
              ]

    heading =
      Helpers.classy DOM.h1 "mitt-konto--heading"
        [ DOM.text "Välkommen till Mitt konto"
        ]

    frequentIssues =
      Helpers.classy DOM.p "faq"
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

<<<<<<< HEAD
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
=======
    description =
      Helpers.classy DOM.div "description"
        [ DOM.h2_ [ DOM.text "I Mitt konto sköter du smidigt dina prenumerationsärenden för Hufvudstadsbladet, Västra Nyland, Östnyland och HBL Junior. Du kan lätt:" ]
        , descriptionList
        ]

    descriptionList :: JSX
    descriptionList = Helpers.classy DOM.div "list" $
                        descriptionListItem <$> [ "göra addressändringar"
                                                , "göra uppehåll"
                                                , "reklamera utebliven tidning"
                                                , "uppdatera ditt kredit- eller bankkort"
                                                , "visa din fakturor"
                                                , "avsluta din prenumeration"
                                                , "med mera ..."
                                                ]


    descriptionListItem :: String -> JSX
    descriptionListItem text = Helpers.classy DOM.div "item"
                                 [ DOM.div { className: "check-icon" }
                                 , Helpers.classy DOM.div "text"
                                     [ DOM.text text ]
                                 ]
>>>>>>> Patch MittKonto.Main.Views.LoginView
