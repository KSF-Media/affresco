module MittKonto.Main.LoginView where

import Prelude

import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Set as Set
import Effect.Aff as Aff
import Effect.Exception (message)
import KSF.JSError as Error
import KSF.Sentry as Sentry
import KSF.User (UserError(..), isAdminUser)
import MittKonto.Main.Helpers as Helpers
import MittKonto.Main.Types as Types
import React.Basic (JSX)
import React.Basic.DOM as DOM

foreign import logos ::
  { hbl :: String
  , on  :: String
  , vn  :: String
  }

-- | Login page with welcoming header, description text and login form.
loginView :: Types.Self-> Sentry.Logger -> JSX
loginView self@{ state, setState } logger =
  Helpers.classy DOM.div "mitt-konto--frontpage" $
    case state.showWelcome of
        false -> []
        true  ->
          [ logosWrapper
          , title
          , description
          , descriptionList
          ]
    <> [ Helpers.classy DOM.div "form" [ loginForm ] ]
  where
    loginForm =
        state.loginComponent
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
          , onLogin: Aff.launchAff_
          , disableSocialLogins: Set.empty
          }

    logosWrapper = Helpers.classy DOM.div "logos"
              [ DOM.img { src: logos.vn, className: "logo", alt: "VN Västis" }
              , DOM.img { src: logos.hbl, className: "logo", alt: "HBL Husis" }
              , DOM.img { src: logos.on, className: "logo", alt: "ÖN" }
              ]

    title =
      Helpers.classy DOM.div "title"
        [ Helpers.classy DOM.h1 "mitt-konto--heading"
            [ DOM.text "Mitt konto – kundservice för HBL, VN, ÖN och HBL Junior"
            ]
        ]

    description =
      Helpers.classy DOM.div "description"
        [ DOM.h2_ [ DOM.text "I Mitt konto sköter du smidigt dina prenumerationsärenden för Hufvudstadsbladet, Västra Nyland, Östnyland och HBL Junior. Du kan lätt:" ]
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
