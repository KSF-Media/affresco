module MittKonto.Main.LoginView where

import Prelude

import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Set as Set
import Effect (Effect)
import Effect.Aff as Aff
import Effect.Exception (message)
import KSF.JSError as Error
import KSF.Sentry as Sentry
import KSF.User (UserError(..), User)
import MittKonto.Main.Helpers as Helpers
import MittKonto.Main.Types as Types
import React.Basic (JSX)
import React.Basic.DOM as DOM

foreign import logos ::
  { hbl :: String
  , bbl :: String
  , vn  :: String
  }

-- | Login page with welcoming header, description text and login form.
loginView :: Types.Self-> (User -> Effect Unit) -> Sentry.Logger -> JSX
loginView self@{ state, setState } setUser logger =
  Helpers.classy DOM.div "mitt-konto--frontpage" $
    case state.showWelcome of
        false -> []
        true  ->
          [ logosWrapper
          , title
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
                  -- List of cases we aren't that interested of
                  MergeEmailInUse _ -> pure unit
                  LoginTokenInvalid -> pure unit
                  LoginInvalidCredentials -> pure unit
                  -- If any other UserError occurs, only send an Info event of it
                  _ -> logger.log (show err) Sentry.Info
                self.setState $ Types.setActiveUser Nothing
              Right user -> setUser user
          , onLogin: Aff.launchAff_
          , disableSocialLogins: Set.empty
          , paper: Nothing
          }

    logosWrapper = Helpers.classy DOM.div "logos"
              [ DOM.img { src: logos.vn, className: "logo", alt: "VN Västis" }
              , DOM.img { src: logos.hbl, className: "logo", alt: "HBL Husis" }
              , DOM.img { src: logos.bbl, className: "logo", alt: "BBL" }
              ]

    title =
      Helpers.classy DOM.div "title"
        [ Helpers.classy DOM.h1 "mitt-konto--heading"
            [ DOM.text "Uppdatera ditt kreditkort"
            ]
        ]
