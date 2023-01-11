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
  , on  :: String
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
          , description
          , descriptionList
          ]
    <> [ Helpers.classy DOM.div "form" [ loginForm ], instructionVideo ]
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

    {- This is the exact embed code YouTube gives us that is then converted to PS,
       except for the allow attribute which cannot be found in JSX.
     <iframe 
        width="560" 
        height="315" 
        src="https://www.youtube.com/embed/EQtWIjnRlN0" 
        title="YouTube video player" 
        frameborder="0" 
        allow="accelerometer; autoplay; clipboard-write; encrypted-media; gyroscope; picture-in-picture" 
        allowfullscreen>
      </iframe> 
    -}
    instructionVideo = 
      DOM.div 
        { className: "mitt-konto--frontpage__instruction-video"
        , children: 
            [ DOM.span_ 
                [ DOM.text "Så här sköter du dina prenumerationsärenden i Mitt konto:" ]
            , DOM.iframe 
                { width: "560"
                , height: "315"
                , src:"https://www.youtube.com/embed/EQtWIjnRlN0"
                , title: "YouTube video player"
                , frameBorder:"0"
                , allowFullScreen: true
                }
            ]
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
                        descriptionListItem <$> [ "göra adressändringar"
                                                , "göra uppehåll"
                                                , "reklamera utebliven tidning"
                                                , "uppdatera ditt kredit- eller bankkort"
                                                , "visa dina fakturor"
                                                , "avsluta din prenumeration"
                                                , "med mera ..."
                                                ]


    descriptionListItem :: String -> JSX
    descriptionListItem text = Helpers.classy DOM.div "item"
                                 [ DOM.div { className: "check-icon" }
                                 , Helpers.classy DOM.div "text"
                                     [ DOM.text text ]
                                 ]
