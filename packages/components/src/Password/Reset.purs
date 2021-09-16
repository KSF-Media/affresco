module KSF.Password.Reset where

import Prelude

import Data.Either (Either(..))
import Data.Maybe (Maybe(..), fromMaybe)
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)
import KSF.Password.SendLink (requestResetLink)
import KSF.Password.Change (updatePasswordForm)
import KSF.Tracking as Tracking
import KSF.User (User)
import KSF.User as User
import React.Basic.Events (handler)
import React.Basic.Hooks (Component, component, useEffectOnce, useState', (/\))
import React.Basic.Hooks as React
import React.Basic.DOM as DOM
import React.Basic.DOM.Events (preventDefault)

type Props =
  { user :: Maybe User
  , code :: Maybe String
  , navToMain :: Effect Unit
  , passwordChangeDone :: Boolean
  , setPasswordChangeDone :: Boolean -> Effect Unit
  }

resetPassword :: Component Props
resetPassword = do
  updateForm <- updatePasswordForm
  resetLinkForm <- requestResetLink
  component "ResetPassword" $ \props@{ code, navToMain, user } -> React.do
    resetElement /\ setResetElement <- useState' Nothing
    useEffectOnce $ do
      case props.passwordChangeDone, code of
        true, Just _ ->
          setResetElement $ Just $
            React.fragment
              [ DOM.div_ [ DOM.text "Din lösenord har redan ändrats." ]
              , DOM.div_
                  [ DOM.a
                      { href: "/"
                      , children: [ DOM.text "Tillbaka till inloggningssidan" ]
                      , onClick: handler preventDefault $ const navToMain
                      }
                  ]
              ]
        _, Nothing -> setResetElement $ Just $
                     React.fragment
                       [ DOM.text "Vi skickar dig en länk som låter dig skapa ett nytt lösenord."
                       , resetLinkForm { user }
                       ]
        _, Just c -> launchAff_ do
          -- Check that the recovery code is valid and reserve it for use.
          startResult <- User.startPasswordReset c
          liftEffect $ setResetElement <<< Just =<< case startResult of
            Right _ -> pure $ updateForm { code: c
                                         , setChangeDone: props.setPasswordChangeDone true
                                         , navToMain
                                         }
            Left err -> do
              msg <- case err of
                User.PasswordResetTokenInvalid -> do
                  pure "Länken är föråldrad. Beställ en ny återställningslänk till din e-post."
                _ -> do
                  Tracking.updateResetPassword $ show err
                  pure "Något gick fel. Vänligen försok igen eller kontakta vår kundtjänst."
              pure $
                React.fragment
                  [ DOM.div
                      { className: "error-text"
                      , children: [ DOM.text msg ]
                      }
                  , resetLinkForm { user }
                  ]
      pure $ pure unit
    pure $
      DOM.div
        { id: "forgot-password-page"
        , children:
            [ DOM.h2_ [ DOM.text "Skapa nytt lösenord" ]
            , DOM.div
                { className: "password-reset--content"
                , children: [ fromMaybe (DOM.div { className: "tiny-spinner right" }) resetElement ]
                }
            ]
        }
