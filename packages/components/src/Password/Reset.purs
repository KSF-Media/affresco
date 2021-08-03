module KSF.Password.Reset where

import Prelude

import Data.Either (Either(..))
import Data.Maybe (Maybe(..), fromMaybe)
import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)
import KSF.Password.SendLink (requestResetLink)
import KSF.Password.Change (updatePasswordForm)
import KSF.Tracking as Tracking
import KSF.User (User)
import KSF.User as User
import React.Basic.Hooks (Component, component, useEffectOnce, useState', (/\))
import React.Basic.Hooks as React
import React.Basic.DOM as DOM

type Props = { user :: Maybe User
             , code :: Maybe String
             }

resetPassword :: Component Props
resetPassword = do
  updateForm <- updatePasswordForm
  resetLinkForm <- requestResetLink
  component "ResetPassword" $ \{ code, user } -> React.do
    resetElement /\ setResetElement <- useState' Nothing
    useEffectOnce $ do
      case code of
        Nothing -> setResetElement $ Just $
                     React.fragment
                       [ DOM.text "Vi skickar dig en länk som låter dig skapa ett nytt lösenord."
                       , resetLinkForm { user }
                       ]
        Just c -> launchAff_ do
          -- Check that the recovery code is valid and reserve it for use.
          startResult <- User.startPasswordReset c
          liftEffect $ setResetElement <<< Just =<< case startResult of
            Right _ -> pure $ updateForm { code: c }
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
