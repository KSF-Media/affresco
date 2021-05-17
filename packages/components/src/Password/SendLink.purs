module KSF.Password.SendLink where

import Prelude

import Data.Either (Either(..))
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Validation.Semigroup (invalid, isValid, validation)
import Effect (Effect)
import Effect.Aff as Aff
import Effect.Class (liftEffect)
import Effect.Class.Console as Console
import KSF.AsyncWrapper as AsyncWrapper
import KSF.InputField as InputField
import KSF.User (User)
import KSF.User as User
import KSF.ValidatableForm (class ValidatableField, ValidatedForm, ValidationError(..), inputFieldErrorMessage, validateEmailAddress, validateField)
import React.Basic (JSX)
import React.Basic.Hooks as React
import React.Basic.Hooks (Component, component, useState', (/\))
import React.Basic.DOM as DOM
import React.Basic.DOM.Events (capture_, preventDefault)
import React.Basic.Events as Events

type Props = { user :: Maybe User }

data EmailField = EmailField
derive instance eqEmailField :: Eq EmailField
instance validateEmailField :: ValidatableField EmailField where
  validateField field value _serverErrors = case field of
    EmailField -> validateEmailAddress field value

requestResetLink :: Component Props
requestResetLink = do
  component "RequestPasswordReset" \ { user } -> React.do
    let initialState = AsyncWrapper.Editing $ _.email <$> user
    wrapperState /\ setWrapperState <- useState' initialState
    let submitForm :: String -> Effect Unit
        submitForm email = do
          setWrapperState $ AsyncWrapper.Loading $ Just email
          Aff.launchAff_ do
            res <- User.requestPasswordReset email
            liftEffect $ case res of
              Right unit -> setWrapperState $ AsyncWrapper.Success Nothing
              Left err -> setWrapperState $ AsyncWrapper.Error err
    pure $ renderForm submitForm (setWrapperState initialState)
      (setWrapperState <<< AsyncWrapper.Editing) wrapperState
  where
    renderForm :: (String -> Effect Unit) -> Effect Unit -> (Maybe String -> Effect Unit) -> AsyncWrapper.Progress (Maybe String) -> JSX
    renderForm submitEmail resetForm setEmail wrapperState =
      AsyncWrapper.asyncWrapper
        { wrapperState
        , readyView: mempty
        , editingView: renderEdit
        , loadingView: const $ DOM.div { className: "tiny-spinner" }
        , successView: const renderSuccess
        , errorView: const renderFailure
        }
      where
        renderEdit :: Maybe String -> JSX
        renderEdit email =
          DOM.form
            { className: "password--reset"
            , id: "forgot-password-form"
            , onSubmit: Events.handler preventDefault $ const $ submit validatedForm
            , children:
                [ DOM.div
                    { className: "password--reset-field"
                    , children:
                        [ InputField.inputField
                            { type_: InputField.Email
                            , name: "email"
                            , placeholder: "E-post"
                            , value: email
                            , onChange: setEmail
                            , label: Just "E-post"
                            , validationError: inputFieldErrorMessage $ validateField EmailField email []
                            }
                        , DOM.button
                            { type: "submit"
                            , className: "button-green submit-button"
                            , disabled: not $ isValid validatedForm
                            , children: [ DOM.text "Skicka" ]
                            }
                        ]
                    }
                ]
            }

        renderSuccess =
          DOM.div
            { className: "success-text"
            , children: [ DOM.text "Vi har skickat ett e-post med instruktioner för att skapa ett nytt lösenord." ]
            }

        renderFailure =
          DOM.div
            { className: "error-text"
            , children: [ DOM.text "Något gick fel."
                        , DOM.a
                            { href: "/#lösenord"
                            , onClick: capture_ resetForm
                            , children: [ DOM.text "Försök igen." ]
                            }
                        ]
            }

        validatedForm :: ValidatedForm EmailField (Maybe String)
        validatedForm = case wrapperState of
          AsyncWrapper.Editing state ->
            validateField EmailField state []
          _ -> invalid $ pure $ InvalidNotInitialized EmailField

        submit :: ValidatedForm EmailField (Maybe String) -> Effect Unit
        submit =
          validation (\errors -> Console.error "Invalid email.") (submitEmail <<< fromMaybe "")
