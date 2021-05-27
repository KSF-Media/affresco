module KSF.Password.Change where

import Prelude

import Data.Array (cons)
import Data.Either (Either(..))
import Data.Foldable (traverse_)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Validation.Semigroup (invalid, isValid, validation)
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Aff as Aff
import Effect.Class (liftEffect)
import Effect.Class.Console as Console
import Foreign.Object as Object
import KSF.Api (Password(..)) as Api
import KSF.AsyncWrapper as AsyncWrapper
import KSF.InputField as InputField
import KSF.Tracking as Tracking
import KSF.User (User)
import KSF.User as User
import KSF.ValidatableForm (class ValidatableField, ValidatedForm, ValidationError(..), inputFieldErrorMessage, validateField, validatePassword, validatePasswordComparison, validateWithServerErrors)
import React.Basic (JSX)
import React.Basic.Hooks as React
import React.Basic.Hooks (Component, component, useState, (/\))
import React.Basic.DOM as DOM
import React.Basic.DOM.Events (preventDefault, capture_)
import React.Basic.Events as Events

type Props = { user :: Maybe User }

type PasswordReset =
  { password :: Maybe String
  , confirmPassword :: Maybe String
  }
type State =
  { serverErrors :: Array (ValidationError PasswordChangeField)
  , formData :: AsyncWrapper.Progress PasswordReset
  }
data PasswordChangeField
  = Password
  | ConfirmPassword (Maybe String)
derive instance eqPasswordChangeField :: Eq PasswordChangeField
instance validatablePasswordChangeField :: ValidatableField PasswordChangeField where
  validateField field value serverErrors = case field of
    Password -> validateWithServerErrors serverErrors Password value validatePassword
    confirmPw@(ConfirmPassword originalPassword) -> validatePasswordComparison Password confirmPw originalPassword value

updatePasswordForm :: (Api.Password -> Api.Password -> Aff (Either User.UserError Unit)) -> Component Props
updatePasswordForm update = do
  let initialWrapperState = AsyncWrapper.Editing { password: Nothing, confirmPassword: Nothing }
      initialState =
        { serverErrors: mempty
        , formData: initialWrapperState
        }
  component "PasswordReset" $ const React.do
    state /\ setState <- useState initialState
    let setWrapperState :: (AsyncWrapper.Progress PasswordReset -> AsyncWrapper.Progress PasswordReset) -> Effect Unit
        setWrapperState f = setState (\s -> s { formData = f s.formData })
        submitPassword :: PasswordReset -> Effect Unit
        submitPassword pw = Aff.launchAff_ do
          liftEffect $ setWrapperState $ const $ AsyncWrapper.Loading pw
          res <- update (Api.Password $ fromMaybe "" pw.password) (Api.Password $ fromMaybe "" pw.confirmPassword)
          liftEffect $ case res of
            Right _ -> setWrapperState $ const $ AsyncWrapper.Success Nothing
            Left (User.InvalidFormFields errors) -> do
              liftEffect $ handleServerErrs errors
              setWrapperState $ const $ AsyncWrapper.Editing pw
            Left err -> do
              setWrapperState $ const $ AsyncWrapper.Error ""
              Tracking.updateResetPassword $ show err
        handleServerErrs :: User.ValidationServerError -> Effect Unit
        handleServerErrs errs = do
          traverse_ setFormInvalid $ Object.keys errs
        setFormInvalid "password" = setState _ { serverErrors = Invalid Password "Lösenordet måste ha minst 6 tecken." `cons` state.serverErrors }
        setFormInvalid _ = pure unit
    pure $ renderForm submitPassword (setWrapperState $ const initialWrapperState)
      (setWrapperState <<< map) state
  where
    renderForm :: (PasswordReset -> Effect Unit) -> Effect Unit -> ((PasswordReset -> PasswordReset) -> Effect Unit) -> State -> JSX
    renderForm submitPassword resetForm setForm state =
      DOM.div
        { className: "password--reset"
        , children:
            [ DOM.h2_ [ DOM.text "Ändra lösenord" ]
            , AsyncWrapper.asyncWrapper
                { wrapperState: state.formData
                , readyView: mempty
                , editingView: renderEdit
                , loadingView: const $ DOM.div { className: "tiny-spinner" }
                , successView: const renderSuccess
                , errorView: const renderFailure
                }
            ]
        }
      where
        renderEdit :: PasswordReset -> JSX
        renderEdit { password, confirmPassword } =
          DOM.form
            { className: "password--reset"
            , id: "change-password-form"
            , onSubmit: Events.handler preventDefault $ const $ submit validatedForm
            , children:
                [ InputField.inputField
                    { type_: InputField.Password
                    , name: "password"
                    , placeholder: "Önskat lösenord"
                    , value: password
                    , onChange: \newPw -> setForm _ { password = newPw }
                    , label: Just "Önstkat lösenord"
                    , validationError: inputFieldErrorMessage $ validateField Password password state.serverErrors
                    }
                , InputField.inputField
                    { type_: InputField.Password
                    , name: "confirmPassword"
                    , placeholder: "Bekräfta lösenordet"
                    , value: confirmPassword
                    , onChange: \newPw -> setForm _ { confirmPassword = newPw }
                    , label: Just "Bekräfta lösenordet"
                    , validationError: inputFieldErrorMessage $ validateField (ConfirmPassword password) confirmPassword []
                    }
                , DOM.button
                    { type: "submit"
                    , className: "button-green submit-button"
                    , disabled: not $ isValid validatedForm
                    , children: [ DOM.text "Spara" ]
                    }
                ]
            }

        renderSuccess =
          DOM.div
            { className: "success-text"
            , children:
                [ DOM.text "Lösenordet ändrat"
                , DOM.a
                    { href: "/"
                    , children: [ DOM.text "Tillbaka till inloggningssidan" ]
                    }
                ]
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
        validatedForm :: ValidatedForm PasswordChangeField PasswordReset
        validatedForm = case state.formData of
          AsyncWrapper.Editing { password, confirmPassword } ->
            (\pw confirmPw -> { password: pw, confirmPassword: confirmPw })
            <$> validateField Password password state.serverErrors
            <*> validateField (ConfirmPassword password) confirmPassword []
          _ -> invalid $ pure $ InvalidNotInitialized Password

        submit :: ValidatedForm PasswordChangeField PasswordReset -> Effect Unit
        submit =
          validation (\errors -> Console.error "Could not update password.") submitPassword

