module Prenumerera.Register where

import Prelude

import Control.Alt ((<|>))
import Data.Either (Either(..))
import Data.List.NonEmpty (all)
import Data.Foldable
import Data.Maybe (Maybe(..), isNothing, maybe)
import Data.Monoid (guard)
import Data.Nullable (toMaybe)
import Data.Validation.Semigroup (toEither)
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Aff as Aff
import Effect.Class (liftEffect)
import KSF.InputField as InputField
import KSF.Registration.Component (RegistrationInputField(..))
import KSF.Registration.Component as Registration
import KSF.User (User, UserUpdate(..))
import KSF.User as User
import KSF.User.Login as Login
import KSF.ValidatableForm (ValidatedForm, isNotInitialized)
import Prenumerera.Package (Package)
import React.Basic (JSX)
import React.Basic.DOM as DOM
import React.Basic.DOM.Events (preventDefault)
import React.Basic.Events (handler)
import React.Basic.Hooks (Component, useState, useState', (/\))
import React.Basic.Hooks as React

type Props =
  { user :: Maybe User
  , setUser :: Maybe User -> Effect Unit
  , package :: Package
-- TODO: Why doesn't this work?
--  , withSpinner :: forall a. Aff a -> Aff a
  , withSpinnerUnit :: Aff Unit -> Aff Unit
  , withSpinnerUser :: Aff User -> Aff User
  , next :: Effect Unit
  , cancel :: Effect Unit
  }

type RegisterData =
  { existingUser :: Boolean
  , form :: Registration.State
  }

initialRegisterData :: Maybe User -> RegisterData
initialRegisterData Nothing =
  { existingUser: false
  , form: Registration.initialState { usePhone = false }
  }

initialRegisterData (Just user) =
  { existingUser: true
  , form:
      { formData:
          { emailAddress: Just user.email
          , firstName: toMaybe user.firstName
          , lastName: toMaybe user.lastName
          , streetAddress: _.streetAddress <$> address
          , zipCode: (toMaybe <<< _.zipCode) =<< address
          , city: (toMaybe <<< _.city) =<< address
          , country: (_.countryCode <$> address) <|> Just "FI"
          , phone: Nothing
            -- Not displayed if existing account
          , password: Nothing
          , confirmPassword: Nothing
          }
      , serverErrors: []
      , usePhone: false
      }
  }
  where
    address = toMaybe user.address

component :: Component Props
component = do
  login <- Login.login
  React.component "Register" $ \ { user, setUser, package, withSpinnerUnit, withSpinnerUser, next, cancel } -> React.do
    let loginForm =
          login
            { onMerge: pure unit
            , onMergeCancelled: pure unit
            , onRegister: pure unit
            , onRegisterCancelled: pure unit
            , onUserFetch: userFetched
            , onLogin: Aff.launchAff_ <<< withSpinnerUnit
            , disableSocialLogins: mempty
            }
        userFetched (Right u) = setUser $ Just u
        userFetched (Left _) = pure unit
        initial = initialRegisterData user
    loginScreen /\ setLoginScreen <- useState' $ isNothing user
    registerData /\ setRegisterData <- useState initial
    createError /\ setCreateError <- useState' false
    let onSubmit form = do
          let registerNew = Registration.submitForm registerData.form (setFormState setRegisterData)
                            (Aff.runAff_ userCreate <<< withSpinnerUser) form
              update = updateUser initial.form.formData registerData.form.formData (setCreateError true) next
          maybe registerNew update user
        userCreate (Right u) = do
          setUser $ Just u
          next
        userCreate (Left _) = do
          setCreateError true
    pure $ render package $
      if createError then renderError registerData.existingUser else
        if loginScreen
        then renderLogin loginForm $ setLoginScreen false
        else renderRegister registerData setRegisterData onSubmit cancel

render :: Package -> JSX -> JSX
render package content =
  DOM.div
    { className: "container"
    , children:
        [ DOM.h4_ [ DOM.text $ "Din beställning: " <> package.name ]
        , DOM.div
            { id: "ksf-identify"
            , children:
                [ DOM.div
                    { className: "ksf-auth-wrapper"
                    , children:
                        [ DOM.div
                            { className: "ksf-auth-content"
                            , children: [ content ]
                            }
                        ]
                    }
                ]
            }
        ]
    }

renderError :: Boolean -> JSX
renderError false =
  DOM.text "Något gick fel."
renderError true =
  DOM.text "Kunde inte uppdateras."

renderLogin :: JSX -> Effect Unit -> JSX
renderLogin content startRegister =
  DOM.div
    { id: "ksf-identify-login"
    , children:
        [ DOM.div
            { className: "ksf-auth-header"
            , children:
              -- TODO add image
                [ DOM.h2
                    { className: "dynamicloginPrompt"
                    , id: "loginPrompt"
                    , children: [ DOM.text "Registrera dig eller logga in för att beställa" ]
                    }
                , DOM.p
                    { className: "gift-disclaimer"
                    , children:
                        [ DOM.text "Önskar du ge tidningen i gåva? Kontakta kundservice "
                        , DOM.a { href: "mailto:pren@ksfmedia.fi"
                                , children: [ DOM.text "pren@ksfmedia.fi" ]
                                }
                        , DOM.text " eller på numret "
                        , DOM.a { href: "tel:+35891253500"
                                , children: [ DOM.text "09 125 35 00" ]
                                }
                        , DOM.text " (vardagar kl. 8-12 och 13-16)."
                        ]
                    }
                ]
            }
        , content
        , DOM.div
            { className: "no-account"
            , children:
                [ DOM.text "Ny kund? "
                , DOM.a
                    { href: "#"
                    , children: [ DOM.text "Registrera dig" ]
                    , onClick: handler preventDefault $ const startRegister
                    }
                , DOM.text "!"
                ]
            }
        ]
    }

renderRegister :: RegisterData -> ((RegisterData -> RegisterData) -> Effect Unit) -> (ValidatedForm RegistrationInputField Registration.FormData -> Effect Unit) -> Effect Unit -> JSX
renderRegister reg@{ form } setState save cancel =
  DOM.div
    { id: "ksf-registration-form"
    , className: "ksf-auth-section"
    , children:
        [ if reg.existingUser
            then DOM.div
                   { className: "ksf-auth-header"
                   , children: [ DOM.h2_ [ DOM.text "Kontrollera konto" ] ]
                   }
            else mempty
        , DOM.div
            { className: "janrain-api-container ksf-janrain-user-form"
            , children:
                [ DOM.form
                    { className: "janrain-api-form user-form"
                    , onSubmit: handler preventDefault $ const $ save $ Registration.formValidations form
                    , children:
                        (if reg.existingUser then [ DOM.h3_ [ DOM.text "Din information" ] ] else []) <>
                        [ row [ inputField FirstName, inputField LastName ]
                        , row [ inputField StreetAddress, inputField City ]
                        , row [ inputField (Zip (form.formData.country)), inputField Country ]
                        , row [ inputField EmailAddress ]
                        ] <>
                        (guard (not reg.existingUser) $
                         [ row [ inputField Password, inputField (ConfirmPassword form.formData.password) ] ]
                        ) <> [ row [ DOM.div {className: "input-field--container"} , submit ] ]
                    }
                ]
            }
        ]
    }
  where
    row xs =
      DOM.div
        { className: "row"
        , children: xs
        }

    inputField :: RegistrationInputField -> JSX
    inputField EmailAddress = case reg.existingUser of
      false -> Registration.inputField EmailAddress form $ setFormState setState
      true -> InputField.inputField
        { type_: InputField.Email
        , label: Just "E-postadress"
        , placeholder: "E-postadress"
        , name: "email"
        , value: form.formData.emailAddress
        , onChange: const $ pure unit
        , validationError: Nothing
        , disabled: true
        }
    inputField field = Registration.inputField field form $ setFormState setState
    submit =
      DOM.div_
        [ if not reg.existingUser then disclaimer else mempty
        , DOM.input
            { type: "submit"
            , className: "submit-button"
            , value: "Fortsätt"
            , disabled: isFormInvalid
            }
        , DOM.label
            { className: "txt-cancel"
            , children:
                [ DOM.text "eller "
                , DOM.a { href: "/", children: [ DOM.text "avbryt" ] }
                ]
            , onClick: handler preventDefault $ const cancel
            }
        ]
    disclaimer =
      DOM.div
        { className: "disclaimer"
        , children:
            [ DOM.text "Genom att klicka på \"fortsätt\", accepterar du våra "
            , DOM.a
                { href: "https://www.hbl.fi/bruksvillkor/#terms"
                , target: "_blank"
                , children: [ DOM.text "användarvillkor" ]
                }
            , DOM.text " och bekräftar att ha läst och förstått vår "
            , DOM.a
                { href: "https://www.hbl.fi/bruksvillkor/#privacy"
                , target: "_blank"
                , children: [ DOM.text "integritetspolicy" ]
                }
            , DOM.text "."
            ]
        }
    isFormInvalid
      | Left errs <- toEither $ Registration.formValidations form
      = not $ all isNotInitialized errs
      | otherwise = false

setFormState :: ((RegisterData -> RegisterData) -> Effect Unit) -> (Registration.State -> Registration.State) -> Effect Unit
setFormState setState f = setState $ \s -> s { form = f s.form }

updateUser :: Registration.FormData -> Registration.FormData -> Effect Unit -> Effect Unit -> User -> Effect Unit
updateUser orig new updateError next user = do
  let nameChanged = orig.firstName /= new.firstName || orig.lastName /= new.lastName
      addressChanged = orig.streetAddress /= new.streetAddress ||
                       orig.zipCode /= new.zipCode ||
                       orig.country /= new.country
      afterUpdate (Left _) = updateError
      afterUpdate (Right (Left _)) = updateError
      afterUpdate _ = next
      update = case nameChanged, addressChanged of
                 false, false -> Nothing
                 true, false  -> UpdateName <$>
                                 ({ firstName: _, lastName: _ }
                                   <$> new.firstName
                                   <*> new.lastName)
                 false, true  -> UpdateAddress <$>
                                 ({ countryCode: _, zipCode: _, streetAddress: _, startDate: Nothing }
                                   <$> new.country
                                   <*> new.zipCode
                                   <*> new.streetAddress)
                 true, true   -> UpdateFull <$>
                                 ({ firstName: _, lastName: _, countryCode: _, zipCode: _, streetAddress: _, city:_, startDate: Nothing }
                                   <$> new.firstName
                                   <*> new.lastName
                                   <*> new.country
                                   <*> new.zipCode
                                   <*> new.streetAddress
                                   <*> new.city)
                 _, _ -> Nothing
  maybe next (Aff.runAff_ afterUpdate <<< User.updateUser user.uuid) update
