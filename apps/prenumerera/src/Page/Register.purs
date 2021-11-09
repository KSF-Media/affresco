module Prenumerera.Page.Register where

import Prelude

import Control.Alt ((<|>))
import Data.Either (Either(..))
import Data.List.NonEmpty (all)
import Data.Maybe (Maybe(..), isNothing, maybe)
import Data.Monoid (guard)
import Data.Nullable (toMaybe)
import Data.Validation.Semigroup (toEither, validation)
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Aff as Aff
import Effect.Exception as Exception
import KSF.InputField as InputField
import KSF.Registration.Component (RegistrationInputField(..))
import KSF.Registration.Component as Registration
import KSF.Spinner as Spinner
import KSF.User (User, UserUpdate(..))
import KSF.User as User
import KSF.User.Login as Login
import KSF.ValidatableForm (ValidatedForm, isNotInitialized)
import Prenumerera.Package (Package)
import Prenumerera.Package.Description (Description)
import React.Basic (JSX)
import React.Basic.DOM as DOM
import React.Basic.DOM.Events (preventDefault)
import React.Basic.Events (handler)
import React.Basic.Hooks (Component, useEffect, useState, useState', (/\))
import React.Basic.Hooks as React

type Props =
  { user :: Maybe User
  , setUser :: Maybe User -> Effect Unit
  , package :: Package
  , description :: Description
  , scrollToTop :: Effect Unit
  , next :: User -> Effect Unit
  , cancel :: Effect Unit
  }

type RegisterData =
  { existingUser :: Boolean
  , form :: Registration.State
  }

initialRegisterData :: Boolean -> Maybe User -> RegisterData
initialRegisterData digitalOnly Nothing =
  { existingUser: false
  , form: Registration.initialState { digitalOnly = digitalOnly }
  }

initialRegisterData digitalOnly (Just user) =
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
            -- Not displayed or used if existing account, but still
            -- need something since we share validations with
            -- register.
          , password: Just "onödig"
          , confirmPassword: Just "onödig"
          }
      , serverErrors: []
      , digitalOnly
      }
  }
  where
    address = toMaybe user.address

component :: Component Props
component = do
  login <- Login.login
  React.component "Register" $ \ { user, setUser, package, description, scrollToTop, next, cancel } -> React.do
    loading /\ setLoading' <- useState' Nothing
    let initialAtInit = initialRegisterData package.digitalOnly user
    allowLateLogin /\ setAllowLateLogin <- useState' $ isNothing user
    initial /\ setInitial <- useState' initialAtInit
    registerData /\ setRegisterData <- useState initial
    loginScreen /\ setLoginScreen <- useState' $ isNothing user
    let withSpinner :: forall a. Aff a -> Aff a
        withSpinner = Spinner.withSpinner setLoading'
        loginForm =
          login
            { onMerge: pure unit
            , onMergeCancelled: pure unit
            , onRegister: pure unit
            , onRegisterCancelled: pure unit
            , onUserFetch: userFetched
            , onLogin: Aff.launchAff_ <<< withSpinner
            , disableSocialLogins: mempty
            }
        userFetched (Right u) = do
          let initialAfterLogin = initialRegisterData package.digitalOnly $ Just u
          setInitial initialAfterLogin
          setRegisterData $ const initialAfterLogin
          scrollToTop
          setLoginScreen false
          setAllowLateLogin false
          setUser $ Just u
        userFetched (Left _) = do
          pure unit
    createError /\ setCreateError <- useState' false
    let onSubmit form = do
          let registerNew = Registration.submitForm registerData.form (setFormState setRegisterData)
                            (Aff.runAff_ userCreate <<< withSpinner) form
              update = flip (updateUser
                             initial.form.formData
                             (setFormData setRegisterData)
                             (scrollToTop *> setCreateError true)
                             next) form
          maybe registerNew update user
        userCreate (Right u) = do
          next u
        userCreate (Left err) = do
          case Exception.message err of
            -- Do nothing, createUser should have marked the fields already
            "email in use" -> pure unit
            "invalid form fields" -> pure unit
            _ -> setCreateError true
    -- Login may have succeeded asynchronously via magiclogin when
    -- this component was already open
    useEffect (isNothing user /\ allowLateLogin) do
      case user of
        Nothing -> pure unit
        Just u -> when allowLateLogin $ userFetched $ Right u
      pure $ pure unit
    pure $ case loading of
      Nothing -> render description $
                 if createError then renderError registerData.existingUser else
                   if loginScreen
                     then renderLogin loginForm $ (scrollToTop *> setLoginScreen false)
                     else renderRegister registerData setRegisterData onSubmit cancel
      Just Spinner.Loading -> Spinner.loadingSpinner

render :: Description -> JSX -> JSX
render description content =
  DOM.div
    { className: "container"
    , children:
        [ DOM.h4_ [ DOM.text $ "Din beställning: " <> description.brandLong <> description.descriptionShort ]
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
                        ] <>
                        (guard (not form.digitalOnly) $
                         [ row [ inputField StreetAddress, inputField (Zip (form.formData.country)) ]
                         , row [ inputField City, inputField Country ]
                         ]) <>
                        [ row [ inputField EmailAddress ]
                        ] <>
                        (guard (not reg.existingUser) $
                         [ row [ inputField Password, inputField (ConfirmPassword form.formData.password) ] ]
                        ) <> [ row [ DOM.div {className: "input-field--container"} , submit ]
                             , row []
                             ]
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

setFormData :: ((RegisterData -> RegisterData) -> Effect Unit) -> (Registration.FormData -> Registration.FormData) -> Effect Unit
setFormData setState f = setFormState setState $ \s -> s { formData = f s.formData }

updateUser
  :: Registration.FormData
  -> ((Registration.FormData -> Registration.FormData) -> Effect Unit)
  -> Effect Unit
  -> (User -> Effect Unit)
  -> User
  -> ValidatedForm Registration.RegistrationInputField Registration.FormData
  -> Effect Unit
updateUser orig setForm updateError next user = validation
  (\_ -> setForm _
           { firstName       = orig.firstName       <|> Just ""
           , lastName        = orig.lastName        <|> Just ""
           , streetAddress   = orig.streetAddress   <|> Just ""
           , city            = orig.city            <|> Just ""
           , zipCode         = orig.zipCode         <|> Just ""
           , country         = orig.country         <|> Just ""
           , password        = orig.password        <|> Just ""
           , confirmPassword = orig.confirmPassword <|> Just ""
           }
  )
  \new -> do
    let nameChanged = orig.firstName /= new.firstName || orig.lastName /= new.lastName
        addressChanged = orig.streetAddress /= new.streetAddress ||
                         orig.zipCode /= new.zipCode ||
                         orig.country /= new.country
        afterUpdate (Left _) = updateError
        afterUpdate (Right (Left _)) = updateError
        afterUpdate (Right (Right u)) = next u
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
                                   ({ firstName: _, lastName: _, countryCode: _, zipCode: _, streetAddress: _, city:_, startDate: Nothing, phone: toMaybe user.phone }
                                     <$> new.firstName
                                     <*> new.lastName
                                     <*> new.country
                                     <*> new.zipCode
                                     <*> new.streetAddress
                                     <*> new.city)
    maybe (next user) (Aff.runAff_ afterUpdate <<< User.updateUser user.uuid) update
