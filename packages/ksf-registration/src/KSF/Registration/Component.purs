module KSF.Registration.Component where

import Prelude

import Control.Monad.Error.Class (catchError, throwError)
import Data.Array (zipWith)
import Data.Foldable (traverse_)
import Data.Maybe (Maybe(..), fromMaybe)
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Class.Console as Console
import Foreign.Object (Object)
import Foreign.Object as Object
import Persona as Persona
import React.Basic (JSX, StateUpdate(..), make, send)
import React.Basic as React
import React.Basic.DOM as DOM
import React.Basic.DOM.Events (preventDefault, targetValue)
import React.Basic.Events (handler)
import React.Basic.Extended (Style)
import React.Basic.Extended as React.Extended

foreign import registrationStyles :: Style

type Self = React.Self Props State Void

type Props =
  { onRegister :: Aff Persona.LoginResponse -> Effect Unit
  , onCancelRegistration :: Effect Unit
  }

type State =
  { firstName :: Maybe String
  , lastName :: Maybe String
  , streetAddress :: Maybe String
  , city :: Maybe String
  , country :: Maybe String
  , zip :: Maybe String
  , phone :: Maybe String
  , emailAddress :: Maybe String
  , password :: Maybe String
  , confirmPassword :: Maybe String
  , inputValidations ::
       { password :: Validation
       , passwordConfirm :: Validation
       , emailAddress :: EmailValidation
       }
  }

type RegistrationInputFieldError = Object (Array String)

data RegistrationInputField =
  FirstName
  | LastName
  | StreetAddress
  | City
  | Zip
  | Country
  | Phone
  | EmailAddress
  | Password
  | ConfirmPassword

type InputAttributes =
  { placeholder :: String
  , name :: String
  , onChange :: Maybe String -> Effect Unit
  , value :: Maybe String
  }

type InputType = String

data Validation = Valid | Invalid
derive instance eqValidation :: Eq Validation

data EmailValidation = InUse | Validation Validation

type Pattern = String

data Action =
  UpdateInput RegistrationInputField (Maybe String)
  | PasswordMismatch Validation
  | EmailInUse
  | FormFieldInvalid String

registration :: Props -> JSX
registration = make component { initialState, render, update }

initialState :: State
initialState =
  { firstName: Nothing
  , lastName: Nothing
  , streetAddress: Nothing
  , city: Nothing
  , zip: Nothing
  , country: Just "FI"
  , phone: Nothing
  , emailAddress: Nothing
  , password: Nothing
  , confirmPassword: Nothing
  , inputValidations:
     { password: Valid
     , passwordConfirm: Valid
     , emailAddress: Validation Valid
     }
  }

component :: React.Component Props
component = React.createComponent "Registration"

update :: Self -> Action -> StateUpdate Props State Action
update self = case _ of
  UpdateInput FirstName newValue ->
    Update self.state { firstName = newValue }
  UpdateInput LastName newValue ->
    Update self.state { lastName = newValue }
  UpdateInput StreetAddress newValue ->
    Update self.state { streetAddress = newValue }
  UpdateInput City newValue ->
    Update self.state { city = newValue }
  UpdateInput Zip newValue ->
    Update self.state { zip = newValue }
  UpdateInput Country newValue ->
    Update self.state { country = newValue }
  UpdateInput Phone newValue ->
    Update self.state { phone = newValue }
  UpdateInput EmailAddress newValue ->
    Update self.state { emailAddress = newValue }
  UpdateInput Password newValue ->
    Update self.state { password = newValue }
  UpdateInput ConfirmPassword newValue ->
    Update self.state { confirmPassword = newValue }

  PasswordMismatch validation ->
    Update self.state { inputValidations { passwordConfirm = validation } }
  EmailInUse ->
    Update self.state { inputValidations { emailAddress = InUse } }

  FormFieldInvalid fieldName ->
    case fieldName of
      "emailAddress" -> Update self.state { inputValidations { emailAddress = Validation Invalid } }
      "password"     -> Update self.state { inputValidations { password = Invalid } }
      _ -> NoUpdate

render :: Self -> JSX
render self =
  React.Extended.requireStyle
    registrationStyles
    $ DOM.div
        { className: "registration--container clearfix"
        , children:
            [ form
                [ registrationTitle
                , inputRow
                    (input firstNameInput "Förnamn*")
                    (input lastNameInput "Efternamn*")
                , inputRow
                    (input addressInput "Adress*")
                    (input cityInput "Stad*")
                , inputRow
                    (input zipInput "Postnummer*")
                    (input countryDropdown "Land*")
                , inputRow
                    (input phoneInput "Telefon*")
                    (input emailInput "E-postadress*")
                , inputRow
                    (input passwordInput "Lösenord*")
                    (input confirmPasswordInput "Bekräfta lösenord*")
                , inputRow
                    (halfInputRow [ DOM.text "* = obligatoriskt fält" ])
                    (halfInputRow confirm)
                ]
            ]
        }
  where
    form :: Array JSX -> JSX
    form children =
      DOM.form
        { className: ""
        , children
        , onSubmit: handler preventDefault (\_ -> when canSubmit submit)
        }
      where
        -- We want to only check that the passwords match
        canSubmit = self.state.inputValidations.passwordConfirm == Valid

        submit = do
          let maybeUser = do
                firstName     <- self.state.firstName
                lastName      <- self.state.lastName
                emailAddress  <- self.state.emailAddress
                password      <- self.state.password
                streetAddress <- self.state.streetAddress
                city          <- self.state.city
                zipCode       <- self.state.zip
                country       <- self.state.country
                phone         <- self.state.phone
                pure { firstName, lastName, emailAddress, password, streetAddress, city, zipCode, country, phone }

          case maybeUser of
            Just user -> do
              self.props.onRegister $ Persona.register user `catchError` case _ of
                 err | Just (errData :: Persona.EmailAddressInUseRegistration) <- Persona.errorData err -> do
                         Console.error errData.email_address_in_use_registration.description
                         liftEffect $ send self EmailInUse
                         throwError err
                     | Just (errData :: Persona.InvalidFormFields) <- Persona.errorData err -> do
                         Console.error errData.invalid_form_fields.description
                         liftEffect $ handleErrs errData.invalid_form_fields.errors
                         throwError err
                     | otherwise -> do
                         Console.error "An unexpected error occurred during registration"
                         throwError err
            Nothing -> Console.error "Not all registration fields were filled."

    handleErrs :: RegistrationInputFieldError -> Effect Unit
    handleErrs errs = traverse_ (send self <<< FormFieldInvalid) $ Object.keys errs

    inputFieldUpdate :: RegistrationInputField -> Maybe String -> Effect Unit
    inputFieldUpdate field newInputValue = do
      send self (UpdateInput field newInputValue)

    firstNameInput :: JSX
    firstNameInput =
      createTextInput
        { placeholder: "Förnamn"
        , name: "firstName"
        , onChange: inputFieldUpdate FirstName
        , value: self.state.firstName
        }

    lastNameInput :: JSX
    lastNameInput =
      createTextInput
        { placeholder: "Efternamn"
        , name: "lastName"
        , onChange: inputFieldUpdate LastName
        , value: self.state.lastName
        }

    addressInput :: JSX
    addressInput =
      createTextInput
        { placeholder: "Adress"
        , name: "address"
        , onChange: inputFieldUpdate StreetAddress
        , value: self.state.streetAddress
        }

    cityInput :: JSX
    cityInput =
      createTextInput
        { placeholder: "Stad"
        , name: "city"
        , onChange: inputFieldUpdate City
        , value: self.state.city
        }

    zipInput :: JSX
    zipInput =
      createPatternInput
        { placeholder: "Postnummer"
        , name: "zip"
        , onChange: inputFieldUpdate Zip
        , value: self.state.zip
        }
        zipRegexPattern
      where
        -- Allows only digits
        zipRegexPattern = "\\d+"

    phoneInput :: JSX
    phoneInput =
      createPatternInput
        { placeholder: "Telefon"
        , name: "phone"
        , onChange: inputFieldUpdate Phone
        , value: self.state.phone
        }
        phoneRegexPattern
      where
       -- Allows digits, "+" and whitespaces,
       -- e.g. 040 1231234, +358 04 1231234
       phoneRegexPattern = "[\\d|\\+|\\s]+"

    emailInput :: JSX
    emailInput =
      case self.state.inputValidations.emailAddress of
        Validation Valid   -> emailField
        Validation Invalid -> withValidationError emailInvalidMsg emailField
        InUse              -> withValidationError emailInUseMsg emailField
      where
        emailField =
          createTextInput
            { placeholder: "E-postadress"
            , name: "email"
            , onChange: inputFieldUpdate EmailAddress
            , value: self.state.emailAddress
            }
        emailInUseMsg =
          """E-postadressen är redan i bruk.
             Har du redan ett konto hos Hufvudstadsbladet, Västra Nyland eller Östnyland?
             Då kan du använda samma inloggning.
             Du kan också skapa konto med en annan adress."""
        emailInvalidMsg = "Ogiltig E-postadress."

    passwordInput :: JSX
    passwordInput =
      case self.state.inputValidations.password of
        Valid   -> passwordField
        Invalid -> withValidationError passwordInvalidMsg passwordField
      where
        passwordField =
          DOM.input
            { type: "password"
            , placeholder: "Lösenord"
            , name: "password"
            , required: true
            , onChange: handler targetValue $ inputFieldUpdate Password
            , value: fromMaybe "" self.state.password
            , pattern: ".{6,}"
            }
        -- TODO: Probably not always the case.
        -- The problem of displaying error message directly from Janrain
        -- is for example the language used (e.g. Swedish for Finnish users).
        -- Also, the error might be gibberish.
        passwordInvalidMsg = "Lösenordet måste ha minst 6 tecken."

    confirmPasswordInput :: JSX
    confirmPasswordInput =
      case self.state.inputValidations.passwordConfirm of
        Valid   -> confirmPasswordField
        Invalid -> withValidationError passwordMismatchMsg confirmPasswordField
      where
        confirmPasswordField =
          DOM.input
            { type: "password"
            , required: true
            , onBlur: handler targetValue comparePasswords
            , placeholder: "Bekräfta lösenord"
            , name: "confirm-password"
            , onChange: handler targetValue $ inputFieldUpdate ConfirmPassword
            , value: fromMaybe "" self.state.confirmPassword
            }

        comparePasswords confirmedPassword
          | confirmedPassword /= self.state.password = send self (PasswordMismatch Invalid)
          | otherwise = send self (PasswordMismatch Valid)

        passwordMismatchMsg = "Lösenorden överensstämmer inte med varandra."

    withValidationError :: String -> JSX -> JSX
    withValidationError msg inputField =
      DOM.div
        { className: "registration--invalid-form-field"
        , children:
            [ inputField
            , DOM.div
                { className: "mt1 registration--invalid-form-text"
                , children: [ DOM.text msg ]
                }
            ]
        }

    countryDropdown :: JSX
    countryDropdown =
      dropdown
        [ "FI", "AX", "SV", "NO", "DK" ]
        [ "Finland", "Åland", "Sverige", "Norge", "Danmark" ]

    dropdown :: Array String -> Array String -> JSX
    dropdown options descriptions =
      DOM.select
        { className: ""
        , children: zipWith createOption options descriptions
        , onChange: handler targetValue $ inputFieldUpdate Country
        , value: fromMaybe "FI" self.state.country
        }
      where
        createOption value description =
          DOM.option
            { value
            , children: [ DOM.text description ]
            }

    confirm :: Array JSX
    confirm =
      [ acceptTermsText
      , confirmButton
      , cancelText
      ]

    confirmButton :: JSX
    confirmButton =
      DOM.input
        { type: "submit"
        , className: "registration--create-button mt2"
        , value: "Skapa konto"
        }

    acceptTermsText :: JSX
    acceptTermsText =
      DOM.div
        { className: "registration--accept-terms"
        , children:
            [ DOM.text "Genom att klicka på \"Fortsätt\", accepterar du våra "
            , DOM.a
                { href: termsUrl
                , target: "_blank"
                , children: [ DOM.text "användarvillkor" ]
                }
            , DOM.text " och bekräftar att ha läst och förstått vår "
            , DOM.a
                { href: privacyPolicyUrl
                , target: "_blank"
                , children: [ DOM.text "integritetspolicy." ]
                }
            ]
        }
      where
        termsUrl = "https://www.hbl.fi/bruksvillkor/?_ga=2.33626213.557863145.1547627789-663578572.1543831809#terms"
        privacyPolicyUrl = "https://www.hbl.fi/bruksvillkor/?_ga=2.233133274.557863145.1547627789-663578572.1543831809#privacy"

    cancelText :: JSX
    cancelText =
      DOM.div
        { className: "mt2"
        , children:
            [ DOM.text "Eller "
            , DOM.a
                { href: ""
                , onClick: handler preventDefault (\_ -> self.props.onCancelRegistration)
                , children: [ DOM.text "avbryt." ]
                }
            ]
        }

registrationTitle :: JSX
registrationTitle =
  DOM.div
    { className: "col-12 mx-auto"
    , children:
        [ DOM.h1_ [ DOM.text "Skapa din konto" ] ]
    }

inputRow :: JSX -> JSX -> JSX
inputRow leftInput rightInput =
  DOM.div
    { className: "clearfix flex justify-around registration--input-row"
    , children: [ leftInput, rightInput ]
    }

input :: JSX -> String -> JSX
input inputField label =
  halfInputRow
    [ DOM.div
        { className: "registration--input-label"
        , children: [ DOM.text label ]
        }
    , inputField
    ]

halfInputRow :: Array JSX -> JSX
halfInputRow children =
  DOM.div
    { className: "col sm-col-12 md-col-5 registration--input"
    , children
    }

createTextInput :: InputAttributes -> JSX
createTextInput inputAttrs =
  createInput inputAttrs "text"

createEmailInput :: InputAttributes -> JSX
createEmailInput inputAttrs =
  createInput inputAttrs "email"

-- TODO: Validation error message to the user is not specific
createPatternInput :: InputAttributes -> Pattern -> JSX
createPatternInput { placeholder, name, onChange } pattern =
  DOM.input
    { type: "text"
    , required: true
    , onChange: handler targetValue onChange
    , placeholder
    , name
    , pattern
    }

createInput :: InputAttributes -> InputType -> JSX
createInput { placeholder, name, onChange, value } type_ =
  DOM.input
    { type: type_
    , required: true
    , onChange: handler targetValue onChange
    , placeholder
    , name
    , value: fromMaybe "" value
    }
