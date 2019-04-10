module KSF.Registration.Component where

import Prelude

import Control.Alt ((<|>))
import Control.Monad.Error.Class (catchError, throwError)
import Data.Array (all, any, foldl, snoc, zipWith)
import Data.Either (Either(..))
import Data.Foldable (traverse_)
import Data.Maybe (Maybe(..), fromMaybe, isNothing)
import Data.String (length)
import Data.String.Regex as Regex
import Data.String.Regex.Flags as Regex.Flags
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Class.Console as Console
import Foreign.Object (Object)
import Foreign.Object as Object
import Persona as Persona
import React.Basic (JSX, StateUpdate(..), make, runUpdate)
import React.Basic as React
import React.Basic.DOM as DOM
import React.Basic.DOM.Events (preventDefault, targetValue)
import React.Basic.Events (EventHandler, handler, handler_)
import React.Basic.Extended (Style)
import React.Basic.Extended as React.Extended

foreign import registrationStyles :: Style

type Self = React.Self Props State

type Props =
  { onRegister :: Aff Persona.LoginResponse -> Effect Unit
  , onCancelRegistration :: Effect Unit
  }

type InputField =
  { value :: Maybe String
  , validationError :: Maybe ValidationError
  , validationFns :: Array ValidationFn
  }

type State =
  { firstName          :: InputField
  , lastName           :: InputField
  , streetAddress      :: InputField
  , city               :: InputField
  , country            :: InputField
  , zip                :: InputField
  , phone              :: InputField
  , emailAddress       :: InputField
  , password           :: InputField
  , confirmPassword    :: InputField
  }

type RegistrationInputFieldError = Object (Array String)

data RegistrationInputField
  = FirstName
  | LastName
  | StreetAddress
  | City
  | Zip
  | Country
  | Phone
  | EmailAddress
  | Password
  | ConfirmPassword
derive instance eqRegistrationInputField :: Eq RegistrationInputField
derive instance ordRegistrationInputField :: Ord RegistrationInputField

type ValidationFn = Maybe String -> Maybe ValidationError

data ValidationError
  = Invalid String
  | InvalidEmpty String
  | InvalidPatternFailure String
  | InvalidEmailInUse String
  | InvalidNotInitialized -- Fictional state only to be set when the form is first rendered
derive instance eqValidationError :: Eq ValidationError

data Action
  = UpdateInput RegistrationInputField (Maybe String)
  | FormFieldValidation RegistrationInputField
  | SetServerValidationError RegistrationInputField ValidationError

registration :: Props -> JSX
registration = make component { initialState, render }

initialState :: State
initialState =
  { firstName: emptyField { validationFns = [validateEmptyField FirstName firstNameMissing] }
  , lastName: emptyField { validationFns = [validateEmptyField LastName lastNameMissing] }
  , streetAddress: emptyField { validationFns = [validateEmptyField StreetAddress streetAddressMissing] }
  , city: emptyField { validationFns = [validateEmptyField City cityMissing] }
  , zip: emptyField { validationFns = [validateEmptyField Zip zipMissing, validateInputWithRegex Zip zipRegexPattern zipRegexFailure] }
  , country: emptyField { value = Just "FI", validationError = Nothing }
  , phone: emptyField { validationFns = [validateEmptyField Phone phoneMissing, validateInputWithRegex Phone phoneRegexPattern phoneRegexFailure] }
  , emailAddress: emptyField { validationFns = [validateEmptyField EmailAddress emailMissing, validateInputWithRegex EmailAddress emailRegex emailRegexFailure] }
  , password: emptyField { validationFns = [validatePassword] }
  , confirmPassword: emptyField { validationFns = [validateEmptyField ConfirmPassword passwordMissing] }
  }
  where
    emptyField :: InputField
    emptyField = { value: Nothing, validationError: Just $ InvalidNotInitialized, validationFns: [] }

    -- Allows word characters (a-z, A-Z, 0-9, _), dashes and whitespaces
    zipRegexPattern = "^[\\s|\\w|-]+$"
    zipRegexFailure = "Postnummerfältet kan bara innehålla siffror och bokstäver."

    -- Allows digits, "+" and whitespaces,
    -- e.g. 040 1231234, +358 04 1231234
    phoneRegexPattern = "^[\\d|\\+|\\s|-|\\(|\\)]+$"
    phoneRegexFailure = "Telefonnummer kan bara bestå av siffror, mellanslag och +-tecken."

    -- From https://developer.mozilla.org/en-US/docs/Web/HTML/Element/input/email#Basic_validation
    emailRegex = "^[a-zA-Z0-9.!#$%&'*+\\/=?^_`{|}~-]+@[a-zA-Z0-9](?:[a-zA-Z0-9-]{0,61}[a-zA-Z0-9])?(?:\\.[a-zA-Z0-9](?:[a-zA-Z0-9-]{0,61}[a-zA-Z0-9])?)*$"
    emailRegexFailure = emailInvalidMsg

    firstNameMissing     = "Förnamn krävs."
    lastNameMissing      = "Efternamn krävs."
    streetAddressMissing = "Adress krävs."
    cityMissing          = "Stad krävs."
    zipMissing           = "Postnummer krävs."
    phoneMissing         = "Telefon krävs."
    emailMissing         = "E-postadress krävs."
    passwordMissing      = "Lösenord krävs."

emailInvalidMsg :: String
emailInvalidMsg = "Ogiltig E-postadress."

emailInUseMsg :: String
emailInUseMsg =
  """E-postadressen är redan i bruk.
     Har du redan ett konto hos Hufvudstadsbladet, Västra Nyland eller Östnyland?
     Då kan du använda samma inloggning.
     Du kan också skapa konto med en annan adress."""

component :: React.Component Props
component = React.createComponent "Registration"

update :: Self -> Action -> StateUpdate Props State
update self = case _ of
  UpdateInput attr newValue -> Update do
    case attr of
      FirstName       -> self.state { firstName { value = newValue } }
      LastName        -> self.state { lastName { value = newValue } }
      StreetAddress   -> self.state { streetAddress { value = newValue } }
      City            -> self.state { city { value = newValue } }
      Zip             -> self.state { zip { value = newValue } }
      Country         -> self.state { country { value = newValue } }
      Phone           -> self.state { phone { value = newValue } }
      EmailAddress    -> self.state { emailAddress { value = newValue } }
      Password        -> self.state { password { value = newValue } }
      ConfirmPassword -> self.state { confirmPassword { value = newValue } }

  FormFieldValidation fieldName ->
    let validate :: InputField -> Maybe ValidationError
        validate inputField =
          foldl (<|>) Nothing $ map (_ $ inputField.value) inputField.validationFns

    in Update case fieldName of
      FirstName       -> self.state { firstName { validationError = validate self.state.firstName } }
      LastName        -> self.state { lastName { validationError = validate self.state.lastName } }
      StreetAddress   -> self.state { streetAddress { validationError = validate self.state.streetAddress } }
      City            -> self.state { city { validationError = validate self.state.city } }
      Zip             -> self.state { zip { validationError = validate self.state.zip } }
      Country         -> self.state { country { validationError = validate self.state.country } }
      Phone           -> self.state { phone { validationError = validate self.state.phone } }
      EmailAddress    -> self.state { emailAddress { validationError = validate self.state.emailAddress } }
      Password        -> self.state { password { validationError = validate self.state.password } }
      ConfirmPassword ->
        -- Adds a validation function that compares the confirmed password input with the password input
        let confirmPasswordWithCompareFn = self.state.confirmPassword { validationFns = self.state.confirmPassword.validationFns `snoc` comparePasswords self.state.password.value }
        in self.state { confirmPassword { validationError = validate confirmPasswordWithCompareFn } }

  -- For setting the invalid states according to response from the server
  SetServerValidationError fieldName invalidState ->
    case fieldName of
      EmailAddress -> Update self.state { emailAddress { validationError = Just $ invalidState } }
      Password     -> Update self.state { password { validationError = Just $ invalidState } }
      _            -> NoUpdate

validateEmptyField :: RegistrationInputField -> String -> Maybe String -> Maybe ValidationError
validateEmptyField fieldName validationErrorMsg fieldValue
  | Just value <- fieldValue, length value > 0 = Nothing
  | otherwise = Just $ InvalidEmpty validationErrorMsg

validateInputWithRegex :: RegistrationInputField -> String -> String -> Maybe String -> Maybe ValidationError
validateInputWithRegex fieldName regexString errMsg fieldInput
  | Just inputValue <- fieldInput
  , Right regexPattern <- Regex.regex regexString Regex.Flags.noFlags
  , Regex.test regexPattern inputValue
  = Nothing
  | otherwise = Just $ InvalidPatternFailure errMsg

-- | Only checks that password is minimum of 6 characters long
validatePassword :: Maybe String -> Maybe ValidationError
validatePassword password
  | Just pw <- password
  , length pw >= 6
  = Nothing
  | otherwise = Just $ Invalid passwordInvalidMsg

comparePasswords :: Maybe String -> Maybe String -> Maybe ValidationError
comparePasswords password confirmedPassword
  | confirmedPassword /= password = Just $ Invalid "Lösenorden överensstämmer inte med varandra."
  | otherwise = Nothing

-- TODO: Probably not always the case.
-- The problem of displaying error message directly from Janrain
-- is for example the language used (e.g. Swedish for Finnish users).
-- Also, the error might be gibberish.
passwordInvalidMsg :: String
passwordInvalidMsg = "Lösenordet måste ha minst 6 tecken."

validationErrorMessageOf :: ValidationError -> String
validationErrorMessageOf = case _ of
  Invalid err -> err
  InvalidEmpty err -> err
  InvalidPatternFailure err -> err
  InvalidEmailInUse err -> err
  InvalidNotInitialized -> ""

send :: Self -> Action -> Effect Unit
send = runUpdate update

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
        , onSubmit: handler preventDefault (\_ -> do
                                               validateForm
                                               submit)
        }
      where
        submit = when isFormValid do
          let maybeUser = do
                firstName       <- self.state.firstName.value
                lastName        <- self.state.lastName.value
                emailAddress    <- self.state.emailAddress.value
                password        <- self.state.password.value
                confirmPassword <- self.state.confirmPassword.value
                streetAddress   <- self.state.streetAddress.value
                city            <- self.state.city.value
                zipCode         <- self.state.zip.value
                country         <- self.state.country.value
                phone           <- self.state.phone.value
                pure { firstName, lastName, emailAddress, password, confirmPassword, streetAddress, city, zipCode, country, phone }

          case maybeUser of
            Just user -> do
              self.props.onRegister $ Persona.register user `catchError` case _ of
                 err | Just (errData :: Persona.EmailAddressInUseRegistration) <- Persona.errorData err -> do
                         Console.error errData.email_address_in_use_registration.description
                         liftEffect $ send self $ SetServerValidationError EmailAddress $ InvalidEmailInUse emailInUseMsg
                         throwError err
                     | Just (errData :: Persona.InvalidFormFields) <- Persona.errorData err -> do
                         Console.error errData.invalid_form_fields.description
                         liftEffect $ handlePersonaErrs errData.invalid_form_fields.errors
                         throwError err
                     | otherwise -> do
                         Console.error "An unexpected error occurred during registration"
                         throwError err
            Nothing -> Console.error "Not all registration fields were filled."

    handlePersonaErrs :: RegistrationInputFieldError -> Effect Unit
    handlePersonaErrs errs = do
      traverse_ setFormInvalid $ Object.keys errs
      where
        -- Only match known cases
        -- TODO: Is this enough?
        toRegistrationInputField :: String -> Maybe RegistrationInputField
        toRegistrationInputField = case _ of
          "password"     -> Just Password
          "emailAddress" -> Just EmailAddress
          _              -> Nothing

        setFormInvalid key =
          case toRegistrationInputField key of
            Just EmailAddress -> send self $ SetServerValidationError EmailAddress $ Invalid emailInvalidMsg
            Just Password     -> send self $ SetServerValidationError Password $ Invalid passwordInvalidMsg
            _                 -> pure unit

    inputFieldUpdate :: RegistrationInputField -> EventHandler
    inputFieldUpdate field = handler targetValue (\val -> send self $ UpdateInput field val)

    inputFieldValidation :: RegistrationInputField -> EventHandler
    inputFieldValidation field = handler_ $ send self $ FormFieldValidation field

    -- | Runs validation functions against values of the current state
    validateForm :: Effect Unit
    validateForm = do
      traverse_ validate
        [ FirstName
        , LastName
        , StreetAddress
        , City
        , Zip
        , Phone
        , EmailAddress
        , Password
        , ConfirmPassword
        ]
      where
        validate fieldName = send self $ FormFieldValidation fieldName

    isFormValid :: Boolean
    isFormValid = all isNothing formValidationErrors

    -- | Is form valid or uninitialized
    isFormInitializedInvalid = any isInvalidInitialized formValidationErrors
      where
        isInvalidInitialized Nothing = false
        isInvalidInitialized (Just InvalidNotInitialized) = false
        isInvalidInitialized _ = true

    formValidationErrors =
      [ self.state.firstName.validationError
      , self.state.lastName.validationError
      , self.state.streetAddress.validationError
      , self.state.city.validationError
      , self.state.zip.validationError
      , self.state.phone.validationError
      , self.state.emailAddress.validationError
      , self.state.password.validationError
      , self.state.confirmPassword.validationError
      ]

    firstNameInput :: JSX
    firstNameInput = do
      withValidationErrorText inputField self.state.firstName.validationError
      where
        inputField =
          DOM.input
            { placeholder: "Förnamn"
            , name: "firstName"
            , onChange: inputFieldUpdate FirstName
            , onBlur: inputFieldValidation FirstName
            , value: fromMaybe "" self.state.firstName.value
            , type: "text"
            }

    lastNameInput :: JSX
    lastNameInput =
      withValidationErrorText inputField self.state.lastName.validationError
      where
        inputField =
          DOM.input
            { placeholder: "Efternamn"
            , name: "lastName"
            , onChange: inputFieldUpdate LastName
            , onBlur: inputFieldValidation LastName
            , value: fromMaybe "" self.state.lastName.value
            , type: "text"
            }

    addressInput :: JSX
    addressInput =
      withValidationErrorText inputField self.state.streetAddress.validationError
      where
        inputField =
          DOM.input
            { placeholder: "Adress"
            , name: "address"
            , onChange: inputFieldUpdate StreetAddress
            , onBlur: inputFieldValidation StreetAddress
            , value: fromMaybe "" self.state.streetAddress.value
            , type: "text"
            }

    cityInput :: JSX
    cityInput =
      withValidationErrorText inputField self.state.city.validationError
      where
        inputField =
          DOM.input
            { placeholder: "Stad"
            , name: "city"
            , onChange: inputFieldUpdate City
            , onBlur: inputFieldValidation City
            , value: fromMaybe "" self.state.city.value
            , type: "text"
            }

    zipInput :: JSX
    zipInput =
      withValidationErrorText inputField self.state.zip.validationError
      where
        inputField =
            DOM.input
              { placeholder: "Postnummer"
              , name: "zip"
              , onChange: inputFieldUpdate Zip
              , onBlur: inputFieldValidation Zip
              , value: fromMaybe "" self.state.zip.value
              , type: "text"
              }

    phoneInput :: JSX
    phoneInput =
      withValidationErrorText phoneInputField self.state.phone.validationError
      where
       phoneInputField =
         DOM.input
           { placeholder: "Telefon"
           , name: "phone"
           , onChange: inputFieldUpdate Phone
           , onBlur: inputFieldValidation Phone
           , value: fromMaybe "" self.state.phone.value
           , type: "text"
           }

    emailInput :: JSX
    emailInput =
      withValidationErrorText emailField self.state.emailAddress.validationError
      where
        emailField =
          DOM.input
            { placeholder: "E-postadress"
            , name: "email"
            , onChange: inputFieldUpdate EmailAddress
            , onBlur: inputFieldValidation EmailAddress
            , value: fromMaybe "" self.state.emailAddress.value
            , type: "email"
            }

    passwordInput :: JSX
    passwordInput =
      withValidationErrorText passwordField self.state.password.validationError
      where
        passwordField =
          DOM.input
            { placeholder: "Lösenord (minst 6 tecken)"
            , name: "password"
            , onChange: inputFieldUpdate Password
            , onBlur: inputFieldValidation Password
            , value: fromMaybe "" self.state.password.value
            , type: "password"
            }

    confirmPasswordInput :: JSX
    confirmPasswordInput =
      withValidationErrorText confirmPasswordField self.state.confirmPassword.validationError
      where
        confirmPasswordField =
          DOM.input
            { placeholder: "Bekräfta lösenord"
            , name: "confirm-password"
            , onChange: handler targetValue onConfirmPasswordChange
            , onBlur: inputFieldValidation ConfirmPassword
            , value: fromMaybe "" self.state.confirmPassword.value
            , type: "password"
            }

        -- If the passwords do not match (Invalid state),
        -- validate input while the user is trying to correct it
        -- to give immediate response.
        onConfirmPasswordChange
          | isNothing self.state.confirmPassword.validationError = \pw -> send self $ UpdateInput ConfirmPassword pw
          | otherwise = \pw -> do
              send self $ UpdateInput ConfirmPassword pw
              send self $ FormFieldValidation ConfirmPassword

    withValidationErrorText :: JSX -> Maybe ValidationError -> JSX
    withValidationErrorText inputField validationError =
      case validationError of
        Nothing -> inputField
        -- Initially, we don't want to show any errors in UI
        Just InvalidNotInitialized -> inputField
        Just err ->
          DOM.div
            { className: "registration--invalid-form-field"
            , children:
                [ inputField
                , DOM.div
                    { className: "mt1 registration--invalid-form-text"
                    , children: [ DOM.text $ validationErrorMessageOf err ]
                    }
                ]
            }

    countryDropdown :: JSX
    countryDropdown =
      dropdown
        [ "FI", "AX", "SE", "NO", "DK" ]
        [ "Finland", "Åland", "Sverige", "Norge", "Danmark" ]

    dropdown :: Array String -> Array String -> JSX
    dropdown options descriptions =
      DOM.select
        { className: ""
        , children: zipWith createOption options descriptions
        , onChange: inputFieldUpdate Country
        , value: fromMaybe "FI" self.state.country.value
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
      , validationErrorMessage
      , confirmButton
      , cancelText
      ]

    confirmButton :: JSX
    confirmButton =
      DOM.input
        { type: "submit"
        , className: "registration--create-button mt2"
        , disabled: if isFormInitializedInvalid then true else false
        , value: "Skapa konto"
        }

    validationErrorMessage :: JSX
    validationErrorMessage
      | not isFormInitializedInvalid = mempty
      | otherwise =
          DOM.div
            { className: "registration--invalid-form-generic-message mt2"
            , children: [ DOM.text "Alla obligatoriska fält är inte korrekt ifyllda, kontrollera uppgifterna." ]
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
    { className: "col-12 mx-auto center"
    , children:
        [ DOM.h1_ [ DOM.text "Skapa ditt konto" ] ]
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
