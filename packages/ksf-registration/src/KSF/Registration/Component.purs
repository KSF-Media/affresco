module KSF.Registration.Component where

import Prelude

import Control.Alt (alt, (<|>))
import Control.Monad.Error.Class (catchError, throwError)
import Data.Array (all, foldMap, foldl, snoc, zipWith)
import Data.Either (Either(..))
import Data.Foldable (fold, for_, oneOf, traverse_)
import Data.Map (Map, fromFoldable, insert, lookup, values)
import Data.Map.Internal as Map
import Data.Maybe (Maybe(..), fromJust, fromMaybe, isJust, maybe)
import Data.Monoid (guard, mempty)
import Data.String (length)
import Data.String.Regex (regex)
import Data.String.Regex as Regex
import Data.String.Regex.Flags (RegexFlags(..), noFlags)
import Data.String.Regex.Flags as Regex.Flags
import Data.Tuple (Tuple(..))
import Effect (Effect, forE)
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
  , inputValidations2 :: Map RegistrationInputField Validation
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

type InputAttributes =
  { placeholder :: String
  , name :: String
  , onChange :: Maybe String -> Effect Unit
  , validate :: Array (Maybe String -> Effect Validation)
  , value :: Maybe String
  }

type InputType = String

data Validation
  = Valid
  | Invalid
  | InvalidEmpty
  | InvalidPatternFailure
  | InvalidEmailInUse -- TODO: does not belong here
derive instance eqValidation :: Eq Validation

data EmailValidation = InUse | Validation Validation
derive instance eqEmailValidation :: Eq EmailValidation

type Pattern = String

data Action
  = UpdateInput RegistrationInputField (Maybe String)
  | PasswordMismatch Validation
  | EmailInUse
  | FormFieldValidation Validation RegistrationInputField

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
  , inputValidations2: Map.fromFoldable
      [ Tuple FirstName Valid
      , Tuple LastName Valid
      , Tuple StreetAddress Valid
      , Tuple City Valid
      , Tuple Zip Valid
      , Tuple Country Valid
      , Tuple Phone Valid
      , Tuple EmailAddress Valid
      , Tuple Password Valid
      , Tuple ConfirmPassword Valid
      ]
  }

component :: React.Component Props
component = React.createComponent "Registration"

update :: Self -> Action -> StateUpdate Props State Action
update self = case _ of
  UpdateInput attr newValue -> Update do
    case attr of
      FirstName       -> self.state { firstName = newValue }
      LastName        -> self.state { lastName = newValue }
      StreetAddress   -> self.state { streetAddress = newValue }
      City            -> self.state { city = newValue }
      Zip             -> self.state { zip = newValue }
      Country         -> self.state { country = newValue }
      Phone           -> self.state { phone = newValue }
      EmailAddress    -> self.state { emailAddress = newValue }
      Password        -> self.state { password = newValue }
      ConfirmPassword -> self.state { confirmPassword = newValue }
  PasswordMismatch validation ->
    Update self.state { inputValidations { passwordConfirm = validation } }
  EmailInUse ->
    Update self.state { inputValidations { emailAddress = InUse } }

  FormFieldValidation validation fieldName ->
    Update self.state { inputValidations2 = insert fieldName validation self.state.inputValidations2 }

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
        canSubmit = self.state.inputValidations.passwordConfirm == Valid

        submit = do
          let maybeUser = do
                firstName       <- self.state.firstName
                lastName        <- self.state.lastName
                emailAddress    <- self.state.emailAddress
                password        <- self.state.password
                confirmPassword <- self.state.confirmPassword
                streetAddress   <- self.state.streetAddress
                city            <- self.state.city
                zipCode         <- self.state.zip
                country         <- self.state.country
                phone           <- self.state.phone
                pure { firstName, lastName, emailAddress, password, confirmPassword, streetAddress, city, zipCode, country, phone }

          case maybeUser of
            Just user -> do
              self.props.onRegister $ Persona.register user `catchError` case _ of
                 err | Just (errData :: Persona.EmailAddressInUseRegistration) <- Persona.errorData err -> do
                         Console.error errData.email_address_in_use_registration.description
                         liftEffect $ send self EmailInUse
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
        -- TOOD: Is this enough?
        toRegistrationInputField :: String -> Maybe RegistrationInputField
        toRegistrationInputField = case _ of
          "password"     -> Just Password
          "emailAddress" -> Just EmailAddress
          _              -> Nothing

        setFormInvalid key
          | Just inputFieldName <- toRegistrationInputField key =
              -- FIX: what about email in use?
              send self (FormFieldValidation Invalid inputFieldName)
          | otherwise = pure unit

    inputFieldUpdate :: RegistrationInputField -> Maybe String -> Effect Unit
    inputFieldUpdate field newInputValue = do
      send self (UpdateInput field newInputValue)

    firstNameInput :: JSX
    firstNameInput = do
      withValidationError'' inputField FirstName [(Tuple InvalidEmpty "Förnamn krävs")]
      where
        inputField =
          createTextInput
            { placeholder: "Förnamn"
            , name: "firstName"
            , onChange: inputFieldUpdate FirstName
            , validate: [validateEmptyField FirstName]
            , value: self.state.firstName
            }

    lastNameInput :: JSX
    lastNameInput =
      withValidationError' LastName Invalid inputField "Efternamn krävs."
      <> inputField
      where
        inputField =
          createTextInput
            { placeholder: "Efternamn"
            , name: "lastName"
            , onChange: inputFieldUpdate LastName
            , validate: [validateEmptyField LastName]
            , value: self.state.lastName
            }

    validateEmptyField fieldName fieldValue
      | Just value <- fieldValue
      , length value > 0
      = pure Valid <* send self (FormFieldValidation Valid fieldName)
      | otherwise = pure Invalid <* send self (FormFieldValidation InvalidEmpty fieldName)

    addressInput :: JSX
    addressInput =
      withValidationError' StreetAddress InvalidEmpty inputField "Adress krävs."
      <> inputField
      where
        inputField =
          createTextInput
            { placeholder: "Adress"
            , name: "address"
            , onChange: inputFieldUpdate StreetAddress
            , validate: [validateEmptyField StreetAddress]
            , value: self.state.streetAddress
            }

    cityInput :: JSX
    cityInput =
      withValidationError' City InvalidEmpty inputField "Stad krävs."
      <> inputField
      where
        inputField =
          createTextInput
            { placeholder: "Stad"
            , name: "city"
            , onChange: inputFieldUpdate City
            , validate: [validateEmptyField City]
            , value: self.state.city
            }

    zipInput :: JSX
    zipInput =
      withValidationError'' inputField Zip [(Tuple InvalidEmpty "Postnummer krävs."), (Tuple InvalidPatternFailure"Postnummerfältet kan bara innehålla siffror och bokstäver.")]
      where
        -- Allows word characters (a-z, A-Z, 0-9, _), dashes and whitespaces
        zipRegexPattern = "^[\\s|\\w|-]+$"
        inputField =
            createTextInput
              { placeholder: "Postnummer"
              , name: "zip"
              , onChange: inputFieldUpdate Zip
              , validate: [validateEmptyField Zip, validateInputWithRegex Zip zipRegexPattern]
              , value: self.state.zip
              }

    isFieldValid :: RegistrationInputField -> Boolean
    isFieldValid field = maybe false (_ == Valid) $ lookup field self.state.inputValidations2

    phoneInput :: JSX
    phoneInput =
      createTextInput
        { placeholder: "Telefon"
        , name: "phone"
        , onChange: inputFieldUpdate Phone
        , value: self.state.phone
        , validate: [validateEmptyField Phone, validateInputWithRegex Phone phoneRegexPattern]
        }
      where
       -- Allows digits, "+" and whitespaces,
       -- e.g. 040 1231234, +358 04 1231234
       phoneRegexPattern = "[\\d|\\+|\\s|-|\\(|\\)]+"

    emailInput :: JSX
    emailInput =
      case lookup EmailAddress self.state.inputValidations2 of
        Just Invalid           -> validationErrorText emailField emailInvalidMsg
        Just InvalidEmailInUse -> validationErrorText emailField emailInUseMsg
        _                      -> emailField
      where
        emailField =
          createEmailInput
            { placeholder: "E-postadress"
            , name: "email"
            , onChange: inputFieldUpdate EmailAddress
            , validate: [validateInputWithRegex EmailAddress emailRegex]
            , value: self.state.emailAddress
            }
        emailInUseMsg =
          """E-postadressen är redan i bruk.
             Har du redan ett konto hos Hufvudstadsbladet, Västra Nyland eller Östnyland?
             Då kan du använda samma inloggning.
             Du kan också skapa konto med en annan adress."""
        emailInvalidMsg = "Ogiltig E-postadress."

        -- From https://developer.mozilla.org/en-US/docs/Web/HTML/Element/input/email#Basic_validation
        emailRegex = "^[a-zA-Z0-9.!#$%&'*+\\/=?^_`{|}~-]+@[a-zA-Z0-9](?:[a-zA-Z0-9-]{0,61}[a-zA-Z0-9])?(?:\\.[a-zA-Z0-9](?:[a-zA-Z0-9-]{0,61}[a-zA-Z0-9])?)*$"

    validateInputWithRegex :: RegistrationInputField -> String -> Maybe String -> Effect Validation
    validateInputWithRegex fieldName regexString fieldInput
      | Just inputValue <- fieldInput
      , Right regexPattern <- Regex.regex regexString Regex.Flags.noFlags
      , Regex.test regexPattern inputValue
      = pure Valid <* send self (FormFieldValidation Valid fieldName)
      | otherwise = pure Invalid <* send self (FormFieldValidation InvalidPatternFailure fieldName)

    passwordInput :: JSX
    passwordInput =
      withValidationError' Password Invalid passwordField passwordInvalidMsg
      <> passwordField
      where
        passwordField =
          DOM.input
            { type: "password"
            , placeholder: "Lösenord (minst 6 tecken)"
            , name: "password"
            , required: true
            , onChange: handler targetValue $ inputFieldUpdate Password
            , onBlur: handler targetValue validatePassword
            , value: fromMaybe "" self.state.password
            , pattern: ".{6,}"
            , title: "Lösenordet måste ha minst 6 tecken"
            }
        -- TODO: Probably not always the case.
        -- The problem of displaying error message directly from Janrain
        -- is for example the language used (e.g. Swedish for Finnish users).
        -- Also, the error might be gibberish.
        passwordInvalidMsg = "Lösenordet måste ha minst 6 tecken."

        validatePassword password
          | Just pw <- password
          , length pw >= 6
          = send self (FormFieldValidation Valid Password)
          | otherwise = send self (FormFieldValidation Invalid Password)

    withValidationError' :: RegistrationInputField -> Validation -> JSX -> String -> JSX
    withValidationError' fieldName validationType inputField err
      | Just validationState <- lookup fieldName self.state.inputValidations2
      , validationState == validationType
      = validationErrorText inputField err
      | otherwise = mempty


    withValidationError'' :: JSX -> RegistrationInputField -> Array (Tuple Validation String) -> JSX
    withValidationError'' inputField fieldName validations =
      case oneOf $ map wät validations of
        Just lol -> lol
        _ -> inputField
      where
        wät :: (Tuple Validation String) -> Maybe JSX
        wät (Tuple validationType errorMessage)
          | Just validationState <- lookup fieldName self.state.inputValidations2
          , validationState == validationType
          = Just $ validationErrorText inputField errorMessage
          | otherwise = Nothing

    validationErrorText :: JSX -> String -> JSX
    validationErrorText inputField errorText =
      DOM.div
        { className: "registration--invalid-form-field"
        , children:
            [ inputField
            , DOM.div
                { className: "mt1 registration--invalid-form-text"
                , children: [ DOM.text errorText ]
                }
            ]
        }

    confirmPasswordInput :: JSX
    confirmPasswordInput =
      withValidationError' Password Invalid (confirmPasswordField Invalid) passwordMismatchMsg
      <> (confirmPasswordField Valid)
      where
        confirmPasswordField :: Validation -> JSX
        confirmPasswordField isValid =
          DOM.input
            { type: "password"
            , onBlur: handler targetValue comparePasswords
            , placeholder: "Bekräfta lösenord"
            , name: "confirm-password"
            , onChange: handler targetValue (onConfirmPasswordChange isValid)
            , value: fromMaybe "" self.state.confirmPassword
            }

        -- If the passwords do not match (Invalid state),
        -- validate input while the user is trying to correct it
        -- to give immediate response.
        onConfirmPasswordChange Valid = inputFieldUpdate ConfirmPassword
        onConfirmPasswordChange _ = \pw -> do
          comparePasswords pw
          inputFieldUpdate ConfirmPassword pw

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
        [ "FI", "AX", "SE", "NO", "DK" ]
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
      , validationErrorMessage
      , confirmButton
      , cancelText
      ]

    isFormValid :: Boolean
    isFormValid = all (_ == Valid) $ values self.state.inputValidations2

    confirmButton :: JSX
    confirmButton =
      DOM.input
        { type: "submit"
        , className: "registration--create-button mt2"
        , disabled: if isFormValid then false else true
        , value: "Skapa konto"
        }

    validationErrorMessage :: JSX
    validationErrorMessage
      | isFormValid = mempty
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
    , onChange: handler targetValue onChange
    , placeholder
    , name
    , pattern
    }

createInput :: InputAttributes -> InputType -> JSX
createInput { placeholder, name, onChange, validate, value } type_ =
  DOM.input
    { type: type_
    , onChange: handler targetValue onChange
    , onBlur: handler targetValue runValidations
    , placeholder
    , name
    , value: fromMaybe "" value
    }
  where
    runValidations fieldValue =
      pure unit <* foldl (noniin fieldValue) (pure Valid) validate

    -- | Run validations until an `Invalid` computation is made by a validation function.
    noniin :: Maybe String -> Effect Validation -> (Maybe String -> Effect Validation) -> Effect Validation
    noniin v b fn = do
      bb <- b
      if bb == Invalid
        then b
        else fn v
    wat fieldValue =
      traverse_ (\fn -> fn fieldValue) validate
