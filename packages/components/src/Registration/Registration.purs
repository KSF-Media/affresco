module KSF.Registration where

import Prelude

import Data.Array (any, intercalate, snoc)
import Data.Array as Array
import Data.Either (Either(..))
import Data.Foldable (foldMap, traverse_)
import Data.List.NonEmpty (NonEmptyList(..), head, zipWith)
import Data.List.NonEmpty as NonEmptyList
import Data.Maybe (Maybe(..), fromMaybe, isJust)
import Data.String (length, null)
import Data.String.Regex as Regex
import Data.String.Regex.Flags as Regex.Flags
import Data.Validation.Semigroup (V(..), andThen, invalid, isValid, unV)
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class.Console as Console
import Persona as Persona
import React.Basic (JSX, make)
import React.Basic as React
import React.Basic.DOM as DOM
import React.Basic.DOM.Events (preventDefault, targetValue)
import React.Basic.Events (handler, handler_)

type Self = React.Self Props FormData

type ValidatedForm a = V (NonEmptyList ValidationError) a

type Props =
  { onRegister :: Aff Persona.LoginResponse -> Effect Unit
  , onCancelRegistration :: Effect Unit
  }

type FormData =
  { firstName          :: Maybe String
  , lastName           :: Maybe String
  , streetAddress      :: Maybe String
  , city               :: Maybe String
  , country            :: Maybe String
  , zipCode            :: Maybe String
  , phone              :: Maybe String
  , emailAddress       :: Maybe String
  , password           :: Maybe String
  , confirmPassword    :: Maybe String
  }

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

data ValidationError
  = Invalid RegistrationInputField String
  | InvalidEmpty RegistrationInputField String
  | InvalidPatternFailure RegistrationInputField String
  | InvalidEmailInUse String
  | InvalidNotInitialized -- Fictional state only to be set when the form is first rendered
derive instance eqValidationError :: Eq ValidationError

registration :: Props -> JSX
registration = make component { initialState, render }

component :: React.Component Props
component = React.createComponent "Registration"

render :: Self -> JSX
render self =
  registrationForm
  [  DOM.div
    { className: "registration--container"
    , children:
        [ firstNameInput self, lastNameInput self
        , streetAddressInput self, cityInput self
        , zipCodeInput self, countryDropdown self
        , phoneInput self, emailAddressInput self
        , passwordInput self, confirmPasswordInput self
        ]
    }
  ]
  where
    registrationForm :: Array JSX -> JSX
    registrationForm children =
      DOM.form
        { children
        , onSubmit: handler preventDefault $ (\_ -> submitForm $ formValidations self)
        }

initialState :: FormData
initialState =
  { firstName:       Nothing
  , lastName:        Nothing
  , streetAddress:   Nothing
  , city:            Nothing
  , country:         Nothing
  , zipCode:         Nothing
  , phone:           Nothing
  , emailAddress:    Nothing
  , password:        Nothing
  , confirmPassword: Nothing
  }

inputLabel :: String -> String -> JSX
inputLabel labelText labelFor =
  DOM.label
    { className: "registration--input-label"
    , children: [ DOM.text labelText ]
    , htmlFor: labelFor
    }

type InputFieldAttributes =
  { label :: String
  , type_ :: String
  , name  :: String
  , value :: Maybe String
  , onChange :: Maybe String -> Effect Unit
  , validatedInput :: ValidatedForm (Maybe String)
  }

inputField :: InputFieldAttributes -> JSX
inputField a =
  DOM.div
    { className: "registration--input-container"
    , children:
        [ inputLabel a.label a.name
        , DOM.input
            { type: a.type_
            , placeholder: a.label
            , name: a.name
            , value: fromMaybe "" a.value
            , onChange: handler targetValue a.onChange
            , className:
              if isJust $ inputFieldErrorMessage a.validatedInput
                then "registration--invalid-form-field"
                else ""
            }
        ] `snoc` foldMap errorMessage (inputFieldErrorMessage a.validatedInput)
    }
  where
    errorMessage e =
      DOM.span
        { className: "registration--invalid-form-text"
        , children: [ DOM.text e ]
        }

firstNameInput :: Self -> JSX
firstNameInput self = inputField
  { type_: "text"
  , label: "Förnamn"
  , name: "firstName"
  , onChange: (\val -> self.setState _ { firstName = val })
  , validatedInput: validateFirstName self.state.firstName
  , value: self.state.firstName
  }

lastNameInput :: Self -> JSX
lastNameInput self = inputField
  { type_: "text"
  , label: "Efternamn"
  , name: "lastName"
  , onChange: (\val -> self.setState _ { lastName = val })
  , validatedInput: validateLastName self.state.lastName
  , value: self.state.lastName
  }

streetAddressInput :: Self -> JSX
streetAddressInput self = inputField
  { type_: "text"
  , label: "Adress"
  , name: "streetAddress"
  , onChange: (\val -> self.setState _ { streetAddress = val })
  , validatedInput: validateStreetAddress self.state.streetAddress
  , value: self.state.streetAddress
  }

cityInput :: Self -> JSX
cityInput self = inputField
  { type_: "text"
  , label: "Stad"
  , name: "city"
  , onChange: (\val -> self.setState _ { city = val })
  , validatedInput: validateCity self.state.city
  , value: self.state.city
  }

zipCodeInput :: Self -> JSX
zipCodeInput self = inputField
  { type_: "text"
  , label: "Postnummer"
  , name: "zipCode"
  , onChange: (\val -> self.setState _ { zipCode = val })
  , validatedInput: validateZipCode self.state.zipCode
  , value: self.state.zipCode
  }

countryDropdown :: Self -> JSX
countryDropdown self =
  DOM.div
    { className: "registration--input-container"
    , children:
        [ inputLabel "Land" "country"
        , DOM.select
            { children: map createOption countries
            , onChange: handler targetValue (\val -> self.setState _ { country = val })
            , value: fromMaybe "FI" self.state.country
            }
        ]
    }
  where
    createOption { countryCode, countryName } =
      DOM.option
        { value: countryCode
        , children: [ DOM.text countryName ]
        }
    countries =
      [ { countryCode: "FI", countryName: "Finland" }
      , { countryCode: "AX", countryName: "Åland" }
      , { countryCode: "SE", countryName: "Sverige" }
      , { countryCode: "NO", countryName: "Norge" }
      , { countryCode: "DK", countryName: "Danmark" }
      ]

phoneInput :: Self -> JSX
phoneInput self = inputField
  { type_: "text"
  , label: "Telefon"
  , name: "phone"
  , onChange: (\val -> self.setState _ { phone = val })
  , validatedInput: validatePhone self.state.phone
  , value: self.state.phone
  }

emailAddressInput :: Self -> JSX
emailAddressInput self = inputField
  { type_: "email"
  , label: "E-postadress."
  , name: "emailAddress"
  , onChange: (\val -> self.setState _ { emailAddress = val })
  , validatedInput: validateEmailAddress self.state.emailAddress
  , value: self.state.emailAddress
  }

passwordInput :: Self -> JSX
passwordInput self =
  DOM.input
    { placeholder: "Lösenord (minst 6 tecken)"
    , name: "password"
    , onBlur: handler targetValue (\val -> self.setState _ { password = val }) -- handler_ $ Console.log "lol"
--    , onChange: pure unit
    , value: fromMaybe "" self.state.password
    , type: "password"
    }

confirmPasswordInput :: Self -> JSX
confirmPasswordInput self =
  withValidationErrorText input (validatePasswordComparison self.state.password self.state.confirmPassword)
  where
    input =
      DOM.input
        { placeholder: "Bekräfta lösenord"
        , name: "confirm-password"
        , onChange: handler targetValue (\val -> self.setState _ { confirmPassword = val })
        , value: fromMaybe "" self.state.confirmPassword
        , type: "password"
        }

submitForm :: ValidatedForm FormData -> Effect Unit
submitForm = unV
  (\errors   -> Console.log $ intercalate ", " $ map validationErrorMessageOf errors)
  (\formData -> Console.log "Everything is fine!")

formValidations :: Self -> ValidatedForm FormData
formValidations self =
  { firstName:        _
  , lastName:         _
  , streetAddress:    _
  , city:             _
  , country:          _
  , zipCode:          _
  , phone:            _
  , emailAddress:     _
  , password:         _
  , confirmPassword:  _
  }
  <$> validateFirstName self.state.firstName
  <*> validateLastName self.state.lastName
  <*> validateStreetAddress self.state.streetAddress
  <*> validateCity self.state.city
  <*> validateCountry self.state.country
  <*> validateZipCode self.state.zipCode
  <*> validatePhone self.state.phone
  <*> validateEmailAddress self.state.emailAddress
  <*> validatePasswordLength self.state.password
  <*> validatePasswordComparison self.state.password initialState.confirmPassword

validateFirstName :: Maybe String -> ValidatedForm (Maybe String)
validateFirstName = validateEmptyField FirstName "Förnamn krävs."

validateLastName :: Maybe String -> ValidatedForm (Maybe String)
validateLastName = validateEmptyField LastName "Efternamn krävs."

validateStreetAddress :: Maybe String -> ValidatedForm (Maybe String)
validateStreetAddress = validateEmptyField StreetAddress "Adress krävs."

validateCity :: Maybe String -> ValidatedForm (Maybe String)
validateCity = validateEmptyField City "Stad krävs."

validateCountry :: Maybe String -> ValidatedForm (Maybe String)
validateCountry = validateEmptyField Country "Land krävs."

validateZipCode :: Maybe String -> ValidatedForm (Maybe String)
validateZipCode zipCode =
  validateEmptyField Zip "Postnummer krävs." zipCode `andThen`
  validateInputWithRegex Password "^[\\s|\\w|-]+$" "Postnummerfältet kan bara innehålla siffror och bokstäver."

validatePhone :: Maybe String -> ValidatedForm (Maybe String)
validatePhone phone =
  validateEmptyField Phone "Telefon krävs." phone `andThen`
  validateInputWithRegex Phone "^[\\d|\\+|\\s|-|\\(|\\)]+$" "Telefonnummer kan bara bestå av siffror, mellanslag och +-tecken."

validateEmailAddress :: Maybe String -> ValidatedForm (Maybe String)
validateEmailAddress email =
  validateEmptyField EmailAddress "E-postadress krävs." email `andThen`
  validateInputWithRegex EmailAddress emailRegex "Ogiltig E-postadress."
  where
    -- From https://developer.mozilla.org/en-US/docs/Web/HTML/Element/input/email#Basic_validation
    emailRegex = "^[a-zA-Z0-9.!#$%&'*+\\/=?^_`{|}~-]+@[a-zA-Z0-9](?:[a-zA-Z0-9-]{0,61}[a-zA-Z0-9])?(?:\\.[a-zA-Z0-9](?:[a-zA-Z0-9-]{0,61}[a-zA-Z0-9])?)*$"

validatePassword :: Maybe String -> ValidatedForm (Maybe String)
validatePassword password = validateEmptyField Password "Lösenord krävs." password `andThen` validatePasswordLength

validatePasswordLength :: Maybe String -> ValidatedForm (Maybe String)
validatePasswordLength Nothing = invalid $ pure $ InvalidNotInitialized
validatePasswordLength password
  | Just pw <- password, length pw >= 6 = pure $ Just pw
  | otherwise = invalid $ pure $ Invalid Password "Lösenordet måste ha minst 6 tecken."

validatePasswordComparison :: Maybe String -> Maybe String -> ValidatedForm (Maybe String)
validatePasswordComparison Nothing Nothing = invalid $ pure $ InvalidNotInitialized
validatePasswordComparison password confirmedPassword
  | Just pw <- password
  , Just confirmedPw <- confirmedPassword
  , pw == confirmedPw
  = pure $ Just pw
  | otherwise = invalid $ pure $ Invalid ConfirmPassword "Lösenorden överensstämmer inte med varandra."


validateEmptyField :: RegistrationInputField -> String -> Maybe String -> ValidatedForm (Maybe String)
validateEmptyField _ _ Nothing = invalid $ pure $ InvalidNotInitialized
validateEmptyField fieldName validationErr (Just value) =
  if null value
    then invalid $ pure $ InvalidEmpty fieldName validationErr
    else pure (Just value)

validateInputWithRegex :: RegistrationInputField -> String -> String -> Maybe String -> ValidatedForm (Maybe String)
validateInputWithRegex fieldName regexString errMsg inputValue
  | Just val <- inputValue
  , Right regexPattern <- Regex.regex regexString Regex.Flags.noFlags
  , Regex.test regexPattern val
  = pure $ Just val
  | otherwise = invalid $ pure $ InvalidPatternFailure fieldName errMsg

withValidationErrorText :: JSX -> ValidatedForm (Maybe String) -> JSX
withValidationErrorText input = unV handleInvalidField (\_ -> input)
  where
    handleInvalidField errs
      -- If field is not initialized, do not show error
      | InvalidNotInitialized <- head errs = input
      | otherwise =
        DOM.div
          { className: "registration--invalid-form-field"
          , children:
              [ input
              , DOM.div
                  { className: "registration--invalid-form-text"
                  , children: [ DOM.text $ validationErrorMessageOf $ head errs ]
                  }
              ]
          }

inputFieldErrorMessage :: ValidatedForm (Maybe String) -> Maybe String
inputFieldErrorMessage = unV handleInvalidField (\_ -> Nothing)
  where
    handleInvalidField errs
      -- If field is not initialized, it's concidered to be valid
      | InvalidNotInitialized <- head errs = Nothing
      | otherwise = Just $ validationErrorMessageOf $ head errs

validationErrorMessageOf :: ValidationError -> String
validationErrorMessageOf = case _ of
  Invalid _ err               -> err
  InvalidEmpty _ err          -> err
  InvalidPatternFailure _ err -> err
  InvalidEmailInUse err       -> err
  InvalidNotInitialized       -> ""

isNotInitialized :: ValidationError -> Boolean
isNotInitialized InvalidNotInitialized = true
isNotInitialized _ = false
