module KSF.Registration where

import Prelude

import Data.Array (intercalate)
import Data.Array as Array
import Data.Either (Either(..))
import Data.Foldable (traverse_)
import Data.List.NonEmpty (NonEmptyList(..), head, zipWith)
import Data.List.NonEmpty as NonEmptyList
import Data.Maybe (Maybe(..), fromMaybe)
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
  , zip                :: Maybe String
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
registration  = make component { initialState, render }

component :: React.Component Props
component = React.createComponent "Registration"

render :: Self -> JSX
render self =
  registrationForm
  [  DOM.div
    { className: "registration--container"
    , children:
        [ firstNameInput self, lastNameInput self
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
  { firstName:        Nothing
  , lastName:         Nothing
  , streetAddress:    Nothing
  , city:             Nothing
  , country:          Nothing
  , zip:              Nothing
  , phone:            Nothing
  , emailAddress:     Nothing
  , password:         Nothing
  , confirmPassword:  Nothing
  }

inputField :: String -> JSX -> JSX
inputField labelText input =
  DOM.div
    { className: "registration--input-field"
    , children:
        [ inputLabel labelText
        , input
        ]
    }

inputLabel :: String -> JSX
inputLabel labelText =
  DOM.span
    { className: "registration--input-label"
    , children: [ DOM.text labelText ]
    }

firstNameInput :: Self -> JSX
firstNameInput self =
  withValidationErrorText (inputField "Förnamn*" input) (validateFirstName self.state.firstName)
  where
    input =
       DOM.input
         { type: "text"
         , placeholder: "Förnamn*"
         , name: "firstName"
         , value: fromMaybe "" self.state.firstName
         , onChange: handler targetValue (\val -> self.setState _ { firstName = val })
         }

lastNameInput :: Self -> JSX
lastNameInput self =
  withValidationErrorText (inputField "Efternamn*" input) (validateLastName self.state.lastName)
  where
    input =
      DOM.input
        { type: "text"
        , placeholder: "Efternamn*"
        , name: "lastName"
        , value: fromMaybe "" self.state.lastName
        , onChange: handler targetValue (\val -> self.setState _ { lastName = val })
        }

passwordInput :: Self -> JSX
passwordInput self =
  DOM.input
    { placeholder: "Lösenord (minst 6 tecken)"
    , name: "password"
    , onChange: handler targetValue (\val -> self.setState _ { password = val })
    , value: fromMaybe "" self.state.password
    , type: "password"
    }

confirmPasswordInput :: Self -> JSX
confirmPasswordInput self =
  withValidationErrorText inputField (validatePasswordComparison self.state.password self.state.confirmPassword)
  where
    inputField =
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
  , streetAddress:    Nothing
  , city:             Nothing
  , country:          Nothing
  , zip:              _
  , phone:            Nothing
  , emailAddress:     Nothing
  , password:         _
  , confirmPassword:  _
  }
  <$> validateFirstName self.state.firstName
  <*> validateLastName self.state.lastName
  <*> validateEmptyZip self.state.zip `andThen` validateZipPattern
  <*> validatePasswordLength self.state.password
  <*> validatePasswordComparison self.state.password initialState.confirmPassword

validateFirstName :: Maybe String -> ValidatedForm (Maybe String)
validateFirstName = validateEmptyField FirstName "Förnamn krävs."

validateLastName :: Maybe String -> ValidatedForm (Maybe String)
validateLastName = validateEmptyField LastName "Efternamn krävs."

validateEmptyZip :: Maybe String -> ValidatedForm (Maybe String)
validateEmptyZip = validateEmptyField Zip "Postnummer krävs."

validateZipPattern :: Maybe String -> ValidatedForm (Maybe String)
validateZipPattern = validateInputWithRegex Password "^[\\s|\\w|-]+$" "Postnummerfältet kan bara innehålla siffror och bokstäver."

validatePasswordLength :: Maybe String -> ValidatedForm (Maybe String)
validatePasswordLength password
  | Just pw <- password, length pw >= 6 = pure $ Just pw
  | otherwise = invalid $ pure $ Invalid Password "Lösenordet måste ha minst 6 tecken."

validatePasswordComparison :: Maybe String -> Maybe String -> ValidatedForm (Maybe String)
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
withValidationErrorText inputField = unV
  (\errs ->
    DOM.div
      { className: "registration--invalid-form-field"
      , children:
          [ inputField
          , DOM.div
              { className: "mt1 registration--invalid-form-text"
              , children: [ DOM.text $ validationErrorMessageOf $ head errs ]
              }
          ]
      })
  (\_ -> inputField)

validationErrorMessageOf :: ValidationError -> String
validationErrorMessageOf = case _ of
  Invalid _ err               -> err
  InvalidEmpty _ err          -> err
  InvalidPatternFailure _ err -> err
  InvalidEmailInUse err       -> err
  InvalidNotInitialized       -> ""
