module KSF.Registration.Component where

import Prelude

import Control.Alt (alt, (<|>))
import Control.Monad.Error.Class (catchError, throwError)
import Control.Monad.List.Trans (ListT(..))
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
  , inputValidations :: Map RegistrationInputField Validation
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

type RegistrationInputFieldValidations =
  { firstName       :: Array ValidationFn
  , lastName        :: Array ValidationFn
  , streetAddress   :: Array ValidationFn
  , city            :: Array ValidationFn
  , zip             :: Array ValidationFn
  -- , country         :: Array ValidationFn -- TODO: Maybe needed at some point
  , phone           :: Array ValidationFn
  , emailAddress    :: Array ValidationFn
  , password        :: Array ValidationFn
  , confirmPassword :: Array ValidationFn
  }

type ValidationFn = (Maybe String -> Effect Validation)

type InputAttributes =
  { placeholder :: String
  , name :: String
  , onChange :: Maybe String -> Effect Unit
  , validations :: Array ValidationFn
  , value :: Maybe String
  }

data Validation
  = Valid
  | Invalid
  | InvalidEmpty
  | InvalidPatternFailure
  | InvalidEmailInUse
derive instance eqValidation :: Eq Validation

data Action
  = UpdateInput RegistrationInputField (Maybe String)
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
  , inputValidations: Map.fromFoldable
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

  FormFieldValidation validation fieldName ->
    Update self.state { inputValidations = insert fieldName validation self.state.inputValidations }

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
        , onSubmit: handler preventDefault
          (\_ -> do
              runValidations
              submit
          )
        }
      where
        submit = when isFormValid do
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
                         liftEffect $ send self $ FormFieldValidation InvalidEmailInUse EmailAddress
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
              send self (FormFieldValidation Invalid inputFieldName)
          | otherwise = pure unit

    inputFieldUpdate :: RegistrationInputField -> Maybe String -> Effect Unit
    inputFieldUpdate field newInputValue = do
      send self (UpdateInput field newInputValue)

    formValidations :: RegistrationInputFieldValidations
    formValidations =
      { firstName:       [validateEmptyField FirstName]
      , lastName:        [validateEmptyField LastName]
      , streetAddress:   [validateEmptyField StreetAddress]
      , city:            [validateEmptyField City]
      , zip:             [validateEmptyField Zip,          validateInputWithRegex Zip zipRegexPattern]
      , phone:           [validateEmptyField Phone,        validateInputWithRegex Phone phoneRegexPattern]
      , emailAddress:    [validateEmptyField EmailAddress, validateInputWithRegex EmailAddress emailRegex]
      , password:        [validatePassword]
      , confirmPassword: [comparePasswords]
      }
      where
        -- Allows word characters (a-z, A-Z, 0-9, _), dashes and whitespaces
        zipRegexPattern = "^[\\s|\\w|-]+$"

        -- Allows digits, "+" and whitespaces,
        -- e.g. 040 1231234, +358 04 1231234
        phoneRegexPattern = "^[\\d|\\+|\\s|-|\\(|\\)]+$"

        -- From https://developer.mozilla.org/en-US/docs/Web/HTML/Element/input/email#Basic_validation
        emailRegex = "^[a-zA-Z0-9.!#$%&'*+\\/=?^_`{|}~-]+@[a-zA-Z0-9](?:[a-zA-Z0-9-]{0,61}[a-zA-Z0-9])?(?:\\.[a-zA-Z0-9](?:[a-zA-Z0-9-]{0,61}[a-zA-Z0-9])?)*$"

    runValidations :: Effect Unit
    runValidations = do
       validate self.state.firstName formValidations.firstName
       validate self.state.lastName formValidations.lastName
       validate self.state.streetAddress formValidations.streetAddress
       validate self.state.city formValidations.city
       validate self.state.zip formValidations.zip
       validate self.state.phone formValidations.phone
       validate self.state.emailAddress formValidations.emailAddress
       validate self.state.password formValidations.password
       validate self.state.confirmPassword formValidations.confirmPassword
      where
        validate value validationFns =
          void $ traverse_ (_ $ value) validationFns

    isFormValid :: Boolean
    isFormValid = all (_ == Valid) $ values self.state.inputValidations

    isFieldValid :: RegistrationInputField -> Boolean
    isFieldValid field = maybe false (_ == Valid) $ lookup field self.state.inputValidations

    firstNameInput :: JSX
    firstNameInput = do
      withValidationErrors inputField FirstName [Tuple InvalidEmpty "Förnamn krävs"]
      where
        inputField =
          createTextInput
            { placeholder: "Förnamn"
            , name: "firstName"
            , onChange: inputFieldUpdate FirstName
            , validations: formValidations.firstName
            , value: self.state.firstName
            }

    lastNameInput :: JSX
    lastNameInput =
      withValidationErrors inputField LastName [Tuple InvalidEmpty "Efternamn krävs."]
      where
        inputField =
          createTextInput
            { placeholder: "Efternamn"
            , name: "lastName"
            , onChange: inputFieldUpdate LastName
            , validations: formValidations.lastName
            , value: self.state.lastName
            }

    addressInput :: JSX
    addressInput =
      withValidationErrors inputField StreetAddress [Tuple InvalidEmpty "Adress krävs."]
      where
        inputField =
          createTextInput
            { placeholder: "Adress"
            , name: "address"
            , onChange: inputFieldUpdate StreetAddress
            , validations: formValidations.streetAddress
            , value: self.state.streetAddress
            }

    cityInput :: JSX
    cityInput =
      withValidationErrors inputField City [Tuple InvalidEmpty "Stad krävs."]
      where
        inputField =
          createTextInput
            { placeholder: "Stad"
            , name: "city"
            , onChange: inputFieldUpdate City
            , validations: formValidations.city
            , value: self.state.city
            }

    zipInput :: JSX
    zipInput =
      withValidationErrors inputField Zip
        [ Tuple InvalidEmpty "Postnummer krävs."
        , Tuple InvalidPatternFailure "Postnummerfältet kan bara innehålla siffror och bokstäver."
        ]
      where
        inputField =
            createTextInput
              { placeholder: "Postnummer"
              , name: "zip"
              , onChange: inputFieldUpdate Zip
              , validations: formValidations.zip
              , value: self.state.zip
              }

    phoneInput :: JSX
    phoneInput =
      withValidationErrors phoneInputField Phone
        [ Tuple InvalidEmpty "Telefon krävs."
        , Tuple InvalidPatternFailure "Telefonnummer kan bara innehålla nummer."
        ]
      where
       phoneInputField =
         createTextInput
           { placeholder: "Telefon"
           , name: "phone"
           , onChange: inputFieldUpdate Phone
           , value: self.state.phone
           , validations: formValidations.phone
           }

    emailInput :: JSX
    emailInput =
      withValidationErrors emailField EmailAddress emailValidations
      where
        emailValidations =
          [ Tuple InvalidEmpty          "E-postadress krävs."
          , Tuple InvalidPatternFailure emailInvalidMsg
          , Tuple InvalidEmailInUse     emailInUseMsg
          -- The `Invalid` state is set if Persona returns an error that is not an "email in use" error.
          -- It should never happen, but if it does, it's probably due to input not being a valid email address.
          , Tuple Invalid               emailInvalidMsg
          ]
        emailField =
          createEmailInput
            { placeholder: "E-postadress"
            , name: "email"
            , onChange: inputFieldUpdate EmailAddress
            , validations: formValidations.emailAddress
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
      withValidationErrors passwordField Password [Tuple Invalid passwordInvalidMsg]
      where
        passwordField =
          createInput
            { placeholder: "Lösenord (minst 6 tecken)"
            , name: "password"
            , onChange: inputFieldUpdate Password
            , validations: formValidations.password
            , value: self.state.password
            }
            "password"
        -- TODO: Probably not always the case.
        -- The problem of displaying error message directly from Janrain
        -- is for example the language used (e.g. Swedish for Finnish users).
        -- Also, the error might be gibberish.
        passwordInvalidMsg = "Lösenordet måste ha minst 6 tecken."

    confirmPasswordInput :: JSX
    confirmPasswordInput =
      withValidationErrors confirmPasswordField ConfirmPassword [Tuple Invalid passwordMismatchMsg]
      where
        confirmPasswordField =
          createInput
            { placeholder: "Bekräfta lösenord"
            , name: "confirm-password"
            , onChange: onConfirmPasswordChange
            , validations: formValidations.confirmPassword
            , value: self.state.confirmPassword
            }
          "password"

        -- If the passwords do not match (Invalid state),
        -- validate input while the user is trying to correct it
        -- to give immediate response.
        onConfirmPasswordChange
          | isFieldValid ConfirmPassword = inputFieldUpdate ConfirmPassword
          | otherwise = \pw -> inputFieldUpdate ConfirmPassword pw <* comparePasswords pw

        passwordMismatchMsg = "Lösenorden överensstämmer inte med varandra."

    withValidationErrors :: JSX -> RegistrationInputField -> Array (Tuple Validation String) -> JSX
    withValidationErrors inputField fieldName validations =
      case oneOf $ map chooseValidationErrorMessage validations of
        Just validationError -> validationError
        _ -> inputField
      where
        chooseValidationErrorMessage :: (Tuple Validation String) -> Maybe JSX
        chooseValidationErrorMessage (Tuple validationType errorMessage)
          | Just validationState <- lookup fieldName self.state.inputValidations
          , validationState == validationType
          = Just $ validationErrorText inputField errorMessage
          | otherwise = Nothing

    validateEmptyField fieldName fieldValue
      | Just value <- fieldValue
      , length value > 0
      = pure Valid <* send self (FormFieldValidation Valid fieldName)
      | otherwise = pure Invalid <* send self (FormFieldValidation InvalidEmpty fieldName)

    validateInputWithRegex :: RegistrationInputField -> String -> Maybe String -> Effect Validation
    validateInputWithRegex fieldName regexString fieldInput
      | Just inputValue <- fieldInput
      , Right regexPattern <- Regex.regex regexString Regex.Flags.noFlags
      , Regex.test regexPattern inputValue
      = pure Valid <* send self (FormFieldValidation Valid fieldName)
      | otherwise = pure Invalid <* send self (FormFieldValidation InvalidPatternFailure fieldName)

    -- | Only checks that password is minimum of 6 characters long
    validatePassword password
      | Just pw <- password
      , length pw >= 6
      = pure Valid <* send self (FormFieldValidation Valid Password)
      | otherwise = pure Invalid <* send self (FormFieldValidation Invalid Password)

    comparePasswords confirmedPassword
      | confirmedPassword /= self.state.password = pure Invalid <* send self (FormFieldValidation Invalid ConfirmPassword)
      | otherwise = pure Valid <* send self (FormFieldValidation Valid ConfirmPassword)

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

createInput :: InputAttributes -> String -> JSX
createInput { placeholder, name, onChange, validations, value } inputType =
  DOM.input
    { type: inputType
    , onChange: handler targetValue onChange
    , onBlur: handler targetValue runValidations
    , placeholder
    , name
    , value: fromMaybe "" value
    }
  where
    runValidations fieldValue =
      void $ foldl (runUntilInvalid fieldValue) (pure Valid) validations

    -- | Run validations until an `Invalid` computation is made by a validation function.
    runUntilInvalid :: Maybe String -> Effect Validation -> (Maybe String -> Effect Validation) -> Effect Validation
    runUntilInvalid fieldValue validation validationFn = do
      validationState <- validation
      if validationState == Invalid
        then pure Invalid
        else validationFn fieldValue
