module KSF.Registration where

import Prelude

import Control.Alt ((<|>))
import Control.Monad.Error.Class (catchError, throwError)
import Data.Array (all, any, cons, find, intercalate, snoc)
import Data.Either (Either(..))
import Data.Foldable (foldMap, traverse_)
import Data.List.NonEmpty (NonEmptyList, head, toList)
import Data.Maybe (Maybe(..), fromMaybe, isJust)
import Data.String (length, null)
import Data.String.Regex as Regex
import Data.String.Regex.Flags as Regex.Flags
import Data.Validation.Semigroup (V, andThen, invalid, isValid, toEither, unV)
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Class.Console as Console
import Foreign.Object as Object
import KSF.Registration.Component (RegistrationInputFieldError, emailInUseMsg)
import Persona as Persona
import React.Basic (JSX, make)
import React.Basic as React
import React.Basic.DOM as DOM
import React.Basic.DOM.Events (preventDefault, targetValue)
import React.Basic.Events (handler)
import Unsafe.Coerce (unsafeCoerce)

type Self = React.Self Props State

type ValidatedForm a = V (NonEmptyList ValidationError) a

type Props =
  { onRegister :: Aff Persona.LoginResponse -> Effect Unit
  , onCancelRegistration :: Effect Unit
  }

type State =
  { serverErrors :: Array ValidationError
  , formData :: FormData
  }

type FormData =
  { firstName           :: Maybe String
  , lastName            :: Maybe String
  , streetAddress       :: Maybe String
  , city                :: Maybe String
  , country             :: Maybe String
  , zipCode             :: Maybe String
  , phone               :: Maybe String
  , emailAddress        :: Maybe String
  , password            :: Maybe String
  , unvalidatedPassword :: Maybe String
  , confirmPassword     :: Maybe String
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
  | InvalidNotInitialized RegistrationInputField -- Fictional state only to be set when the form is first rendered
derive instance eqValidationError :: Eq ValidationError

registration :: Props -> JSX
registration = make component { initialState, render }

component :: React.Component Props
component = React.createComponent "Registration"

render :: Self -> JSX
render self = registrationForm
  [ DOM.div
      { className: "registration--container"
      , children:
          [ firstNameInput self, lastNameInput self
          , streetAddressInput self, cityInput self
          , zipCodeInput self, countryDropdown self
          , phoneInput self, emailAddressInput self
          , passwordInput self, confirmPasswordInput self
          , confirm self
          ]
      }
  ]
  where
    registrationForm :: Array JSX -> JSX
    registrationForm children =
      DOM.form
        { children
        , onSubmit: handler preventDefault $ (\_ -> submitForm self $ formValidations self)
        }

initialState :: State
initialState =
  { formData:
      { firstName:       Nothing
      , lastName:        Nothing
      , streetAddress:   Nothing
      , city:            Nothing
      , country:         Just "FI"
      , zipCode:         Nothing
      , phone:           Nothing
      , emailAddress:    Nothing
      , password:        Nothing
      , unvalidatedPassword: Nothing
      , confirmPassword: Nothing
      }
  , serverErrors: []
  }

inputLabel :: String -> String -> JSX
inputLabel labelText labelFor =
  DOM.label
    { className: "registration--input-label"
    , children: [ DOM.text labelText ]
    , htmlFor: labelFor
    }

type InputFieldAttributes =
  { label          :: String
  , type_          :: String
  , name           :: String
  , placeholder    :: String
  , value          :: Maybe String
  , onChange       :: Maybe String -> Effect Unit
  , onBlur         :: Maybe (Maybe String -> Effect Unit)
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
            , onBlur: handler targetValue $ fromMaybe (\_ -> pure unit) a.onBlur
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
firstNameInput self@{ state: { formData }} = inputField
  { type_: "text"
  , label: "Förnamn"
  , name: "firstName"
  , placeholder: "Förnamn"
  , onChange: (\val -> self.setState _ { formData { firstName = val } })
  , onBlur: Nothing
  , validatedInput: validateFirstName formData.firstName
  , value: formData.firstName
  }

lastNameInput :: Self -> JSX
lastNameInput self@{ state: { formData }} = inputField
  { type_: "text"
  , label: "Efternamn"
  , name: "lastName"
  , placeholder: "Efternamn"
  , onChange: (\val -> self.setState _ { formData { lastName = val }})
  , onBlur: Nothing
  , validatedInput: validateLastName formData.lastName
  , value: formData.lastName
  }

streetAddressInput :: Self -> JSX
streetAddressInput self@{ state: { formData }} = inputField
  { type_: "text"
  , label: "Adress"
  , name: "streetAddress"
  , placeholder: "Adress"
  , onChange: (\val -> self.setState _ { formData { streetAddress = val } })
  , onBlur: Nothing
  , validatedInput: validateStreetAddress formData.streetAddress
  , value: formData.streetAddress
  }

cityInput :: Self -> JSX
cityInput self@{ state: { formData }} = inputField
  { type_: "text"
  , label: "Stad"
  , name: "city"
  , placeholder: "Stad"
  , onChange: (\val -> self.setState _ { formData { city = val } })
  , onBlur: Nothing
  , validatedInput: validateCity formData.city
  , value: formData.city
  }

zipCodeInput :: Self -> JSX
zipCodeInput self@{ state: { formData }} = inputField
  { type_: "text"
  , label: "Postnummer"
  , name: "zipCode"
  , placeholder: "Postnummer"
  , onChange: (\val -> self.setState _ { formData { zipCode = val } })
  , onBlur: Nothing
  , validatedInput: validateZipCode formData.zipCode
  , value: formData.zipCode
  }

countryDropdown :: Self -> JSX
countryDropdown self@{ state: { formData }} =
  DOM.div
    { className: "registration--input-container"
    , children:
        [ inputLabel "Land" "country"
        , DOM.select
            { children: map createOption countries
            , onChange: handler targetValue (\val -> self.setState _ { formData { country = val } })
            , value: fromMaybe "FI" formData.country
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
phoneInput self@{ state: { formData }} = inputField
  { type_: "text"
  , label: "Telefon"
  , name: "phone"
  , placeholder: "Telefon"
  , onChange: (\val -> self.setState _ { formData { phone = val } })
  , onBlur: Nothing
  , validatedInput: validatePhone formData.phone
  , value: formData.phone
  }

emailAddressInput :: Self -> JSX
emailAddressInput self@{ state: { formData }} = inputField
  { type_: "email"
  , label: "E-postadress"
  , name: "emailAddress"
  , placeholder: "E-postadress"
  , onChange: (\val -> self.setState _ { formData { emailAddress = val } })
  , onBlur: Nothing
  , validatedInput: validateEmailAddress formData.emailAddress self.state.serverErrors
  , value: formData.emailAddress
  }

passwordInput :: Self -> JSX
passwordInput self = inputField
    { placeholder: "Lösenord (minst 6 tecken)"
    , type_: "password"
    , label: "Lösenord"
    , name: "password"
    -- We only validate the password when the user clicks out from the input field
    , onBlur: Just (\val -> self.setState _ { formData { password = val } })
    , onChange: \val -> self.setState _ { formData { unvalidatedPassword = val } }
    , value: self.state.formData.unvalidatedPassword
    , validatedInput: validatePassword self.state.formData.password
    }

confirmPasswordInput :: Self -> JSX
confirmPasswordInput self@{ state: { formData }} = inputField
    { placeholder: "Bekräfta lösenord"
    , type_: "password"
    , label: "Bekräfta lösenord"
    , name: "confirmPassword"
    , onBlur: Nothing
    , onChange: \val -> self.setState _ { formData { confirmPassword = val } }
    , value: formData.confirmPassword
    , validatedInput: validatePasswordComparison formData.password formData.confirmPassword
    }


confirm :: Self -> JSX
confirm self =
  DOM.div
    { className: "registration--submit"
    , children:
        [ acceptTermsText
        , validationErrorMessage
        , confirmButton
        , cancelText
        ]
    }
  where
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

    isFormInvalid
      | Left errs <- toEither $ formValidations self
      = not $ all isNotInitialized errs
      | otherwise = false

    confirmButton :: JSX
    confirmButton =
      DOM.input
        { type: "submit"
        , className: "registration--create-button mt2"
        , disabled: if isFormInvalid then true else false
        , value: "Skapa konto"
        }

    validationErrorMessage :: JSX
    validationErrorMessage
      | not isFormInvalid = mempty
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
            [ DOM.text "Genom att klicka på \"Skapa konto\", accepterar du våra "
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
        termsUrl         = "https://www.hbl.fi/bruksvillkor/#terms"
        privacyPolicyUrl = "https://www.hbl.fi/bruksvillkor/#privacy"


submitForm :: Self -> ValidatedForm FormData -> Effect Unit
submitForm self@{ state: { formData } } = unV
  (\errors   -> do
      -- Show validation errors to user (if not shown):
      -- As we don't show error messages when an input field is InvalidNotinitialized (when the value is Nothing),
      -- let's set all of the Nothing values to Just empty strings to vizualise errors to the user, when the submit button is clicked.
      -- We need to do this, because we don't want to show validation errors straight when the user comes to the registration page.
      self.setState _
        { formData
            { firstName       = formData.firstName       <|> Just ""
            , lastName        = formData.lastName        <|> Just ""
            , streetAddress   = formData.streetAddress   <|> Just ""
            , city            = formData.city            <|> Just ""
            , zipCode         = formData.zipCode         <|> Just ""
            , country         = formData.country         <|> Just ""
            , phone           = formData.phone           <|> Just ""
            , password        = formData.password        <|> Just ""
            , confirmPassword = formData.confirmPassword <|> Just ""
            }
        }
  )
  createUser
  where
    -- TODO: Clear self.state.serverErrors
    createUser form
      | Just user <- mkNewUser form = do
          self.props.onRegister $ Persona.register user `catchError` case _ of
            err | Just (errData :: Persona.EmailAddressInUseRegistration) <- Persona.errorData err -> do
                    Console.error errData.email_address_in_use_registration.description
                    liftEffect $ self.setState _ { serverErrors = InvalidEmailInUse emailInUseMsg `cons` self.state.serverErrors }
                    throwError err
                | Just (errData :: Persona.InvalidFormFields) <- Persona.errorData err -> do
                    Console.error errData.invalid_form_fields.description
                    liftEffect $ handleServerErrs errData.invalid_form_fields.errors
                    throwError err
                | otherwise -> do
                    Console.error "An unexpected error occurred during registration"
                    throwError err
      | otherwise = Console.error "Not all registration fields were filled."

    emailInUseMsg :: String
    emailInUseMsg =
      """E-postadressen är redan i bruk.
         Har du redan ett konto hos Hufvudstadsbladet, Västra Nyland eller Östnyland?
         Då kan du använda samma inloggning.
         Du kan också skapa konto med en annan adress."""

    handleServerErrs :: RegistrationInputFieldError -> Effect Unit
    handleServerErrs errs = do
      traverse_ setFormInvalid $ Object.keys errs
      where
        setFormInvalid key =
          -- NOTE & TODO:
          -- The "must have at least 6 chars" error is probably not always the case.
          -- The problem of displaying error message directly from Janrain
          -- is for example the language used (e.g. Swedish for Finnish users).
          -- Also, the error might be gibberish.
          case key of
            "emailAddress" -> self.setState _ { serverErrors = Invalid EmailAddress "Ogiltig E-postadress." `cons` self.state.serverErrors }
            "password"     -> self.setState _ { serverErrors = Invalid Password "Lösenordet måste ha minst 6 tecken." `cons` self.state.serverErrors }
            _              -> pure unit

mkNewUser :: FormData -> Maybe Persona.NewUser
mkNewUser f =
  { firstName: _
  , lastName: _
  , emailAddress: _
  , password: _
  , confirmPassword: _
  , streetAddress: _
  , city: _
  , zipCode: _
  , country: _
  , phone: _
  }
  <$> f.firstName
  <*> f.lastName
  <*> f.emailAddress
  <*> f.password
  <*> f.confirmPassword
  <*> f.streetAddress
  <*> f.city
  <*> f.zipCode
  <*> f.country
  <*> f.phone

formValidations :: Self -> ValidatedForm FormData
formValidations self@{ state: { formData } } =
  { firstName: _
  , lastName: _
  , streetAddress:    _
  , city: _
  , country: _
  , zipCode: _
  , phone: _
  , emailAddress: _
  , password: _
  , unvalidatedPassword: Nothing
  , confirmPassword: _
  }
  <$> validateFirstName formData.firstName
  <*> validateLastName formData.lastName
  <*> validateStreetAddress formData.streetAddress
  <*> validateCity formData.city
  <*> validateCountry formData.country
  <*> validateZipCode formData.zipCode
  <*> validatePhone formData.phone
  <*> validateEmailAddress formData.emailAddress self.state.serverErrors
  <*> validatePasswordLength formData.password
  <*> validatePasswordComparison formData.password formData.confirmPassword

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

validateEmailAddress :: Maybe String -> Array ValidationError -> ValidatedForm (Maybe String)
validateEmailAddress email serverErrors =
  validateServerError EmailAddress serverErrors email `andThen`
  validateEmptyField EmailAddress "E-postadress krävs." `andThen`
  validateInputWithRegex EmailAddress emailRegex "Ogiltig E-postadress."
  where
    -- From https://developer.mozilla.org/en-US/docs/Web/HTML/Element/input/email#Basic_validation
    emailRegex = "^[a-zA-Z0-9.!#$%&'*+\\/=?^_`{|}~-]+@[a-zA-Z0-9](?:[a-zA-Z0-9-]{0,61}[a-zA-Z0-9])?(?:\\.[a-zA-Z0-9](?:[a-zA-Z0-9-]{0,61}[a-zA-Z0-9])?)*$"

validatePassword :: Maybe String -> ValidatedForm (Maybe String)
validatePassword password = validateEmptyField Password "Lösenord krävs." password `andThen` validatePasswordLength

validatePasswordLength :: Maybe String -> ValidatedForm (Maybe String)
validatePasswordLength Nothing = notInitialized Password
validatePasswordLength password
  | Just pw <- password, length pw >= 6 = pure $ Just pw
  | otherwise = invalid $ pure $ Invalid Password "Lösenordet måste ha minst 6 tecken."

validatePasswordComparison :: Maybe String -> Maybe String -> ValidatedForm (Maybe String)
validatePasswordComparison Nothing Nothing = notInitialized Password
validatePasswordComparison password confirmedPassword
  | Just pw <- password
  , Just confirmedPw <- confirmedPassword
  , pw == confirmedPw
  = pure $ Just pw
  | otherwise = invalid $ pure $ Invalid ConfirmPassword "Lösenorden överensstämmer inte med varandra."

validateEmptyField :: RegistrationInputField -> String -> Maybe String -> ValidatedForm (Maybe String)
validateEmptyField field _ Nothing = notInitialized field
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

-- | NOTE: Even though `val` is not required by this function, it must be taken in and returned as is,
--         in order to keep chaining validation functions using `andThen` working.
validateServerError :: RegistrationInputField -> Array ValidationError -> Maybe String -> ValidatedForm (Maybe String)
validateServerError fieldName serverErrors val =
  case find ((_ == fieldName) <<< validationInputFieldOf) serverErrors of
    Just err -> invalid $ pure err
    Nothing  -> pure $ val

notInitialized :: RegistrationInputField -> ValidatedForm (Maybe String)
notInitialized field = invalid $ pure $ InvalidNotInitialized field

inputFieldErrorMessage :: ValidatedForm (Maybe String) -> Maybe String
inputFieldErrorMessage = unV handleInvalidField (\_ -> Nothing)
  where
    handleInvalidField errs
      -- If field is not initialized, it's considered to be valid
      | (InvalidNotInitialized _) <- head errs = Nothing
      | otherwise = Just $ validationErrorMessageOf $ head errs

validationErrorMessageOf :: ValidationError -> String
validationErrorMessageOf = case _ of
  Invalid _ err               -> err
  InvalidEmpty _ err          -> err
  InvalidPatternFailure _ err -> err
  InvalidEmailInUse err       -> err
  InvalidNotInitialized _     -> "NotInitialized"

validationInputFieldOf :: ValidationError -> RegistrationInputField
validationInputFieldOf = case _ of
  Invalid f _               -> f
  InvalidEmpty f _          -> f
  InvalidPatternFailure f _ -> f
  InvalidEmailInUse _       -> EmailAddress
  InvalidNotInitialized f   -> f

isNotInitialized :: ValidationError -> Boolean
isNotInitialized (InvalidNotInitialized _) = true
isNotInitialized _ = false
