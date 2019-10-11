module KSF.Registration.Component where

import Prelude

import Control.Alt ((<|>))
import Control.Monad.Error.Class (throwError)
import Data.Array (all, cons, filter, find, snoc)
import Data.Either (Either(..))
import Data.Foldable (foldMap, traverse_)
import Data.List.NonEmpty (NonEmptyList, head)
import Data.Maybe (Maybe(..), fromMaybe, isNothing)
import Data.String (length, null)
import Data.String.Regex as Regex
import Data.String.Regex.Flags as Regex.Flags
import Data.Validation.Semigroup (V, andThen, invalid, toEither, unV)
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Class.Console as Console
import Effect.Exception (error)
import Foreign.Object as Object
import KSF.CountryDropDown (countryDropDown)
import KSF.InputField.Component as InputField
import KSF.User as User
import KSF.ValidatableForm (class ValidatableField, ValidatedForm, ValidationError(..), inputFieldErrorMessage, isNotInitialized, notInitialized, validateEmptyField, validateField, validatePhone, validateServerError, validateZipCode, validationInputFieldOf)
import Persona as Persona
import React.Basic (JSX, make)
import React.Basic as React
import React.Basic.DOM as DOM
import React.Basic.DOM.Events (preventDefault, targetValue)
import React.Basic.Events (handler)

type Self = React.Self Props State

type Props =
  { onRegister :: Aff User.User -> Effect Unit
  , onCancelRegistration :: Effect Unit
  }

type State =
  { serverErrors :: Array (ValidationError RegistrationInputField)
  , formData :: FormData
  }

type FormData =
  { firstName       :: Maybe String
  , lastName        :: Maybe String
  , streetAddress   :: Maybe String
  , city            :: Maybe String
  , country         :: Maybe String
  , zipCode         :: Maybe String
  , phone           :: Maybe String
  , emailAddress    :: Maybe String
  , password        :: Maybe String
  , confirmPassword :: Maybe String
  }

data RegistrationInputField
  = FirstName
  | LastName
  | StreetAddress
  | City
  | Zip
  | Country
  | Phone
  | EmailAddress (Array (ValidationError RegistrationInputField))
  | Password (Array (ValidationError RegistrationInputField))
  | ConfirmPassword
derive instance eqRegistrationInputField :: Eq RegistrationInputField
derive instance ordRegistrationInputField :: Ord RegistrationInputField
instance validatableFieldRegistrationInputField :: ValidatableField RegistrationInputField where
  validateField field value = case field of
    FirstName -> validateEmptyField FirstName "Förnamn krävs."
    LastName  -> validateEmptyField LastName "Efternamn krävs."
    StreetAddress -> validateEmptyField StreetAddress "Adress krävs."
    City -> validateEmptyField City "Stad krävs."
    Country -> validateEmptyField Country "Land krävs."
    Zip -> validateZipCode field value
    Phone -> validatePhone field value
    EmailAddress serverErrs -> validateEmailAddress field value serverErrs

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
          , zipCodeInput self, countryInput self
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
        { children: formTitle `cons` children
        , onSubmit: handler preventDefault $ (\_ -> submitForm self $ formValidations self)
        }
      where
        formTitle = DOM.h1 { className: "registration--form-title", children: [ DOM.text "Skapa ditt konto" ] }

initialState :: State
initialState =
  { formData:
      { firstName:           Nothing
      , lastName:            Nothing
      , streetAddress:       Nothing
      , city:                Nothing
      , country:             Just "FI"
      , zipCode:             Nothing
      , phone:               Nothing
      , emailAddress:        Nothing
      , password:            Nothing
      , confirmPassword:     Nothing
      }
  , serverErrors: []
  }

-- inputLabel :: String -> String -> JSX
-- inputLabel labelText labelFor =
--   DOM.label
--     { className: "registration--input-label"
--     , children: [ DOM.text labelText ]
--     , htmlFor: labelFor
--     }

-- type InputFieldAttributes =
--   { label          :: String
--   , type_          :: String
--   , name           :: String
--   , placeholder    :: String
--   , value          :: Maybe String
--   , onChange       :: Maybe String -> Effect Unit
--   , validatedInput :: ValidatedForm (Maybe String)
--   }

-- inputField :: InputFieldAttributes -> JSX
-- inputField a =
--   DOM.div
--     { className: "registration--input-container"
--     , children:
--         [ inputLabel a.label a.name
--         , DOM.input
--             { type: a.type_
--             , placeholder: a.label
--             , name: a.name
--             , value: fromMaybe "" a.value
--             , onChange: handler targetValue a.onChange
--             , className: if isValidOrNotInitialized a.validatedInput
--                            then ""
--                            else "registration--invalid-form-field"
--             }
--         ] `snoc` foldMap errorMessage (inputFieldErrorMessage a.validatedInput)
--     }
--   where
--     errorMessage e =
--       DOM.span
--         { className: "registration--invalid-form-text"
--         , children: [ DOM.text e ]
--         }

firstNameInput :: Self -> JSX
firstNameInput self@{ state: { formData }} = InputField.inputField
  { type_: "text"
  , label: "Förnamn"
  , name: "firstName"
  , placeholder: "Förnamn"
  , onChange: (\val -> self.setState _ { formData { firstName = val } })
  , validationError: inputFieldErrorMessage $ validateField FirstName formData.firstName
  , value: formData.firstName
  }

lastNameInput :: Self -> JSX
lastNameInput self@{ state: { formData }} = InputField.inputField
  { type_: "text"
  , label: "Efternamn"
  , name: "lastName"
  , placeholder: "Efternamn"
  , onChange: (\val -> self.setState _ { formData { lastName = val }})
  , validate: inputFieldErrorMessage $ validateField LastName formData.lastName
  , value: formData.lastName
  }

streetAddressInput :: Self -> JSX
streetAddressInput self@{ state: { formData }} = InputField.inputField
  { type_: "text"
  , label: "Adress"
  , name: "streetAddress"
  , placeholder: "Adress"
  , onChange: (\val -> self.setState _ { formData { streetAddress = val } })
  , validatedInput: inputFieldErrorMessage $ validateField StreetAddress formData.streetAddress
  , value: formData.streetAddress
  }

cityInput :: Self -> JSX
cityInput self@{ state: { formData }} = InputField.inputField
  { type_: "text"
  , label: "Stad"
  , name: "city"
  , placeholder: "Stad"
  , onChange: (\val -> self.setState _ { formData { city = val } })
  , validatedInput: inputFieldErrorMessage $ validateField City formData.city
  , value: formData.city
  }

zipCodeInput :: Self -> JSX
zipCodeInput self@{ state: { formData }} = InputField.inputField
  { type_: "text"
  , label: "Postnummer"
  , name: "zipCode"
  , placeholder: "Postnummer"
  , onChange: (\val -> self.setState _ { formData { zipCode = val } })
  , validationError: inputFieldErrorMessage $ validateField Zip formData.zipCode
  , value: formData.zipCode
  }

countryInput :: Self -> JSX
countryInput self@{ state: { formData }} =
  countryDropDown (\val -> self.setState _ { formData { country = val } }) formData.country

phoneInput :: Self -> JSX
phoneInput self@{ state: { formData }} = InputField.inputField
  { type_: "text"
  , label: "Telefon"
  , name: "phone"
  , placeholder: "Telefon"
  , onChange: (\val -> self.setState _ { formData { phone = val } })
  , validatedInput: validatePhone formData.phone
  , value: formData.phone
  }

emailAddressInput :: Self -> JSX
emailAddressInput self@{ state: { formData }} = InputField.inputField
  { type_: "email"
  , label: "E-postadress"
  , name: "emailAddress"
  , placeholder: "E-postadress"
  , onChange: (\val -> self.setState _ { formData { emailAddress = val }
                                         -- Clear server errors of EmailAddress when typing
                                       , serverErrors = removeServerErrors EmailAddress self.state.serverErrors
                                       })
  , validatedInput: validateEmailAddress formData.emailAddress self.state.serverErrors
  , value: formData.emailAddress
  }

passwordInput :: Self -> JSX
passwordInput self@{ state: { formData }} = InputField.inputField
    { placeholder: "Lösenord (minst 6 tecken)"
    , type_: "password"
    , label: "Lösenord"
    , name: "password"
    , onChange: \val -> self.setState _ { formData { password = val }
                                         -- Clear server errors of Password when typing
                                        , serverErrors = removeServerErrors Password self.state.serverErrors
                                        }
    , value: formData.password
    , validatedInput: validatePassword formData.password self.state.serverErrors
    }

confirmPasswordInput :: Self -> JSX
confirmPasswordInput self@{ state: { formData }} = InputField.inputField
    { placeholder: "Bekräfta lösenord"
    , type_: "password"
    , label: "Bekräfta lösenord"
    , name: "confirmPassword"
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
    acceptTermsText :: JSX
    acceptTermsText =
      let termsUrl         = "https://www.hbl.fi/bruksvillkor/#terms"
          privacyPolicyUrl = "https://www.hbl.fi/bruksvillkor/#privacy"
      in DOM.div
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

    validationErrorMessage :: JSX
    validationErrorMessage
      | not isFormInvalid = mempty
      | otherwise =
          DOM.div
            { className: "registration--invalid-form-generic-message mt2"
            , children: [ DOM.text "Alla obligatoriska fält är inte korrekt ifyllda, kontrollera uppgifterna." ]
            }

    confirmButton :: JSX
    confirmButton =
      DOM.input
        { type: "submit"
        , className: "registration--create-button mt2"
        , disabled: isFormInvalid
        , value: "Skapa konto"
        }

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
    createUser form
      | Just user <- mkNewUser form = self.props.onRegister do
          createdUser <- User.createUser user
          case createdUser of
            Right u -> pure u
            Left User.RegistrationEmailInUse -> do
              liftEffect $ self.setState _ { serverErrors = InvalidEmailInUse emailInUseMsg `cons` self.state.serverErrors }
              throwError $ error "email in use"
            Left (User.InvalidFormFields errors) -> do
              liftEffect $ handleServerErrs errors
              throwError $ error "invalid form fields"
            _ -> do
              Console.error unexpectedErr
              throwError $ error unexpectedErr
              where
                unexpectedErr = "An unexpected error occurred during registration"
      | otherwise = Console.error "Not all registration fields were filled."

    emailInUseMsg :: String
    emailInUseMsg =
      """E-postadressen är redan i bruk.
         Har du redan ett konto hos Hufvudstadsbladet, Västra Nyland eller Östnyland?
         Då kan du använda samma inloggning.
         Du kan också skapa konto med en annan adress."""

    handleServerErrs :: User.ValidationServerError -> Effect Unit
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
  , confirmPassword: _
  }
  <$> validateField FirstName formData.firstName
  <*> validateField LastName formData.lastName
  <*> validateField StreetAddress formData.streetAddress
  <*> validateField City formData.city
  <*> validateField Country formData.country
  <*> validateField Zip formData.zipCode
  <*> validateField Phone formData.phone
  <*> validateField EmailAddress formData.emailAddress -- self.state.serverErrors
  <*> validateField Password formData.password
  <*> validateField ConfirmPassword formData.password formData.confirmPassword

validateEmailAddress :: Maybe String -> Array ValidationError -> ValidatedForm (Maybe String)
validateEmailAddress email serverErrors =
  validateServerError EmailAddress serverErrors email `andThen`
  validateEmailAddress EmailAddress
  where
    -- From https://developer.mozilla.org/en-US/docs/Web/HTML/Element/input/email#Basic_validation
    emailRegex = "^[a-zA-Z0-9.!#$%&'*+\\/=?^_`{|}~-]+@[a-zA-Z0-9](?:[a-zA-Z0-9-]{0,61}[a-zA-Z0-9])?(?:\\.[a-zA-Z0-9](?:[a-zA-Z0-9-]{0,61}[a-zA-Z0-9])?)*$"

validatePassword :: RegistrationInputField -> Maybe String -> ValidatedForm RegistrationInputField (Maybe String)
validatePassword p@(Password serverErrors) password =
  validateServerError p password `andThen`
  validateEmptyField Password "Lösenord krävs." `andThen`
  validatePasswordLength

validatePasswordLength :: Maybe String -> ValidatedForm RegistrationInputField (Maybe String)
validatePasswordLength Nothing = notInitialized $ Password []
validatePasswordLength password
  | Just pw <- password, length pw >= 6 = pure $ Just pw
  | otherwise = invalid $ pure $ Invalid (Password []) "Lösenordet måste ha minst 6 tecken."

validatePasswordComparison :: Maybe String -> Maybe String -> ValidatedForm RegistrationInputField (Maybe String)
validatePasswordComparison Nothing Nothing = notInitialized $ Password []
validatePasswordComparison password confirmedPassword
  | Just pw <- password
  , Just confirmedPw <- confirmedPassword
  , pw == confirmedPw
  = pure $ Just pw
  | otherwise = invalid $ pure $ Invalid ConfirmPassword "Lösenorden överensstämmer inte med varandra."

removeServerErrors :: RegistrationInputField -> Array ValidationError -> Array ValidationError
removeServerErrors field serverErrors = filter ((field /= _) <<< validationInputFieldOf) serverErrors
