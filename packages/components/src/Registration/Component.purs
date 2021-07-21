module KSF.Registration.Component where

import Prelude

import Control.Alt ((<|>))
import Control.Monad.Error.Class (throwError)
import Data.Array (cons, elem)
import Data.Either (Either(..))
import Data.Foldable (traverse_)
import Data.List.NonEmpty (all)
import Data.JSDate as JSDate
import Data.Maybe (Maybe(..))
import Data.Nullable as Nullable
import Data.Validation.Semigroup (toEither, validation)
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Class.Console as Console
import Effect.Exception (error)
import Foreign.Object as Object
import KSF.CountryDropDown (defaultCountryDropDown)
import KSF.InputField as InputField
import KSF.User as User
import KSF.ValidatableForm (class ValidatableField, ValidatedForm, ValidationError(..), inputFieldErrorMessage, isNotInitialized, noValidation, removeServerErrors, validateEmailAddress, validateEmptyField, validateField, validatePassword, validatePasswordComparison, validatePhone, validateWithServerErrors, validateFinnishZipCode, validateZipCode)
import Persona as Persona
import React.Basic (JSX)
import React.Basic.Classic (make)
import React.Basic.Classic as React
import React.Basic.DOM as DOM
import React.Basic.DOM.Events (preventDefault)
import React.Basic.Events (handler)

type Self = React.Self Props State

type Props =
  { onRegister :: Aff User.User -> Effect Unit
  , onCancelRegistration :: Effect Unit
  }

type State =
  { serverErrors :: Array (ValidationError RegistrationInputField)
  , formData :: FormData
  , usePhone :: Boolean
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
  | Zip (Maybe String)
  | Country
  | Phone
  | EmailAddress
  | Password
  | ConfirmPassword (Maybe String)
derive instance eqRegistrationInputField :: Eq RegistrationInputField
instance validatableFieldRegistrationInputField :: ValidatableField RegistrationInputField where
  validateField field value serverErrors = case field of
    FirstName                                  -> validateEmptyField FirstName "Förnamn krävs." value
    LastName                                   -> validateEmptyField LastName "Efternamn krävs." value
    StreetAddress                              -> validateEmptyField StreetAddress "Adress krävs." value
    City                                       -> validateEmptyField City "Stad krävs." value
    Country                                    -> validateEmptyField Country "Land krävs." value
    (Zip country)                              -> if country `elem` [ Just "FI", Just "AX", Just "SE" ]
                                                    then validateFinnishZipCode field value
                                                    else validateZipCode field value
    Phone                                      -> validatePhone field value
    EmailAddress                               -> validateWithServerErrors serverErrors EmailAddress value validateEmailAddress
    Password                                   -> validateWithServerErrors serverErrors Password value validatePassword
    confirmPw@(ConfirmPassword originalPassword) -> validatePasswordComparison Password confirmPw originalPassword value

registration :: Props -> JSX
registration = make component { initialState, render }

component :: React.Component Props
component = React.createComponent "Registration"

render :: Self -> JSX
render self = registrationForm
  [ DOM.div
      { className: "registration--container"
      , children:
          (map (\x -> x self.state self.setState)
            [ inputField FirstName, inputField LastName
            , inputField StreetAddress, inputField City
            , inputField (Zip self.state.formData.country), inputField Country
            , inputField Phone, inputField EmailAddress
            , inputField Password, inputField (ConfirmPassword self.state.formData.password)
            ]
          ) <> [ confirm self ]
      }
  ]
  where
    registrationForm :: Array JSX -> JSX
    registrationForm children =
      DOM.form
        { children: formTitle `cons` children
        , onSubmit: handler preventDefault $ (\_ -> submitForm self.state self.setState self.props.onRegister $ formValidations self.state)
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
  , usePhone: true
  }

inputField :: RegistrationInputField -> State -> ((State -> State) -> Effect Unit) -> JSX

inputField FirstName { formData } setState = InputField.inputField
  { type_: InputField.Text
  , label: Just "Förnamn"
  , name: "firstName"
  , placeholder: "Förnamn"
  , onChange: (\val -> setState _ { formData { firstName = val } })
  , validationError: inputFieldErrorMessage $ validateField FirstName formData.firstName []
  , value: formData.firstName
  }

inputField LastName { formData } setState = InputField.inputField
  { type_: InputField.Text
  , label: Just "Efternamn"
  , name: "lastName"
  , placeholder: "Efternamn"
  , onChange: (\val -> setState _ { formData { lastName = val } })
  , validationError: inputFieldErrorMessage $ validateField LastName formData.lastName []
  , value: formData.lastName
  }

inputField StreetAddress { formData } setState = InputField.inputField
  { type_: InputField.Text
  , label: Just "Adress"
  , name: "streetAddress"
  , placeholder: "Adress"
  , onChange: (\val -> setState _ { formData { streetAddress = val } })
  , validationError: inputFieldErrorMessage $ validateField StreetAddress formData.streetAddress []
  , value: formData.streetAddress
  }

inputField City { formData } setState = InputField.inputField
  { type_: InputField.Text
  , label: Just "Stad"
  , name: "city"
  , placeholder: "Stad"
  , onChange: (\val -> setState _ { formData { city = val } })
  , validationError: inputFieldErrorMessage $ validateField City formData.city []
  , value: formData.city
  }

inputField (Zip _) { formData } setState = InputField.inputField
  { type_: InputField.Text
  , label: Just "Postnummer"
  , name: "zipCode"
  , placeholder: "Postnummer"
  , onChange: (\val -> setState _ { formData { zipCode = val } })
  , validationError: inputFieldErrorMessage $ validateField (Zip formData.country) formData.zipCode []
  , value: formData.zipCode
  }

inputField Country { formData } setState =
  defaultCountryDropDown (\val -> setState _ { formData { country = val } }) formData.country

inputField Phone { formData } setState = InputField.inputField
  { type_: InputField.Text
  , label: Just "Telefon"
  , name: "phone"
  , placeholder: "Telefon"
  , onChange: (\val -> setState _ { formData { phone = val } })
  , validationError: inputFieldErrorMessage $ validateField Phone formData.phone []
  , value: formData.phone
  }

inputField EmailAddress { formData, serverErrors } setState = InputField.inputField
  { type_: InputField.Email
  , label: Just "E-postadress"
  , name: "emailAddress"
  , placeholder: "E-postadress"
  , onChange: (\val -> setState _ { formData { emailAddress = val }
                                    -- Clear server errors of EmailAddress when typing
                                  , serverErrors = removeServerErrors EmailAddress serverErrors
                                  })
  , validationError: inputFieldErrorMessage $ validateField EmailAddress formData.emailAddress serverErrors
  , value: formData.emailAddress
  }

inputField Password { formData, serverErrors } setState = InputField.inputField
    { placeholder: "Lösenord (minst 6 tecken)"
    , type_: InputField.Password
    , label: Just "Lösenord"
    , name: "password"
    , onChange: \val -> setState _ { formData { password = val }
                                     -- Clear server errors of Password when typing
                                   , serverErrors = removeServerErrors Password serverErrors
                                   }
    , value: formData.password
    , validationError: inputFieldErrorMessage $ validateField Password formData.password serverErrors
    }

inputField (ConfirmPassword _) { formData } setState = InputField.inputField
    { placeholder: "Bekräfta lösenord"
    , type_: InputField.Password
    , label: Just "Bekräfta lösenord"
    , name: "confirmPassword"
    , onChange: \val -> setState _ { formData { confirmPassword = val } }
    , value: formData.confirmPassword
    , validationError: inputFieldErrorMessage $ validateField (ConfirmPassword formData.password) formData.confirmPassword []
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
      | Left errs <- toEither $ formValidations self.state
      = not $ all isNotInitialized errs
      | otherwise = false

submitForm :: State -> ((State -> State) -> Effect Unit) -> (Aff User.User -> Effect Unit) -> ValidatedForm RegistrationInputField FormData -> Effect Unit
submitForm state@{ formData } setState onRegister = validation
  (\_   -> do
      -- Show validation errors to user (if not shown):
      -- As we don't show error messages when an input field is InvalidNotinitialized (when the value is Nothing),
      -- let's set all of the Nothing values to Just empty strings to vizualise errors to the user, when the submit button is clicked.
      -- We need to do this, because we don't want to show validation errors straight when the user comes to the registration page.
      setState _
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
  \validForm -> do
    nowISO <- JSDate.toISOString =<< JSDate.now
    createUser nowISO validForm
  where
    createUser date form
      | Just user <- mkNewUser form date = onRegister do
          createdUser <- User.createUser user
          case createdUser of
            Right u -> pure u
            Left User.RegistrationEmailInUse -> do
              liftEffect $ setState _ { serverErrors = InvalidEmailInUse EmailAddress emailInUseMsg `cons` state.serverErrors }
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
            "emailAddress" -> setState _ { serverErrors = Invalid EmailAddress "Ogiltig E-postadress." `cons` state.serverErrors }
            "password"     -> setState _ { serverErrors = Invalid Password "Lösenordet måste ha minst 6 tecken." `cons` state.serverErrors }
            _              -> pure unit

mkNewUser :: FormData -> String -> Maybe Persona.NewUser
mkNewUser f nowISO =
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
  , legalConsents:
      [ { consentId: "legal_acceptance_v1"
        , screenName: "legalAcceptanceScreen"
        , dateAccepted: nowISO
        }
      ]
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
  <*> (pure $ Nullable.toNullable f.phone)

formValidations :: State -> ValidatedForm RegistrationInputField FormData
formValidations state@{ formData } =
  { firstName: _
  , lastName: _
  , streetAddress: _
  , city: _
  , country: _
  , zipCode: _
  , phone: _
  , emailAddress: _
  , password: _
  , confirmPassword: _
  }
  <$> validateField FirstName formData.firstName []
  <*> validateField LastName formData.lastName []
  <*> validateField StreetAddress formData.streetAddress []
  <*> validateField City formData.city []
  <*> validateField Country formData.country []
  <*> validateField (Zip formData.country) formData.zipCode []
  <*> (if state.usePhone then validateField Phone formData.phone [] else noValidation formData.phone)
  <*> validateField EmailAddress formData.emailAddress state.serverErrors
  <*> validateField Password formData.password state.serverErrors
  <*> validateField (ConfirmPassword formData.password) formData.confirmPassword []
