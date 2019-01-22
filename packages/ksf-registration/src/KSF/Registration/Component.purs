module KSF.Registration.Component where

import Prelude

import Data.Array (zipWith)
import Data.Either (Either)
import Data.Foldable (all)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Aff as Aff
import Effect.Class.Console as Console
import Effect.Exception (Error)
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
  { onRegister :: Either Error Persona.LoginResponse -> Effect Unit }

type JSProps =
  { onRegister :: Either Error Persona.LoginResponse -> Effect Unit }

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
  , inputValidations ::
       { password :: Validation }
  }

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

type InputAttributes =
  { placeholder :: String
  , name :: String
  , onChange :: (Maybe String -> Effect Unit)
  }

type InputType = String

data Validation = Valid | Invalid
derive instance eqValidation :: Eq Validation

type Pattern = String

data Action =
  UpdateInput RegistrationInputField (Maybe String)
  | PasswordMissmatch Validation

jsComponent :: React.ReactComponent JSProps
jsComponent = React.toReactComponent fromJSProps component { initialState, render, update }

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
  , inputValidations:
     { password: Valid }
  }

fromJSProps :: JSProps -> Props
fromJSProps jsProps = jsProps

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

  PasswordMissmatch validation ->
    Update self.state { inputValidations { password = validation } }

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
            (\_ -> when isFormValid do submit)
        }
      where
        isFormValid = all (eq Valid) [ self.state.inputValidations.password ]
        submit = do
          let maybeUser =
                newUser
                <$> self.state.firstName
                <*> self.state.lastName
                <*> self.state.emailAddress
                <*> self.state.password
                <*> self.state.streetAddress
                <*> self.state.city
                <*> self.state.zip
                <*> self.state.country
                <*> self.state.phone
          case maybeUser of
            Just user -> Aff.runAff_ self.props.onRegister $ Persona.register user
            Nothing   -> Console.error "Not all registration fields were filled."

        newUser
          firstName
          lastName
          emailAddress
          password
          streetAddress
          city
          zipCode
          country
          phone =
            { firstName, lastName, emailAddress, password, streetAddress, city, zipCode, country, phone }

    inputFieldUpdate :: RegistrationInputField -> Maybe String -> Effect Unit
    inputFieldUpdate field newInputValue = do
      send self (UpdateInput field newInputValue)

    firstNameInput :: JSX
    firstNameInput =
      createTextInput
        { placeholder: "Förnamn"
        , name: "firstName"
        , onChange: inputFieldUpdate FirstName
        }

    lastNameInput :: JSX
    lastNameInput =
      createTextInput
        { placeholder: "Efternamn"
        , name: "lastName"
        , onChange: inputFieldUpdate LastName
        }

    addressInput :: JSX
    addressInput =
      createTextInput
        { placeholder: "Adress"
        , name: "address"
        , onChange: inputFieldUpdate StreetAddress
        }

    cityInput :: JSX
    cityInput =
      createTextInput
        { placeholder: "Stad"
        , name: "city"
        , onChange: inputFieldUpdate City
        }

    zipInput :: JSX
    zipInput =
      createPatternInput
        { placeholder: "Postnummer"
        , name: "zip"
        , onChange: inputFieldUpdate Zip
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
        }
        phoneRegexPattern
      where
       --  Allows digits, "+" and whitespaces
       phoneRegexPattern = "[\\d|\\+|\\s]+"

    emailInput :: JSX
    emailInput =
      createTextInput
        { placeholder: "E-postadress"
        , name: "email"
        , onChange: inputFieldUpdate EmailAddress
        }

    passwordInput :: JSX
    passwordInput =
      createPasswordInput
        { placeholder: "Lösenord"
        , name: "password"
        , onChange: inputFieldUpdate Password
        }

    confirmPasswordInput :: JSX
    confirmPasswordInput =
      DOM.div
        { className: inCaseOfMissmatch "registration--invalid-form-field"
        , children:
            [ DOM.input
                { type: "password"
                , required: true
                , onBlur: handler targetValue comparePasswords
                , placeholder: "Bekräfta lösenord"
                , name: "confirm-password"
                }
            , inCaseOfMissmatch
                DOM.div
                  { className: "mt1 registration--invalid-form-text"
                  , children: [ DOM.text "Lösenorden överensstämmer inte med varandra." ]
                  }
            ]
        }
      where
        inCaseOfMissmatch :: forall a. Monoid a => a -> a
        inCaseOfMissmatch invalidAction =
          case self.state.inputValidations.password of
            Invalid -> invalidAction
            Valid   -> mempty
        comparePasswords confirmedPassword
          | confirmedPassword /= self.state.password = send self (PasswordMissmatch Invalid)
          | otherwise = send self (PasswordMissmatch Valid)

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
        }
      where
        createOption value description =
          DOM.option
            { value
            , children: [ DOM.text description ]
            }

registrationTitle :: JSX
registrationTitle =
  DOM.div
    { className: "col-12 mx-auto"
    , children:
        [ DOM.h1_ [ DOM.text "Skapa din konto" ] ]
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
            , children: [ DOM.text "avbryt." ]
            }
        ]
    }

inputRow :: JSX -> JSX -> JSX
inputRow leftInput rightInput =
  DOM.div
    { className: "clearfix flex justify-center mt2"
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
    { className: "col col-4 registration--input ml4"
    , children
    }

createTextInput :: InputAttributes -> JSX
createTextInput inputAttrs =
  createInput inputAttrs "text"

createPasswordInput :: InputAttributes -> JSX
createPasswordInput inputAttrs =
  createInput inputAttrs "password"

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
createInput { placeholder, name, onChange } type_ =
  DOM.input
    { type: type_
    , required: true
    , onChange: handler targetValue onChange
    , placeholder
    , name
    }
