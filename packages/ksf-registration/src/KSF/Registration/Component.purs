module KSF.Registration.Component where

import Prelude

import Data.Array (zipWith)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String (length)
import Effect (Effect)
import Effect.Class.Console as Console
import KSF.Button.Component as Button
import KSF.InputField.Component as InputField
import KSF.Registration.View as View
import React.Basic (JSX, StateUpdate(..), make, send)
import React.Basic as React
import React.Basic.DOM as DOM
import React.Basic.Extended (Style)
import React.Basic.Extended as React.Extended

foreign import registrationStyles :: Style

type Self = React.Self Props State Void
type Props = {}

type JSProps = {}

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
       { passwordMissmatch :: Boolean }
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
  , onChange :: (String -> Effect Unit)
  }

type InputType = String

data Action =
  UpdateInput RegistrationInputField String
  | PasswordMissmatch Boolean

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
  , country: Nothing
  , phone: Nothing
  , emailAddress: Nothing
  , password: Nothing
  , inputValidations:
     { passwordMissmatch: false }
  }

fromJSProps :: JSProps -> Props
fromJSProps _ = {}

component :: React.Component Props
component = React.createComponent "Registration"

update :: Self -> Action -> StateUpdate Props State Action
update self = case _ of
  UpdateInput FirstName newValue ->
    Update self.state { firstName = Just newValue }
  UpdateInput LastName newValue ->
    Update self.state { lastName = Just newValue }
  UpdateInput StreetAddress newValue ->
    Update self.state { streetAddress = Just newValue }
  UpdateInput City newValue ->
    Update self.state { city = Just newValue }
  UpdateInput Zip newValue ->
    Update self.state { zip = Just newValue }
  UpdateInput Country newValue ->
    Update self.state { country = Just newValue }
  UpdateInput Phone newValue ->
    Update self.state { phone = Just newValue }
  UpdateInput EmailAddress newValue ->
    Update self.state { emailAddress = Just newValue }
  UpdateInput Password newValue ->
    Update self.state { password = Just newValue }

  PasswordMissmatch missmatch ->
    Update self.state { inputValidations { passwordMissmatch = missmatch } }

render :: Self -> JSX
render self =
  React.Extended.requireStyle
    registrationStyles
    $ DOM.div
        { className: "registration--container clearfix"
        , children:
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
        }
  where
    inputFieldUpdate :: RegistrationInputField -> String -> Effect Unit
    inputFieldUpdate field newInputValue = do
      Console.log newInputValue
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
      createTextInput
        { placeholder: "Postnummer"
        , name: "zip"
        , onChange: inputFieldUpdate Zip
        }

    phoneInput :: JSX
    phoneInput =
      createTextInput
        { placeholder: "Telefon"
        , name: "phone"
        , onChange: inputFieldUpdate Phone
        }

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
        { className: invalidFieldClass
        , children:
            [ React.element
                InputField.component
                  { type_: "password"
                  , required: true
                  , children: []
                  , defaultValue: Nothing
                  , onChange: (\_ -> pure unit)
                  , onBlur: comparePasswords
                  , placeholder: "Bekräfta lösenord"
                  , name: "confirm-password"
                  }
            ]
        }
      where
        currentPassword = fromMaybe "" self.state.password
        comparePasswords confirmedPassword
          | confirmedPassword /= currentPassword = send self (PasswordMissmatch true)
          | otherwise = send self (PasswordMissmatch false)
        invalidFieldClass =
          if self.state.inputValidations.passwordMissmatch
          then "registration--invalid-form-field"
          else ""

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
  DOM.div
    { className: "registration--create-button mt2"
    , children:
        [ Button.button
            { description: "Skapa konto"
            , destination: Nothing
            , onClick: Console.log "YEP!"
            , onLoad: (\_ -> pure unit)
            }
        ]
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
    }
  where
    createOption value description =
      DOM.option
        { value
        , children: [ DOM.text description ]
        }


createTextInput :: InputAttributes -> JSX
createTextInput inputAttrs =
  createInput inputAttrs "text"

createPasswordInput :: InputAttributes -> JSX
createPasswordInput inputAttrs =
  createInput inputAttrs "password"

createInput :: InputAttributes -> InputType -> JSX
createInput { placeholder, name, onChange } type_ =
  React.element
    InputField.component
      { type_
      , required: true
      , children: []
      , defaultValue: Nothing
      , onChange
      , onBlur: (\_ -> pure unit)
      , placeholder
      , name
      }
