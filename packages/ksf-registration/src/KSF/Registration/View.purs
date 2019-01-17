module KSF.Registration.View where

import Prelude

import Data.Array (zipWith)
import Data.Maybe (Maybe(..))
import Effect.Class.Console as Console
import KSF.Button.Component as Button
import KSF.InputField.Component as InputField
import React.Basic (JSX)
import React.Basic.Compat as React
import React.Basic.DOM as DOM
import React.Basic.Extended (Style)
import React.Basic.Extended as React.Extended

foreign import registrationStyles :: Style

type InputAttributes =
  { placeholder :: String
  , name :: String
  }

type InputType = String

registration :: JSX
registration =
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
                (halfInputRow [ confirmButton ])
            ]
        }

registrationTitle :: JSX
registrationTitle =
  DOM.div
    { className: "col-12 mx-auto"
    , children:
        [ DOM.h1_ [ DOM.text "Skapa din konto" ] ]
    }

confirmButton :: JSX
confirmButton =
  DOM.div
    { className: "registration--create-button"
    , children:
        [ Button.button
            { description: "Skapa konto"
            , destination: Nothing
            , onClick: Console.log "YEP!"
            , onLoad: (\_ -> pure unit)
            }
        ]
    }

inputRow :: JSX -> JSX -> JSX
inputRow leftInput rightInput =
  DOM.div
    { className: "clearfix flex justify-center mt3"
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

firstNameInput :: JSX
firstNameInput =
  createTextInput
    { placeholder: "Förnamn"
    , name: "firstName"
    }

lastNameInput :: JSX
lastNameInput =
  createTextInput
    { placeholder: "Efternamn"
    , name: "lastName"
    }

addressInput :: JSX
addressInput =
  createTextInput
    { placeholder: "Adress"
    , name: "address"
    }

cityInput :: JSX
cityInput =
  createTextInput
    { placeholder: "Stad"
    , name: "city"
    }

zipInput :: JSX
zipInput =
  createTextInput
    { placeholder: "Postnummer"
    , name: "zip"
    }

phoneInput :: JSX
phoneInput =
  createTextInput
    { placeholder: "Telefon"
    , name: "phone"
    }

emailInput :: JSX
emailInput =
  createTextInput
    { placeholder: "E-postadress"
    , name: "email"
    }

passwordInput :: JSX
passwordInput =
  createPasswordInput
    { placeholder: "Lösenord"
    , name: "password"
    }

confirmPasswordInput :: JSX
confirmPasswordInput =
  createPasswordInput
    { placeholder: "Bekräfta lösenord"
    , name: "confirm-password"
    }

createTextInput :: InputAttributes -> JSX
createTextInput inputAttrs =
  createInput inputAttrs "text"

createPasswordInput :: InputAttributes -> JSX
createPasswordInput inputAttrs =
  createInput inputAttrs "password"

createInput :: InputAttributes -> InputType -> JSX
createInput { placeholder, name } type_ =
  React.element
    InputField.component
      { type_
      , required: true
      , children: []
      , defaultValue: Nothing
      , onChange: (\_ -> pure unit)
      , placeholder
      , name
      }
