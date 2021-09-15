module KSF.CountryDropDown where

import Prelude

import Data.Maybe (Maybe, fromMaybe)
import Effect (Effect)
import KSF.InputField (inputLabel)
import React.Basic (JSX)
import React.Basic.DOM as DOM
import React.Basic.DOM.Events (targetValue)
import React.Basic.Events (handler)



countryDropDown :: Array DropDownCountry -> Boolean -> (Maybe String -> Effect Unit) -> Maybe String -> JSX
countryDropDown countries disabled onChange value =
  DOM.div
    { className: "input-field--container"
    , children:
        [ inputLabel { label: "Land", nameFor: "country" }
        , DOM.select
            { children: map createOption countries
            , onChange: handler targetValue onChange
            , value: fromMaybe "FI" value
            }
        ]
    }
  where
    createOption { countryCode, countryName } =
      DOM.option
        { value: countryCode
        , children: [ DOM.text countryName ]
        , disabled: disabled
        }

defaultCountryDropDown :: (Maybe String -> Effect Unit) -> Maybe String -> JSX
defaultCountryDropDown =
  countryDropDown countries false
  where
    countries =
      [ { countryCode: "FI", countryName: "Finland" }
      , { countryCode: "AX", countryName: "Åland" }
      , { countryCode: "SE", countryName: "Sverige" }
      , { countryCode: "NO", countryName: "Norge" }
      , { countryCode: "DK", countryName: "Danmark" }
      ]

limitedCountries :: Array DropDownCountry
limitedCountries =
  [ { countryCode: "FI", countryName: "Finland" }
  , { countryCode: "AX", countryName: "Åland" }
  ]

countryChangeMessage :: JSX
countryChangeMessage =
  DOM.div
    { className: "mitt-konto--note"
    , children: [ DOM.text "Vid ändring till en utländsk adress vänligen kontakta Kundservice" ]
    }

type DropDownCountry =
  { countryCode :: String
  , countryName :: String
  }
