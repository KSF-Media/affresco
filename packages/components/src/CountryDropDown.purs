module KSF.CountryDropDown where

import Prelude

import Data.Maybe (Maybe, fromMaybe)
import Effect (Effect)
import KSF.InputField.Component (inputLabel)
import React.Basic (JSX)
import React.Basic.DOM as DOM
import React.Basic.DOM.Events (targetValue)
import React.Basic.Events (handler)



countryDropDown :: Array DropDownCountry -> (Maybe String -> Effect Unit) -> Maybe String -> JSX
countryDropDown countries onChange value =
  DOM.div
    { className: "input-field--container"
    , children:
        [ inputLabel "Land" "country"
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
        }

defaultCountryDropDown :: (Maybe String -> Effect Unit) -> Maybe String -> JSX
defaultCountryDropDown =
  countryDropDown countries
  where
    countries =
      [ { countryCode: "FI", countryName: "Finland" }
      , { countryCode: "AX", countryName: "Åland" }
      , { countryCode: "SE", countryName: "Sverige" }
      , { countryCode: "NO", countryName: "Norge" }
      , { countryCode: "DK", countryName: "Danmark" }
      ]

type DropDownCountry =
  { countryCode :: String
  , countryName :: String
  }
