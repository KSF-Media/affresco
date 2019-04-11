module KSF.Profile.Component where

import Prelude

import Data.Array (catMaybes, (:))
import Data.Foldable (fold)
import Data.Maybe (fromMaybe)
import Data.Nullable as Nullable
import Data.String (trim)
import KSF.Profile.View as View
import Persona as Persona
import React.Basic (JSX, make)
import React.Basic as React

type Self = React.Self Props {}
type Props =
  { profile :: Persona.User }

jsComponent :: React.Component Props
jsComponent = component

component :: React.Component Props
component = React.createComponent "Profile"

profile :: Props -> JSX
profile = make component
  { initialState: {}
  , render
  }

render :: Self -> JSX
render { props } =
  View.profile
    { email: props.profile.email
    , customerNumber: props.profile.cusno
    , displayName: displayName props.profile
    , address: fromMaybe [] $ addressArray <$> Nullable.toMaybe props.profile.address
    }

addressArray :: Persona.Address -> Array String
addressArray { streetAddress, zipCode, city } = do
  let takeJust = catMaybes <<< map Nullable.toMaybe
  streetAddress : takeJust [ zipCode, city ]

-- FIXME: Persona should provide that, so we don't have to
-- concatenate strings
displayName :: Persona.User -> String
displayName { lastName, firstName } =
  trim $ fold
    [ fromMaybe "" $ Nullable.toMaybe lastName
    , " "
    , fromMaybe "" $ Nullable.toMaybe firstName
    ]
