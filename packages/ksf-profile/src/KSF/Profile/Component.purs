module KSF.Profile.Component where

import Prelude

import Data.Array (catMaybes, (:))
import Data.Foldable (fold)
import Data.Maybe (fromMaybe)
import Data.Nullable as Nullable
import Data.String (trim)
import Persona as Persona
import KSF.Profile.View as View
import React.Basic (JSX)
import React.Basic as React

type Props =
  { profile :: Persona.User }

jsComponent :: React.Component Props
jsComponent = component

component :: React.Component Props
component = React.stateless { displayName: "Profile", render }

render :: Props -> JSX
render { profile } =
  View.profile
    { email: profile.email
    , customerNumber: profile.cusno
    , displayName: displayName profile
    , address: fromMaybe [] $ addressArray <$> Nullable.toMaybe profile.address
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
