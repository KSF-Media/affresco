module KSF.Profile.View where

import Prelude

import KSF.DescriptionList.Component as DescriptionList
import React.Basic.Extended (JSX, Style)
import React.Basic.Extended as React

foreign import profileStyles :: Style

type Attributes =
  { email :: String
  , customerNumber :: String
  , address :: Array String
  , displayName :: String
  }

profile :: Attributes -> JSX
profile { email, customerNumber, address, displayName } =
  React.requireStyle
    profileStyles
    $ React.element
        DescriptionList.component
          { definitions:
            [ { term: "Namn:", descriptions: [ displayName ] }
            , { term: "Adress:", descriptions: address }
            , { term: "E-postadress:", descriptions: [ email ] }
            , { term: "Kundnummer:", descriptions: [ customerNumber ] }
            ]
          }
