module KSF.DescriptionList.Component where

import Prelude

import React.Basic.Extended (JSX)
import React.Basic.Extended as React
import Data.Array (foldl, (:))
import Data.Array as Array
import React.Basic.DOM as DOM

type Props =
  { definitions :: Array Definition }

type Definition =
  { term :: String
  , descriptions :: Array String
  }

component :: React.Component Props
component = React.stateless { displayName: "DescriptionList", render }

render :: Props -> JSX
render { definitions } =
  DOM.dl_ $ foldl mkDescriptionList [] $ map handleEmptyDescriptions definitions
  where
    mkDescriptionList :: Array JSX -> Definition -> Array JSX
    mkDescriptionList dList d = dList <> mkDescription d

    mkDescription :: Definition -> Array JSX
    mkDescription { term, descriptions } =
      DOM.dt_ [ DOM.text term ] : map description descriptions
      where
        description = DOM.dd_ <<< Array.singleton <<< DOM.text

-- | If there are no descriptions, just show a hyphen there.
handleEmptyDescriptions :: Definition -> Definition
handleEmptyDescriptions d@{ descriptions: [] } = d { descriptions = [ "-" ] }
handleEmptyDescriptions d = d
