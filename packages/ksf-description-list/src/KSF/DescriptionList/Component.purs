module KSF.DescriptionList.Component where

import Prelude

import React.Basic.Extended (JSX)
import React.Basic.Extended as React
import Data.Array (foldl, (:))
import Data.Array as Array
import React.Basic.DOM as DOM

type Props =
  { definitions :: Array Definition }

data Description
  = Static (Array String)
  | Editable JSX

type Definition =
  { term :: String
  , description :: Description
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
    mkDescription { term, description } =
      DOM.dt_ [ DOM.text term ] : case description of
        Static descs -> map mkStaticDescription descs
        Editable jsx -> [jsx]
      where
        mkStaticDescription = DOM.dd_ <<< Array.singleton <<< DOM.text

-- | If there are no descriptions, just show a hyphen there.
handleEmptyDescriptions :: Definition -> Definition
handleEmptyDescriptions d@{ description: Static [] } = d { description = Static [ "-" ] }
handleEmptyDescriptions d = d
