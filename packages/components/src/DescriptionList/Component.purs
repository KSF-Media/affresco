module KSF.DescriptionList.Component where

import Prelude

import Data.Array (foldl, (:))
import Data.Array as Array
import React.Basic (JSX)
import React.Basic.DOM as DOM

type Props =
  { definitions :: Array Definition }

type Definition =
  { term :: String
  , description :: Array JSX
  }

render :: Props -> JSX
render { definitions } =
  DOM.dl
    { className: "description-list--container"
    , children: foldl mkDescriptionList [] $ map handleEmptyDescriptions definitions
    }
  where
    mkDescriptionList :: Array JSX -> Definition -> Array JSX
    mkDescriptionList dList d = dList <> mkDescription d

    mkDescription :: Definition -> Array JSX
    mkDescription { term, description } =
      DOM.dt_ [ DOM.text term ] : map mkStaticDescription description
      where
        mkStaticDescription = DOM.dd_ <<< Array.singleton

-- | If there are no descriptions, just show a hyphen there.
handleEmptyDescriptions :: Definition -> Definition
handleEmptyDescriptions d@{ description: [] } = d { description = [ DOM.text "-" ] }
handleEmptyDescriptions d = d
