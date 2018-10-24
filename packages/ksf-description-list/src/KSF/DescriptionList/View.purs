module KSF.DescriptionList.View where

import Prelude

import Data.Array (foldl, (:))
import Data.Array as Array
import React.Basic (JSX)
import React.Basic.DOM as DOM

type Definition =
  { term :: String
  , descriptions :: Array String
  }

type Attributes =
  { definitions :: Array Definition }

descriptionList :: Attributes -> JSX
descriptionList attrs =
  DOM.dl_ $ foldl mkDescriptionList [] attrs.definitions

mkDescriptionList :: Array JSX -> Definition -> Array JSX
mkDescriptionList dList d = dList <> mkDescription d

mkDescription :: Definition -> Array JSX
mkDescription { term, descriptions } =
  DOM.dt_ [ DOM.text term ]
  : map description descriptions
  where
    description = DOM.dd_ <<< Array.singleton <<< DOM.text
