module KSF.DescriptionList.Component where

import Prelude

import Data.Array (foldl, (:))
import Data.Array as Array
import React.Basic (JSX, make)
import React.Basic as React
import React.Basic.DOM as DOM

type Self = React.Self Props {}

type Props =
  { definitions :: Array Definition }

type Definition =
  { term :: String
  , description :: Array JSX
  }

component :: React.Component Props
component = React.createComponent "DescriptionList"

descriptionList :: Props -> JSX
descriptionList = make component { initialState: {}, render }

render :: Self -> JSX
render { props: { definitions } } =
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
