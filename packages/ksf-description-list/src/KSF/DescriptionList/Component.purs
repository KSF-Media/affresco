module KSF.DescriptionList.Component where

import Prelude

import KSF.DescriptionList.View as View
import React.Basic (JSX)
import React.Basic as React

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
  View.descriptionList { definitions: map handleEmptyDescriptions definitions }

-- | If there are no descriptions, just show a hyphen there.
handleEmptyDescriptions :: Definition -> Definition
handleEmptyDescriptions d@{ descriptions: [] } = d { descriptions = [ "-" ] }
handleEmptyDescriptions d = d
