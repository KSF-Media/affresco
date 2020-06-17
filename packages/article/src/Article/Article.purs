module Article where

import Prelude

import React.Basic (JSX)
import React.Basic as React
import React.Basic.DOM as DOM

type Self = React.Self Props State

type Props =
  { article :: Article }

type State = {}

type Article = { title :: String }

component :: React.Component Props
component = React.createComponent "Article"

article :: Props -> JSX
article = React.make component
 { initialState: {}, render }

render :: Self -> JSX
render self =
  DOM.div
    { className: ""
    , children: [ DOM.text self.props.article.title ]
    }
