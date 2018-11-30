module KSF.Footer.Component where

import KSF.Footer.View as View
import React.Basic (JSX, createComponent, makeStateless)
import React.Basic as React

type Props = {}

jsComponent :: React.Component Props
jsComponent = component

component :: React.Component Props
component = createComponent "Footer"

footer :: Props -> JSX
footer = makeStateless component \_ -> View.footer

render :: Props -> JSX
render _ = View.footer
