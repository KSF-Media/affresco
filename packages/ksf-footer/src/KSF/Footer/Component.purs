module KSF.Footer.Component where

import KSF.Footer.View as View
import React.Basic (JSX)
import React.Basic as React

type Props = {}

jsComponent :: React.Component Props
jsComponent = component

component :: React.Component Props
component = React.stateless { displayName: "footer", render }

render :: Props -> JSX
render _ = View.footer
