module KSF.Navbar.Collapsed.Component where

import Prelude

import React.Basic (JSX)
import React.Basic.DOM as DOM
import React.Basic.Hooks (Component)
import React.Basic.Hooks as React

data Visibility = Visible | Hidden

type Props =
  { visibility :: Visibility
  , navItems :: Array JSX
  }

component :: Component Props
component = React.component "Collapsed" $ pure <<< render

render :: Props -> JSX
render props =
  DOM.div
    { className: "collapsed-nav--container"
    , style: DOM.css
      if isHidden props.visibility
      then { "transform": "translateY(0)" }
      else { "transform": "translateY(100%)" }
    , children: map collapsedNavRow props.navItems
    }

collapsedNavRow :: JSX -> JSX
collapsedNavRow menuItem =
  DOM.div
    { className: "collapsed-nav--item"
    , children: [ menuItem ]
    }

isHidden :: Visibility -> Boolean
isHidden Hidden = true
isHidden _ = false

negateVisibility :: Visibility -> Visibility
negateVisibility Hidden  = Visible
negateVisibility Visible = Hidden
