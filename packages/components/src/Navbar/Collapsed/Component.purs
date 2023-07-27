module KSF.Navbar.Collapsed.Component where

import Prelude

import React.Basic (JSX)
import React.Basic.DOM as DOM

data Visibility = Visible | Hidden

type Props =
  { visibility :: Visibility
  , navItems :: Array JSX
  }

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
