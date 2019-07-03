module KSF.Navbar.Collapsed.Component where

import Prelude

import React.Basic (JSX, make)
import React.Basic as React
import React.Basic.DOM as DOM

data Visibility = Visible | Hidden

type Self = React.Self Props {}

type Props =
  { visibility :: Visibility
  , navItems :: Array JSX
  }

component :: React.Component Props
component = React.createComponent "CollapsedNav"

collapsed :: Props -> JSX
collapsed = make component
  { initialState: {}
  , render
  }

render :: Self -> JSX
render { props } =
  DOM.div
    { className: "collapsed-nav--content clearfix"
    , style: DOM.css
      if isHidden props.visibility
      then { "transform": "translateY(0)" }
      else { "transform": "translateY(100%)" }
    , children: map collapsedNavRow props.navItems
    }

collapsedNavRow :: JSX -> JSX
collapsedNavRow menuItem =
  DOM.div
    { className: "collapsed-nav--menu-row flex items-center ml2 pb1"
    , children: [ menuItem ]
    }

isHidden :: Visibility -> Boolean
isHidden Hidden = true
isHidden _ = false

negateVisibility :: Visibility -> Visibility
negateVisibility Hidden  = Visible
negateVisibility Visible = Hidden
