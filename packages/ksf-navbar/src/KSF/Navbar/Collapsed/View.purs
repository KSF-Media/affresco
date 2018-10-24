module KSF.Navbar.Collapsed.View where

import Prelude

import React.Basic.DOM as DOM
import React.Basic.Extended (JSX, Style)
import React.Basic.Extended as React

foreign import collapsedNavbarStyles :: Style

type Attributes =
  { isHidden :: Boolean
  , navItems :: Array JSX
  }

collapsed :: Attributes -> JSX
collapsed { isHidden, navItems } =
  React.requireStyle
    collapsedNavbarStyles
    $ DOM.div
        { className: "collapsed-nav--content clearfix"
        , style: DOM.css
          if isHidden
          then { "transform": "translateY(0)" }
          else { "transform": "translateY(100%)" }
        , children: map collapsedNavRow navItems
        }

collapsedNavRow :: JSX -> JSX
collapsedNavRow menuItem =
  DOM.div
    { className: "collapsed-nav--menu-row flex items-center ml2 pb1"
    , children: [ menuItem ]
    }
