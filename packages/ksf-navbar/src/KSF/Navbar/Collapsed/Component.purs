module KSF.Navbar.Collapsed.Component where

import Data.Void (Void)
import KSF.Navbar.Collapsed.View as View
import React.Basic (JSX, make)
import React.Basic as React

data Visibility = Visible | Hidden

type Self = React.Self Props {} Void

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
  View.collapsed
    { isHidden: isHidden props.visibility
    , navItems: props.navItems
    }

isHidden :: Visibility -> Boolean
isHidden Hidden = true
isHidden _ = false

negateVisibility :: Visibility -> Visibility
negateVisibility Hidden  = Visible
negateVisibility Visible = Hidden
