module KSF.Navbar.Collapsed.Component where

import React.Basic (JSX)
import React.Basic as React
import KSF.Navbar.Collapsed.View as View

data Visibility = Visible | Hidden

type Props =
  { visibility :: Visibility
  , navItems :: Array JSX
  }

component :: React.Component Props
component = React.stateless { displayName: "CollapsedNav", render }

render :: Props -> JSX
render props =
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
