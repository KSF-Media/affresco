module Router where

import React.Basic as React

foreign import route_ :: forall a. Array Route -> React.Component a

type Route =
  forall props.
  { path :: String
  , component :: React.Component { | props }
  }

route :: forall props. Array Route -> React.Component props
route routes =
  route_ routes
