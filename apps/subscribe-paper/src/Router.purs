module Router where

import Data.Function.Uncurried (Fn0, Fn1, runFn0, runFn1)
import Data.Maybe (Maybe)
import Data.Nullable (Nullable)
import Foreign (Foreign)
import React.Basic (Component, JSX, ReactComponent)
import React.Basic as React

foreign import route_ :: forall p. Fn0 (ReactComponent (RouteProps p))
foreign import switch_ :: Fn0 (ReactComponent SwitchProps)

type Match =
  { params :: Foreign
  , isExact :: Boolean
  , path :: String
  , url :: String
  }
type SwitchProps = { children :: Array JSX }
type RouteProps props = { exact :: Boolean, path :: Nullable String, component :: ReactComponent props }

route :: forall props. ReactComponent (RouteProps props)
route = runFn0 route_

switch :: ReactComponent SwitchProps
switch = runFn0 switch_
