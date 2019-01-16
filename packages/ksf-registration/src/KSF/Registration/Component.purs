module KSF.Registration.Component where

import Prelude

import Data.Maybe (Maybe(..))
import KSF.Registration.View as View
import React.Basic (JSX, make)
import React.Basic as React
import React.Basic.DOM as DOM

type Self = React.Self Props State Void
type Props = {}

type JSProps = {}

type State =
  { firstName :: Maybe String
  , lastName :: Maybe String
  , streetAddress :: Maybe String
  , city :: Maybe String
  , country :: Maybe String
  , phone :: Maybe String
  , emailAddress :: Maybe String
  , password :: Maybe String
  }

jsComponent :: React.ReactComponent JSProps
jsComponent = React.toReactComponent fromJSProps component { initialState, render }

registration :: Props -> JSX
registration = make component { initialState, render }

render :: Self -> JSX
render self = View.registration

initialState :: State
initialState =
  { firstName: Nothing
  , lastName: Nothing
  , streetAddress: Nothing
  , city: Nothing
  , country: Nothing
  , phone: Nothing
  , emailAddress: Nothing
  , password: Nothing
  }

fromJSProps :: JSProps -> Props
fromJSProps _ = {}

component :: React.Component Props
component = React.createComponent "Registration"
