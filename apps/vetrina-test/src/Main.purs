module VetrinaTest.Main where

import Prelude

import Bottega.Models.PaymentMethod (toPaymentMethod)
import Data.Array (mapMaybe)
import Data.Either (Either(..))
import Data.Foldable (foldMap)
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Nullable (Nullable, toMaybe)
import Data.Set as Set
import KSF.Paper (Paper)
import KSF.Paper as Paper
import KSF.User (PaymentMethod)
import KSF.Vetrina (Props, JSProps)
import KSF.Vetrina as Vetrina
import React.Basic (JSX)
import React.Basic.Classic as React
import Vetrina.Types (JSProduct, Product, fromJSProduct)

type State = { }
type Self = React.Self Props State

jsComponent :: React.ReactComponent JSProps
jsComponent = React.toReactComponent Vetrina.fromJSProps component { initialState: {}, render }

component :: React.Component Props
component = React.createComponent "VetrinaTest"

render :: Self -> JSX
render self =
  Vetrina.vetrina self.props
