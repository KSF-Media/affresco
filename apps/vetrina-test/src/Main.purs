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
import KSF.Vetrina as Vetrina
import React.Basic (JSX)
import React.Basic.Classic as React
import Vetrina.Types (JSProduct, Product, fromJSProduct)

type JSProps =
  { products :: Nullable (Array JSProduct)
  , accessEntitlements :: Nullable (Array String)
  , headline :: Nullable JSX
  , paper :: Nullable String
  , paymentMethods :: Nullable (Array String)
  , minimalLayout :: Nullable Boolean
  }
type Props =
  { products :: Array Product
  , accessEntitlements :: Array String
  , headline :: Maybe JSX
  , paper :: Maybe Paper
  , paymentMethods :: Array PaymentMethod
  , minimalLayout :: Boolean
  }
type State = { }
type Self = React.Self Props State

fromJSProps :: JSProps -> Props
fromJSProps jsProps =
  { products: maybe [] (mapMaybe fromJSProduct) $ toMaybe jsProps.products
  , accessEntitlements: fromMaybe [] $ toMaybe jsProps.accessEntitlements
  , headline: toMaybe jsProps.headline
  , paper: Paper.fromString =<< toMaybe jsProps.paper
  , paymentMethods: foldMap (mapMaybe toPaymentMethod) $ toMaybe jsProps.paymentMethods
  , minimalLayout: fromMaybe false $ toMaybe jsProps.minimalLayout
  }

jsComponent :: React.ReactComponent JSProps
jsComponent = React.toReactComponent fromJSProps component { initialState: {}, render }

component :: React.Component Props
component = React.createComponent "VetrinaTest"

render :: Self -> JSX
render self =
  Vetrina.vetrina
    { onClose: Just $ pure unit
    , onLogin: pure unit
    , products: Right self.props.products
    , unexpectedError: mempty
    , accessEntitlements: Set.fromFoldable self.props.accessEntitlements
    , headline: self.props.headline
    , paper: self.props.paper
    , paymentMethods: self.props.paymentMethods
    , minimalLayout: self.props.minimalLayout
    }
