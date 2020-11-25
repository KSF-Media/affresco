module VetrinaTest.Main where

import Prelude

import Data.Array (mapMaybe)
import Data.Either (Either(..))
import Data.Maybe (Maybe, fromMaybe, maybe)
import Data.Nullable (Nullable, toMaybe)
import Data.Set as Set
import KSF.Paper (Paper)
import KSF.Paper as Paper
import KSF.Vetrina as Vetrina
import React.Basic (JSX)
import React.Basic.Classic as React
import Vetrina.Types (JSProduct, Product, fromJSProduct)

type JSProps =
  { products :: Nullable (Array JSProduct)
  , accessEntitlements :: Nullable (Array String)
  , headline :: Nullable JSX
  , paper :: Nullable String
  }
type Props =
  { products :: Array Product
  , accessEntitlements :: Array String
  , headline :: Maybe JSX
  , paper :: Maybe Paper
  }
type State = { }
type Self = React.Self Props State

fromJSProps :: JSProps -> Props
fromJSProps jsProps =
  { products: maybe [] (mapMaybe fromJSProduct) $ toMaybe jsProps.products
  , accessEntitlements: fromMaybe [] $ toMaybe jsProps.accessEntitlements
  , headline: toMaybe jsProps.headline
  , paper: Paper.fromString =<< toMaybe jsProps.paper
  }

jsComponent :: React.ReactComponent JSProps
jsComponent = React.toReactComponent fromJSProps component { initialState:   {}, render }

component :: React.Component Props
component = React.createComponent "VetrinaTest"

render :: Self -> JSX
render self =
  Vetrina.vetrina
    { onClose: pure unit
    , onLogin: pure unit
    , products: Right self.props.products
    , unexpectedError: mempty
    , accessEntitlements: Set.fromFoldable self.props.accessEntitlements
    , headline: self.props.headline
    , paper: self.props.paper
    }
