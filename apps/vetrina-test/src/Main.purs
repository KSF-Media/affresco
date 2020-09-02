module VetrinaTest.Main where

import Prelude

import Data.Array (mapMaybe)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Nullable (Nullable, toMaybe)
import Data.Set as Set
import Effect (Effect)
import Effect.Console (log)
import Effect.Exception (Error)
import KSF.Vetrina as Vetrina
import React.Basic (JSX, make)
import React.Basic as React
import React.Basic.DOM as DOM
import Vetrina.Types (JSProduct, Product, fromJSProduct)

type JSProps = { products :: Nullable (Array JSProduct) }
type Props = { products :: Array Product }
type State = { }
type Self = React.Self Props State

fromJSProps :: JSProps -> Props
fromJSProps jsProps =
  { products: case toMaybe jsProps.products of
       Just jsProducts -> mapMaybe fromJSProduct jsProducts
       Nothing -> []
                        
  }
jsComponent :: React.ReactComponent JSProps
jsComponent = React.toReactComponent fromJSProps component { initialState:   {}, render }

component :: React.Component Props
component = React.createComponent "VetrinaTest"

app :: Props -> JSX
app = make component
  { initialState: {}
  , render
  }

render :: Self -> JSX
render self =
  Vetrina.vetrina
    { onClose: pure unit
    , onLogin: pure unit
    , products: Right self.props.products
    , unexpectedError: mempty
    , accessEntitlements: Set.empty
    } 
