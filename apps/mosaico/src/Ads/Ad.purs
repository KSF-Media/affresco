module Mosaico.Ad where

import Prelude

import Effect (Effect)
import Effect.Uncurried (EffectFn1, runEffectFn1)
import Payload.Internal.Utils (toLowerCase)
import React.Basic (JSX)
import React.Basic.Classic (make)
import React.Basic.Classic as React
import React.Basic.DOM as DOM

foreign import fetchAd :: EffectFn1 String Unit

component :: React.Component Props
component = React.createComponent "ad"

type Self =
  { state :: State
  , setState :: (State -> State) -> Effect Unit
  , props :: Props
  }

type Props =
  { contentUnit :: String
  }

type State =
  { populated :: Boolean
  }

ad :: Props -> JSX
ad = make component
  { initialState: { populated: false }
  , didMount: \self -> do
      runEffectFn1 fetchAd self.props.contentUnit

  , render: \self ->
      DOM.div
        { className: blockClass <> " " <> toLowerCase self.props.contentUnit
        , children:
          [ DOM.div
              { id: self.props.contentUnit
              , className: blockClass <> "__content-unit"
              }
          ]
        }
      }
      where
        blockClass = "mosaico-ad"

