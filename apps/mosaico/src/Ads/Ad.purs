module Mosaico.Ad where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Nullable (Nullable, toMaybe)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Uncurried (EffectFn1, runEffectFn1)
import Foreign.Object as Object
import Payload.Internal.Utils (toLowerCase)
import React.Basic (JSX)
import React.Basic.Classic (make)
import React.Basic.Classic as React
import React.Basic.DOM as DOM

foreign import fetchAdImpl :: EffectFn1 String Unit
foreign import getGamId :: EffectFn1 String (Nullable String)

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
  , gamId :: Maybe String
  }

ad :: Props -> JSX
ad = make component
  { initialState: { populated: false, gamId: Nothing }
  , didMount: \self -> do
      runEffectFn1 fetchAdImpl self.props.contentUnit
      gamId <- runEffectFn1 getGamId self.props.contentUnit
      self.setState _ { gamId = toMaybe gamId }
  , render: render
  }
    where
      blockClass = "mosaico-ad"
      networkCode = "21664538223"
      render { state: { gamId: Nothing }} = mempty
      render self@{ state: { gamId: Just gamId }} = 
        DOM.div
          { className: blockClass <> " " <> toLowerCase self.props.contentUnit
          , children:
            [ DOM.div
                { id: self.props.contentUnit
                , className: blockClass <> "__content-unit"
                , _data: Object.fromFoldable [Tuple "ad-unit-id" $ "/" <> networkCode <> "/" <> gamId <> "/" <> "hbl"] 
                }
            ]
          }
