module Mosaico.Ad where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Monoid (guard)
import Data.Nullable (Nullable, toMaybe)
import Data.String (joinWith)
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
foreign import getIsLazy :: EffectFn1 String (Nullable Boolean)
foreign import showConsentRevocationMessage :: forall a. EffectFn1 a Unit

component :: React.Component Props
component = React.createComponent "ad"

type Self =
  { state :: State
  , setState :: (State -> State) -> Effect Unit
  , props :: Props
  }

type Props =
  { contentUnit :: String
  , inBody :: Boolean
  }

type State =
  { gamId :: Maybe String
  , isLazy :: Maybe Boolean
  }

ad :: Props -> JSX
ad = make component
  { initialState: { gamId: Nothing, isLazy: Nothing }
  , didMount: \self -> do
      runEffectFn1 fetchAdImpl self.props.contentUnit
      gamId <- runEffectFn1 getGamId self.props.contentUnit
      isLazy <- runEffectFn1 getIsLazy self.props.contentUnit
      self.setState _ { gamId = toMaybe gamId, isLazy = toMaybe isLazy }
  , render: render
  }
    where
      blockClass = "mosaico-ad"
      networkCode = "21664538223"
      render self@{ state: { gamId: Just gamId, isLazy: Just isLazy }} =
        let inBodyClass = if self.props.inBody then "in-body" else mempty
            contentUnitClass = toLowerCase self.props.contentUnit
        in DOM.div
          { className: joinWith " " [blockClass, contentUnitClass]
          , children:
            [ DOM.div
                { id: self.props.contentUnit
                , className: joinWith " " [blockClass <> "__content-unit", inBodyClass]
                , _data: Object.fromFoldable [Tuple ((guard isLazy $ "lazy-") <> "ad-unit-id") $ "/" <> networkCode <> "/" <> gamId <> "/" <> "hbl"] 
                }
            ]
          }
      render _ = mempty

openConsentRevocationMessage :: forall a. a -> Effect Unit
openConsentRevocationMessage = runEffectFn1 showConsentRevocationMessage