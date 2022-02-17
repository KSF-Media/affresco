module Mosaico.Webview where

import Prelude

import Control.Monad.Rec.Class (untilJust)
import Data.Array (last)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String (split)
import Data.String.Pattern (Pattern(..))
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Aff as Aff
import Effect.AVar as AVar
import Effect.Aff.AVar as AffAVar
import Effect.Class (liftEffect)
import Lettera.Models (Category(..))
import KSF.Random (randomString)
import Foreign.Object as Object
import React.Basic (JSX)
import React.Basic.DOM as DOM
import React.Basic.Hooks as React
import React.Basic.Hooks (Component, component, useEffectOnce, useState', (/\))

type Props =
  { category :: Category
  }

data StreamType = JPG | M3U8

webviewComponent :: Component Props
webviewComponent = do
  initialRandom <- randomString 10
  let streamURL "https://cdn.ksfmedia.fi/video/vn.torg.html" =
        "http://jpg1.stream.sydweb.fi/radhustorget/lastsnap.jpg"
      streamURL "https://cdn.ksfmedia.fi/video/on.torg.html" =
        "https://torget.ostnyland.fi/stream.m3u8"
      streamURL url = url
      initialize (Just JPG) setRandom = do
        (closed :: AVar.AVar Unit) <- (AVar.empty :: Effect (AVar.AVar Unit))
        Aff.launchAff_ do
          _ <- untilJust do
            Aff.delay $ Aff.Milliseconds 500.0
            liftEffect $ setRandom =<< randomString 10
            AffAVar.tryRead closed
          pure unit
        pure $ AVar.tryPut unit closed *> pure unit
      initialize _ _ = pure $ pure unit

  component "Webview" $ \ {category: (Category cat)} -> React.do
    let url = streamURL $ fromMaybe "" cat.url
        streamType = case last $ split (Pattern ".") url of
          Just "jpg" -> Just JPG
          Just "m3u8" -> Just M3U8
          _ -> Nothing
    random /\ setRandom <- useState' initialRandom
    useEffectOnce $ initialize streamType setRandom
    pure $ render streamType url random

render :: Maybe StreamType -> String -> String -> JSX
render (Just JPG) url random =
  DOM.div
    { className: "mosaico-webview"
    , children:
        [ DOM.img
            { src: url <> "?rand=" <> random
            }
        ]
    }
render (Just M3U8) url _ =
  DOM.div
    { className: "mosaico-webview"
    , children:
        [ DOM.video
            { className: "video-js"
            , controls: true
            , preload: "auto"
            , width: "1140"
            , height: "700"
            , _data: Object.fromFoldable [ Tuple "setup" "{}" ]
            , children:
                [ DOM.source
                    { src: url
                    , type: "application/x-mpegURL"
                    }
                ]
            }
        ]
    }
render _ _ _ = mempty
