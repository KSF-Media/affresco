module KSF.News where

import Prelude

import Data.Either (Either(..))
import Data.Maybe (Maybe (..))
import Effect (Effect)
import Effect.Aff as Aff
import Effect.Class (liftEffect)
import Effect.Class.Console as Log
import React.Basic (JSX)
import React.Basic.DOM as DOM
import React.Basic.Hooks (useEffectOnce, useState', (/\), UseEffect, UseState)
import React.Basic.Hooks as React
import Simple.Ajax as AX

type NewsInput = Maybe (Either Unit (Array News))

type Props =
  { news :: NewsInput
  }

useNews :: forall a. (NewsInput -> Effect Unit) -> React.Render a (UseEffect Unit (UseState NewsInput a)) NewsInput
useNews newsLoaded = React.do
  n /\ setNews <- useState' Nothing
  useEffectOnce $ do
    Aff.launchAff_ $ do
      res <- AX.get "https://storage.googleapis.com/cdn-ksfmedia-fi/news/mitt-konto.json"
      x <- case res of
        Right x -> pure $ Just $ Right x
        Left err -> do
          Log.error $ "News load failed" <> show err
          pure $ Just $ Left unit
      liftEffect $ do
        setNews x
        newsLoaded x
    pure $ pure unit
  pure n

type News =
  { date :: String
  , msg :: String
  }

render :: Maybe (Either Unit (Array News)) -> Maybe JSX
render (Just (Right [])) = Nothing
render (Just (Left _)) = Nothing
render Nothing = Just $
  DOM.div
    { className: "news--container clearfix"
    , children: [ DOM.div { className: "tiny-spinner", children: [] } ]
    }
render (Just (Right xs)) = Just $
  DOM.div
    { className: "news--container clearfix"
    , children:
        [ DOM.table_ $
            [ DOM.tbody_ $ map renderNewsItem xs ]
        ]
    }
  where
    renderNewsItem { date, msg } =
      DOM.tr
        { children:
            [ DOM.td_ [ DOM.text date ]
            , DOM.td_ [ DOM.text msg ]
            ]
        }
