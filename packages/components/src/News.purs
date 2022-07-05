module KSF.News where

import Prelude

import Affjax as AX
import Affjax.ResponseFormat as ResponseFormat
import Data.Argonaut.Decode (decodeJson)
import Data.Argonaut.Decode.Error (printJsonDecodeError)
import Data.Either (Either(..))
import Data.Maybe (Maybe (..))
import Effect (Effect)
import Effect.Aff as Aff
import Effect.Class (liftEffect)
import Effect.Class.Console as Log
import KSF.Driver (getDriver)
import React.Basic (JSX)
import React.Basic.DOM as DOM
import React.Basic.Hooks (useEffectOnce, useState', (/\), UseEffect, UseState)
import React.Basic.Hooks as React

type NewsInput = Maybe (Either Unit (Array News))

type Props =
  { news :: NewsInput
  }

useNews :: forall a. (NewsInput -> Effect Unit) -> React.Render a (UseEffect Unit (UseState NewsInput a)) NewsInput
useNews newsLoaded = React.do
  n /\ setNews <- useState' Nothing
  useEffectOnce $ do
    Aff.launchAff_ $ do
      driver <- liftEffect getDriver
      res <- AX.get driver ResponseFormat.json "https://cdn.ksfmedia.fi/news/mitt-konto.json"
      x <- case res of
        Right x -> case decodeJson $ _.body x of
          Right y -> pure $ Just $ Right y
          Left err -> do
            Log.error $ "News decocde failed " <> printJsonDecodeError err
            pure $ Just $ Left unit
        Left err -> do
          Log.error $ "News load failed " <> AX.printError err
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
    { className: "news--container"
    , children: [ DOM.div { className: "tiny-spinner", children: [] } ]
    }
render (Just (Right xs)) = Just $
  DOM.div
    { className: "news--container"
    , children:
        [ DOM.dl_ $ map renderNewsItem xs ]
    }
  where
    renderNewsItem { date, msg } =
      DOM.tr
        { children:
            [ DOM.dt_ [ DOM.text date ]
            , DOM.dd_ [ DOM.text msg ]
            ]
        }
