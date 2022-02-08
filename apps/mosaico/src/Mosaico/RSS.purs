module Mosaico.RSS where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Data.String (Pattern(..), Replacement(..), replaceAll)
import Effect (Effect)
import KSF.Paper as Paper
import Lettera as Lettera
import Lettera.Models(CategoryLabel, Tag)
import Mosaico.Feed (ArticleFeedType(..))

foreign import delete :: Effect Unit
foreign import inject :: String -> Effect Unit

type Feed =
  { title :: String
  , url :: String
  }

encodeQuotes :: String -> String
encodeQuotes = replaceAll (Pattern "'") (Replacement "\\'")

category :: Paper.Paper -> CategoryLabel -> Feed
category paper cat =
  { title: "RSS-flöde för " <> unwrap cat
  , url: Lettera.letteraBaseUrl <> Lettera.letteraFrontPageUrl
      <> "&paper=" <> Paper.toString paper
      <> "&category=" <> show cat
  }

tag :: Paper.Paper -> Tag -> Feed
tag paper t =
  { title: "RSS-flöde för " <> unwrap t
  , url: Lettera.letteraBaseUrl <> Lettera.letteraTagUrl <> Lettera.encodeURIComponent (unwrap t)
      <> "&paper=" <> Paper.toString paper
  }

string :: Feed -> String
string { title, url } =
  "<link rel='alternate' type='application/rss+xml' title='"
  <> encodeQuotes(title)
  <> "' href='" <> url <> "'/>"

setFeed :: Paper.Paper -> ArticleFeedType -> Effect Unit
setFeed paper feedType = do
  let maybeFeed = case feedType of
        CategoryFeed label -> Just $ category paper label
        TagFeed t -> Just $ tag paper t
        _ -> Nothing
  delete
  case maybeFeed of
    Just feed -> do
      inject $ string feed
    Nothing -> pure unit
