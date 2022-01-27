module Mosaico.Analytics where

import Prelude

import Effect (Effect)
import Effect.Uncurried (EffectFn1, runEffectFn1)
import Data.Maybe (Maybe(..), fromMaybe)
import Lettera.Models (Article)
import KSF.User.Cusno (toString)
import KSF.User (User)
import Data.Foldable (foldMap)
import Data.Formatter.DateTime (format)
import KSF.Helpers (dateTimeFormatter)
import Data.Array (intercalate)

type StringArticleMetadata = 
  { title :: String
  , publishingTime :: String
  , authors :: String
  , premium :: String
  , listTitle :: String
  , category :: String
  , section :: String
  , articleUuid :: String
  , tags :: String
  , userCusno :: String
  , userSubs :: String
  }

foreign import _pushToDataLayer :: EffectFn1 StringArticleMetadata Unit
pushToDataLayer :: StringArticleMetadata -> Effect Unit
pushToDataLayer = runEffectFn1 _pushToDataLayer

sendArticleAnalytics:: Article -> Maybe User -> Effect Unit
sendArticleAnalytics article user = do
  let metadata = 
          { title: article.title
          , publishingTime: foldMap (\x -> format dateTimeFormatter x) article.publishingTimeUtc
          , authors: intercalate ", " $ _.byline <$> article.authors
          , premium: show article.premium
          , category: fromMaybe "" article.analyticsCategory
          , section: fromMaybe "" article.analyticsSection
          , listTitle: fromMaybe "" article.listTitle
          , articleUuid: (article.uuid :: String)
          , tags: intercalate ", " $ show <$> article.tags
          , userCusno: case user of
            Just u -> toString u.cusno
            Nothing   -> ""
          , userSubs: case user of
            Just u -> intercalate ", " $ _.package.id <$> u.subs
            Nothing -> ""
          }
  pushToDataLayer metadata
