module Mosaico.Analytics where

import Prelude

import Effect (Effect)
import Effect.Uncurried (EffectFn1, runEffectFn1)
import Data.Maybe (Maybe(..), fromMaybe)
import Lettera.Models (Article, Author, Tag)
import Data.DateTime (DateTime)
import KSF.User.Cusno (toString, Cusno)
import KSF.User (User)
import KSF.Api.Subscription (Subscription)
import Data.Foldable (foldMap)
import Data.Formatter.DateTime (format)
import KSF.Helpers (dateTimeFormatter)
import Data.Array (intercalate)


type ArticleMetadata = 
  { title :: String
  , publishingTime :: Maybe DateTime
  , authors :: Array Author
  , premium :: Boolean
  , listTitle :: Maybe String
  , category :: Maybe String
  , section :: Maybe String
  , articleUuid :: String
  , tags :: Array Tag
  , userCusno :: Maybe Cusno
  , userSubs :: Maybe (Array Subscription)
  }

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

-- stringifyMetadata :: ArticleMetadata -> StringArticleMetadata
-- stringifyMetadata metadata = 
--   { title : metadata.title
--   , publishingTime: foldMap (\x -> format dateTimeFormatter x) metadata.publishingTime
--   , premium: show metadata.premium
--   , category: fromMaybe "" metadata.category
--   , section: fromMaybe "" metadata.section
--   , listTitle: fromMaybe "" metadata.listTitle
--   , articleUuid: (metadata.articleUuid :: String)
--   , tags: intercalate ", " $ show <$> metadata.tags
--   , authors: intercalate ", " $ _.byline <$> metadata.authors
--   , userCusno: case metadata.userCusno of
--     Just c       -> toString c
--     Nothing      -> ""
--   , userSubs: case metadata.userSubs of
--     Just subs    -> intercalate ", " $ _.package.id <$> subs
--     Nothing      -> ""
--   }

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
  pure unit