module Mosaico.Models where

import Prelude

import Data.Hashable (class Hashable, hash)
import Data.Maybe (Maybe)
import Lettera.Models (ArticleStub, Category, CategoryLabel (..), FullArticle(..), Tag (..))

data ArticleFeed
  = ArticleList (Array ArticleStub)
  | Html String

data ArticleFeedType
  = CategoryFeed (Maybe CategoryLabel) -- `Nothing` represents root
  | TagFeed Tag
  | SearchFeed String
derive instance eqArticleFeedType :: Eq ArticleFeedType
instance showArticleFeed :: Show ArticleFeedType where
  show (CategoryFeed c) = "CategoryFeed " <> show c
  show (TagFeed t) = "TagFeed" <> show t
  show (SearchFeed s) = "SearchFeed " <> s
instance hashableArticleFeedType :: Hashable ArticleFeedType where
  hash = hash <<< show