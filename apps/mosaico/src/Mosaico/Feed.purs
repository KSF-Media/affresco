module Mosaico.Feed where

import Prelude

import Data.Argonaut.Core (toArray, stringify)
import Data.Argonaut.Encode (encodeJson)
import Data.Argonaut.Parser (jsonParser)
import Data.Array (mapMaybe)
import Data.DateTime (DateTime)
import Data.Either (hush)
import Data.Foldable (foldMap)
import Data.Hashable (class Hashable, hash)
import Data.HashMap (HashMap)
import Data.HashMap as HashMap
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Newtype (unwrap)
import Data.Nullable (Nullable, toMaybe)
import Data.String as String
import Data.Tuple (Tuple(..))
import Lettera.Models (ArticleStub, CategoryLabel(..), Tag(..), encodeStringifyArticleStubs, parseArticleStubWithoutLocalizing)

type FeedSnapshot =
  { stamp :: DateTime
  , feed :: ArticleFeed
  }

type JSInitialFeed =
  { feedType        :: Nullable String
  , feedPage        :: Nullable String
  , feedContent     :: Nullable String
  }

parseFeed :: JSInitialFeed -> HashMap ArticleFeedType ArticleFeed
parseFeed feed = fromMaybe HashMap.empty do
  let feedPage = toMaybe feed.feedPage
  feedType <- do
    f <- toMaybe feed.feedType
    case String.toLower f of
      "categoryfeed" -> CategoryFeed <<< CategoryLabel <$> feedPage
      "tagfeed"      -> map (TagFeed <<< Tag) feedPage
      "searchfeed"   -> SearchFeed <$> feedPage
      _              -> Nothing
  feedContent <- do
    content <- toMaybe feed.feedContent
    list <- content # (jsonParser >>> hush) >>= toArray
    pure $ ArticleList $ mapMaybe (hush <<< parseArticleStubWithoutLocalizing) list

  pure $ HashMap.singleton feedType feedContent

mkArticleFeed :: ArticleFeedType -> ArticleFeed -> Array (Tuple String String)
mkArticleFeed feedDefinition feed =
  foldMap (\feedContent -> [ Tuple "frontpageFeed" $
                             stringify $ encodeJson { feedPage, feedType, feedContent } ]) $
  case feed of
    ArticleList list -> Just $ encodeStringifyArticleStubs list
    -- If we need the front page HTML in a feed after the initial
    -- load, it's just as much network traffic to make the user load
    -- it on demand instead of having it twice in the initial load.
    -- Getting the HTML from the generated DOM's containing innerHTML
    -- would be ideal but it'd be a challenge.
    _ -> Nothing
  where
    feedPage = Just $ case feedDefinition of
      CategoryFeed page -> unwrap page
      TagFeed tag       -> unwrap tag
      SearchFeed query  -> query
    feedType = case feedDefinition of
      CategoryFeed _ -> "categoryfeed"
      TagFeed _      -> "tagfeed"
      SearchFeed _   -> "searchfeed"

data ArticleFeed
  = ArticleList (Array ArticleStub)
  | Html String

data ArticleFeedType
  = CategoryFeed CategoryLabel
  | TagFeed Tag
  | SearchFeed String
derive instance eqArticleFeedType :: Eq ArticleFeedType
instance showArticleFeed :: Show ArticleFeedType where
  show (CategoryFeed c) = "CategoryFeed " <> show c
  show (TagFeed t) = "TagFeed" <> show t
  show (SearchFeed s) = "SearchFeed " <> s
instance hashableArticleFeedType :: Hashable ArticleFeedType where
  hash = hash <<< show
