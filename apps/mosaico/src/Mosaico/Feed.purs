module Mosaico.Feed where

import Prelude

import Data.Argonaut.Core (Json, fromArray, stringify)
import Data.Argonaut.Decode (decodeJson)
import Data.Argonaut.Encode (encodeJson)
import Data.Array (mapMaybe)
import Data.DateTime (DateTime)
import Data.Either (hush)
import Data.Hashable (class Hashable, hash)
import Data.HashMap (HashMap)
import Data.HashMap as HashMap
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Newtype (unwrap)
import Data.Nullable (Nullable, toMaybe)
import Data.String as String
import Data.Tuple (Tuple(..))
import Foreign.Object as Object
import Lettera.Models (ArticleStub, CategoryLabel(..), Tag(..), articleStubToJson, parseArticleStubWithoutLocalizing)

type FeedSnapshot =
  { stamp :: DateTime
  , feed :: ArticleFeed
  }

type JSInitialFeed =
  { feedType        :: Nullable String
  , feedPage        :: Nullable String
  , feedContent     :: Nullable Json
  , feedContentType :: Nullable String
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
    contentType <- toMaybe feed.feedContentType
    case contentType of
      "articleList" -> do
        list <- hush $ decodeJson content
        pure $ ArticleList $ mapMaybe (hush <<< parseArticleStubWithoutLocalizing) list
      "html" -> do
        obj <- hush $ decodeJson content
        list <- hush <<< decodeJson =<< Object.lookup "list" obj
        Html
          <$> (hush <<< decodeJson =<< Object.lookup "html" obj)
          <*> (pure $ mapMaybe (hush <<< parseArticleStubWithoutLocalizing) list)
      _ ->
        Nothing
  pure $ HashMap.singleton feedType feedContent

mkArticleFeed :: ArticleFeedType -> ArticleFeed -> Array (Tuple String String)
mkArticleFeed feedDefinition feed =
  [ Tuple "frontpageFeed" $ stringify $ encodeJson { feedPage, feedType, feedContent, feedContentType } ]
  where
    fromArticles = fromArray <<< map articleStubToJson
    (Tuple feedContentType feedContent) = case feed of
      ArticleList list -> Tuple "articleList" $ fromArticles list
      -- The prerendered HTML is rather large (at some 150kB) so it
      -- would be nice to have a way to not have it second time in a
      -- variable, but there's just no easy way around it.  React
      -- doesn't really have the option of just using the existing
      -- HTML on the page.  Hydrate can take over an existing DOM tree
      -- but it still expects to know how to render the same tree from
      -- its internal state.  Getting the HTML from the generated
      -- DOM's containing innerHTML would be ideal but it'd be a
      -- challenge.
      Html html list -> Tuple "html" $ encodeJson {html, list: fromArticles list}
    (Tuple feedType feedPage) = case feedDefinition of
      CategoryFeed page -> Tuple "categoryfeed" $ unwrap page
      TagFeed tag       -> Tuple "tagfeed" $ unwrap tag
      SearchFeed query  -> Tuple "searchfeed" query

data ArticleFeed
  = ArticleList (Array ArticleStub)
  | Html String (Array ArticleStub)

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
