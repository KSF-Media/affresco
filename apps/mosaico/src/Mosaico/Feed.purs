module Mosaico.Feed where

import Prelude

import Data.Argonaut.Core (Json, fromArray)
import Data.Argonaut.Decode (decodeJson)
import Data.Argonaut.Encode (encodeJson)
import Data.Array (mapMaybe)
import Data.Either (hush)
import Data.Hashable (class Hashable, hash)
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Data.Nullable (Nullable, toMaybe)
import Data.String as String
import Data.Tuple (Tuple(..))
import Foreign.Object as Object
import Lettera.Models (ArticleStub, CategoryLabel(..), Tag(..), articleStubToJson, parseArticleStubWithoutLocalizing)

type JSInitialFeed =
  { feedType        :: Nullable String
  , feedPage        :: Nullable String
  , feedContent     :: Nullable Json
  , feedContentType :: Nullable String
  }

parseFeed :: JSInitialFeed -> Maybe (Tuple ArticleFeedType ArticleFeed)
parseFeed feed = do
  let feedPage = toMaybe feed.feedPage
  feedType <- do
    f <- toMaybe feed.feedType
    case String.toLower f of
      "categoryfeed" -> CategoryFeed <<< CategoryLabel <$> feedPage
      "tagfeed"      -> map (TagFeed <<< Tag) feedPage
      "searchfeed"   -> SearchFeed <$> feedPage
      "latestfeed"   -> Just LatestFeed
      "mostreadfeed" -> Just MostReadFeed
      "breakingnews" -> Just BreakingNewsFeed
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
          <$> (pure $ mapMaybe (hush <<< parseArticleStubWithoutLocalizing) list)
          <*> (hush <<< decodeJson =<< Object.lookup "html" obj)
      _ ->
        Nothing
  pure $ Tuple feedType feedContent

mkArticleFeed :: ArticleFeedType -> ArticleFeed -> Array (Tuple String Json)
mkArticleFeed feedDefinition feed =
  [ Tuple "frontpageFeed" $ encodeJson { feedPage, feedType, feedContent, feedContentType } ]
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
      Html list html -> Tuple "html" $ encodeJson {html, list: fromArticles list}
    (Tuple feedType feedPage) = case feedDefinition of
      CategoryFeed page -> Tuple "categoryfeed" $ unwrap page
      TagFeed tag       -> Tuple "tagfeed" $ unwrap tag
      SearchFeed query  -> Tuple "searchfeed" query
      LatestFeed        -> Tuple "latestfeed" ""
      MostReadFeed      -> Tuple "mostreadfeed" ""
      BreakingNewsFeed  -> Tuple "breakingnews" ""

data ArticleFeed
  = ArticleList (Array ArticleStub)
  | Html (Array ArticleStub) String

data ArticleFeedType
  = CategoryFeed CategoryLabel
  | TagFeed Tag
  | SearchFeed String
  | LatestFeed
  | MostReadFeed
  | BreakingNewsFeed
derive instance eqArticleFeedType :: Eq ArticleFeedType
derive instance ordArticleFeedType :: Ord ArticleFeedType
instance showArticleFeed :: Show ArticleFeedType where
  show (CategoryFeed c) = "CategoryFeed " <> show c
  show (TagFeed t) = "TagFeed" <> show t
  show (SearchFeed s) = "SearchFeed " <> s
  show LatestFeed = "LatestFeed"
  show MostReadFeed = "MostReadFeed"
  show BreakingNewsFeed = "BreakingNewsFeed"
instance hashableArticleFeedType :: Hashable ArticleFeedType where
  hash = hash <<< show

toList :: ArticleFeed -> Array ArticleStub
toList (ArticleList list) = list
toList _ = []

toHtml :: ArticleFeed -> String
toHtml (Html _ html) = html
toHtml _ = ""
