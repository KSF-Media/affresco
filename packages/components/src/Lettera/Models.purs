module Lettera.Models where

import Prelude

import Data.Argonaut.Core (Json, stringify)
import Data.Array (fromFoldable)
import Data.DateTime (DateTime, adjust)
import Data.Either (Either(..), hush)
import Data.Formatter.DateTime (unformat)
import Data.Generic.Rep (class Generic)
import Data.JSDate as JSDate
import Data.Maybe (Maybe(..), maybe)
import Data.Show.Generic (genericShow)
import Data.String (joinWith)
import Data.Time.Duration as Duration
import Effect (Effect)
import Effect.Class.Console as Console
import Foreign (renderForeignError)
import KSF.Helpers (dateTimeFormatter)
import Simple.JSON (class ReadForeign)
import Simple.JSON as JSON

data FullArticle
  = FullArticle Article
  | PreviewArticle Article

fromFullArticle :: FullArticle -> Article
fromFullArticle (FullArticle a) = a
fromFullArticle (PreviewArticle a) = a

isPreviewArticle :: FullArticle -> Boolean
isPreviewArticle (PreviewArticle _) = true
isPreviewArticle _ = false

newtype LocalDateTime = LocalDateTime DateTime

localizeArticleDateTimeString :: String -> String -> Effect (Maybe LocalDateTime)
localizeArticleDateTimeString uuid dateTimeString =
  case parseDateTime dateTimeString of
    Just d -> fromUTCTime d
    Nothing -> do
      Console.warn $ "Could not parse timestamp for article " <> uuid
      pure Nothing

-- | As the article timestamps we get from Lettera are in UTC timezone,
--   we need to add the timezone offset to them before showing them to the reader.
--   We use the type `LocaDateTime` here to make it more obvious it's not a "regular"
--   `DateTime`.
fromUTCTime :: DateTime -> Effect (Maybe LocalDateTime)
fromUTCTime utcTime = do
  let jsDate = JSDate.fromDateTime utcTime
  offset <- (_ * -1.0) <$> JSDate.getTimezoneOffset jsDate
  pure $ (LocalDateTime <$> adjust (Duration.Minutes offset) utcTime)

type ArticleStubCommon =
  ( title     :: String
  , uuid      :: String
  , preamble  :: String
  , listImage :: Maybe Image
  , tags      :: Array String
  , premium   :: Boolean
  )

type JSArticleStub =
  { publishingTime :: String
  | ArticleStubCommon
  }

type ArticleStub =
  { publishingTime :: Maybe LocalDateTime
  | ArticleStubCommon
  }

type ArticleCommon =
  ( title     :: String
  , body      :: Array BodyElementJS
  , mainImage :: Maybe Image
  , tags      :: Array String
  , uuid      :: String
  , authors   :: Array Author
  , premium   :: Boolean
  )

type JSArticle =
  { publishingTime :: String
  , updateTime     :: Maybe String
  | ArticleCommon
  }

type Article =
  { publishingTime :: Maybe LocalDateTime
  , updateTime     :: Maybe LocalDateTime
  | ArticleCommon
  }

type Author =
  { byline :: String
  , image  :: Maybe String
  }

parseArticleWith :: forall a b. ReadForeign b => (b -> Effect a) -> Json -> Effect (Either String a)
parseArticleWith parseFn articleResponse = do
  case JSON.readJSON $ stringify articleResponse of
    Right jsArticle -> map Right $ parseFn jsArticle
    Left err -> do
      let parsingErrors = joinWith " " $ fromFoldable $ map renderForeignError err
      -- TODO: Sentry and whatnot
      Console.warn $ "Could not parse article JSON, errors: " <> parsingErrors
      pure $ Left $ "Parsing error: " <> parsingErrors

parseArticle :: Json -> Effect (Either String Article)
parseArticle = parseArticleWith fromJSArticle

parseArticleStub :: Json -> Effect (Either String ArticleStub)
parseArticleStub = parseArticleWith fromJSArticleStub

parseDateTime :: String -> Maybe DateTime
parseDateTime = hush <<< unformat dateTimeFormatter

fromJSArticleStub :: JSArticleStub -> Effect ArticleStub
fromJSArticleStub jsStub@{ uuid, publishingTime } = do
  localPublishingTime <- localizeArticleDateTimeString uuid publishingTime
  pure jsStub { publishingTime = localPublishingTime }

fromJSArticle :: JSArticle -> Effect Article
fromJSArticle jsArticle@{ uuid, publishingTime, updateTime } = do
  localPublishingTime <- localizeArticleDateTimeString uuid publishingTime
  localUpdateTime <- maybe (pure Nothing) (localizeArticleDateTimeString uuid) updateTime
  pure $ jsArticle { publishingTime = localPublishingTime, updateTime = localUpdateTime }

type BodyElementJS =
  { html     :: Maybe String
  , image    :: Maybe Image
  , box      :: Maybe BoxInfo
  , headline :: Maybe String
  , footnote :: Maybe String
  , question :: Maybe String
  , quote    :: Maybe String
  }

data BodyElement
  = Html String
  | Image Image
  | Box BoxInfo
  | Headline String
  | Footnote String
  | Question String
  | Quote String
derive instance bodyElementGeneric :: Generic BodyElement _
instance bodyElementShow :: Show BodyElement where show = genericShow

type BoxInfo =
  { title :: Maybe String
  , headline :: Maybe String
  , content :: Array String
  }

type Image =
  { url       :: String
  , caption   :: Maybe String
  , thumb     :: Maybe String
  , alignment :: Maybe String
  , byline    :: Maybe String
  }
