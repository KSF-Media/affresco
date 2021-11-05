module Lettera.Models where

import Prelude

import Data.Argonaut.Core (Json, stringify)
import Data.Argonaut.Decode (class DecodeJson, JsonDecodeError(..), decodeJson, (.!=), (.:), (.:?))
import Data.Argonaut.Encode.Class (encodeJson)
import Data.Array (fromFoldable)
import Data.DateTime (DateTime, adjust)
import Data.Either (Either(..), hush)
import Data.Foldable (foldMap)
import Data.Formatter.DateTime (format, unformat)
import Data.Generic.Rep (class Generic)
import Data.JSDate as JSDate
import Data.Maybe (Maybe(..), maybe)
import Data.Newtype (class Newtype, un)
import Data.Show.Generic (genericShow)
import Data.String (joinWith)
import Data.String as String
import Data.Time.Duration as Duration
import Effect (Effect)
import Effect.Class.Console as Console
import Foreign (renderForeignError)
import KSF.Helpers (dateTimeFormatter)
import Record (modify)
import Simple.JSON (class ReadForeign)
import Simple.JSON as JSON
import Type.Prelude (Proxy(..))

data FullArticle
  = FullArticle Article
  | PreviewArticle Article
  | DraftArticle Article
  | ErrorArticle Article

fromFullArticle :: FullArticle -> Article
fromFullArticle (FullArticle a) = a
fromFullArticle (PreviewArticle a) = a
fromFullArticle (DraftArticle a) = a
fromFullArticle (ErrorArticle a) = a

isPreviewArticle :: FullArticle -> Boolean
isPreviewArticle (PreviewArticle _) = true
isPreviewArticle _ = false

isDraftArticle :: FullArticle -> Boolean
isDraftArticle (DraftArticle _) = true
isDraftArticle _ = false

notFoundArticle :: FullArticle
notFoundArticle = ErrorArticle
  { title: "Hoppsan! Sidan eller artikeln hittades inte"
  , body: []
  , mainImage: Nothing
  , tags: []
  , uuid: "notfound"
  , preamble: Nothing
  , authors: []
  , premium: false
  , publishingTime: Nothing
  , updateTime: Nothing
  }

newtype LocalDateTime = LocalDateTime DateTime
derive instance newtypeLocalDateTime :: Newtype LocalDateTime _

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
  , preamble  :: Maybe String
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
  , preamble  :: Maybe String
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

-- There's no DraftArticle type since that content is identical to
-- what Article has.
type JSDraftArticle =
  { publishingTime :: Maybe String
  , updateTime     :: Maybe String
  | ArticleCommon
  }

type Author =
  { byline :: String
  , image  :: Maybe String
  }

-- TODO: Could be a type class
encodeStringifyArticle :: Article -> String
encodeStringifyArticle = stringify <<< encodeJson <<< articleToJson

encodeStringifyArticleStubs :: Array ArticleStub -> String
encodeStringifyArticleStubs = stringify <<< encodeJson <<< map articleStubToJson

articleToJson :: Article -> Json
articleToJson article =
  encodeJson $
    article
      { publishingTime = foldMap formatLocalDateTime article.publishingTime
      , updateTime     = foldMap formatLocalDateTime article.updateTime
      }

articleStubToJson :: ArticleStub -> Json
articleStubToJson = encodeJson <<< modify (Proxy :: Proxy "publishingTime") (foldMap formatLocalDateTime)

formatLocalDateTime :: LocalDateTime -> String
formatLocalDateTime = format dateTimeFormatter <<< un LocalDateTime

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

-- | An uneffecful function for parsing `Json` to an `Article`
--   Note that `publishingTime` and `updateTime` are not localized here!
--   Unless you have the correct times in the `Json`, the return value
--   will have wrong timestamps
parseArticleWithoutLocalizing :: Json -> (Either String Article)
parseArticleWithoutLocalizing =
  parseArticlePure
    \jsArticle -> jsArticle { publishingTime = LocalDateTime <$> parseDateTime jsArticle.publishingTime
                            , updateTime     = LocalDateTime <$> (parseDateTime =<< jsArticle.updateTime)
                            }

parseArticleStubWithoutLocalizing :: Json -> (Either String ArticleStub)
parseArticleStubWithoutLocalizing =
  parseArticlePure (\jsStub -> jsStub { publishingTime = LocalDateTime <$> parseDateTime jsStub.publishingTime })

parseArticlePure :: forall b a. ReadForeign b => (b -> a) -> Json -> (Either String a)
parseArticlePure convertJSArticle jsonArticle =
  case JSON.readJSON $ stringify jsonArticle of
    Right jsArticle -> Right $ convertJSArticle jsArticle
    Left err ->
      let parsingErrors = joinWith " " $ fromFoldable $ map renderForeignError err
      in Left $ "Parsing error: " <> parsingErrors


parseArticleStub :: Json -> Effect (Either String ArticleStub)
parseArticleStub = parseArticleWith fromJSArticleStub

parseDraftArticle :: Json -> Effect (Either String Article)
parseDraftArticle = parseArticleWith fromJSDraftArticle

parseDateTime :: String -> Maybe DateTime
parseDateTime = hush <<< unformat dateTimeFormatter

fromJSArticleStub :: JSArticleStub -> Effect ArticleStub
fromJSArticleStub jsStub@{ uuid, publishingTime } = do
  localPublishingTime <- localizeArticleDateTimeString uuid publishingTime
  pure jsStub { publishingTime = localPublishingTime }

fromJSDraftArticle :: JSDraftArticle -> Effect Article
fromJSDraftArticle jsDraft@{ uuid, publishingTime, updateTime } = do
  localPublishingTime <- maybe (pure Nothing) (localizeArticleDateTimeString uuid) publishingTime
  localUpdateTime <- maybe (pure Nothing) (localizeArticleDateTimeString uuid) updateTime
  pure $ jsDraft { publishingTime = localPublishingTime, updateTime = localUpdateTime }

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
  , thumb     :: String
  , alignment :: Maybe String
  , byline    :: Maybe String
  }

type DraftParams =
  { time        :: String
  , publication :: String
  , user        :: String
  , hash        :: String
  }

data CategoryType
  = Feed
  | Webview
  | Link

instance categoryTypeDecodeJson :: DecodeJson CategoryType where
  decodeJson json = do
    categoryTypeString <- decodeJson json
    case String.toLower categoryTypeString of
      "feed"    -> Right Feed
      "webview" -> Right Webview
      "link"    -> Right Link
      _         -> Left $ UnexpectedValue json

newtype Category = Category
  { id            :: String
  , label         :: String
  , type          :: CategoryType
  , subCategories :: Array Category
  , url           :: Maybe String
  }

instance categoryDecodeJson :: DecodeJson Category where
  decodeJson json = do
    categoryObj   <- decodeJson json
    id            <- categoryObj .: "id"
    label         <- categoryObj .: "label"
    type_         <- categoryObj .: "type"
    subCategories <- categoryObj .:? "subCategories" .!= mempty
    url           <- categoryObj .:? "url"
    pure $ Category { id, label, type: type_, subCategories, url }
