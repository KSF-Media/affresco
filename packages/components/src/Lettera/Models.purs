module Lettera.Models where

import Prelude

import Data.Argonaut.Core (Json, stringify)
import Data.Argonaut.Decode (class DecodeJson, JsonDecodeError(..), decodeJson, (.!=), (.:), (.:?))
import Data.Argonaut.Encode (class EncodeJson)
import Data.Argonaut.Encode.Class (encodeJson)
import Data.Array (fromFoldable)
import Data.DateTime (DateTime, adjust)
import Data.Either (Either(..), hush)
import Data.Foldable (foldMap)
import Data.Formatter.DateTime (format, unformat)
import Data.Generic.Rep (class Generic)
import Data.Hashable (class Hashable, hash)
import Data.JSDate as JSDate
import Data.Maybe (Maybe(..), maybe)
import Data.Newtype (class Newtype, un, unwrap)
import Data.Show.Generic (genericShow)
import Data.String (joinWith, toLower)
import Data.String (Pattern(..), Replacement(..), replaceAll, toLower) as String
import Data.String.Extra (kebabCase) as String
import Data.String.Pattern (Pattern(..), Replacement(..))
import Data.Time.Duration as Duration
import Effect (Effect)
import Effect.Class.Console as Console
import Foreign (renderForeignError)
import Foreign.Object as Object
import KSF.Helpers (dateTimeFormatter)
import Record (modify)
import Simple.JSON (class ReadForeign, readImpl)
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

isErrorArticle :: FullArticle -> Boolean
isErrorArticle (ErrorArticle _) = true
isErrorArticle _ = false

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
  , externalScripts: Nothing
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
  , premium   :: Boolean
  )

type JSArticleStub =
  { publishingTime :: String
  , tags           :: Array String
  | ArticleStubCommon
  }

type ArticleStub =
  { publishingTime :: Maybe LocalDateTime
  , tags           :: Array Tag
  | ArticleStubCommon
  }

-- For representing HTML string with "<script>" tags.
-- This exists because we need to do some extra steps
-- when we decode/encode the thing
newtype ExternalScript = ExternalScript String
derive instance newtypeExternalScript :: Newtype ExternalScript _

instance readForeignExternalScript :: ReadForeign ExternalScript where
  readImpl f = do
    script <- readImpl f
    pure $ ExternalScript $ String.replaceAll (Pattern "<\\/script>") (Replacement "</script>") script

-- We need to be extra careful when writing <script> tags to the HTML template.
-- Basically, we need an extra backslash before the closing tag, otherwise
-- the DOM gets messed up.
-- More info: https://stackoverflow.com/a/30231195
instance encodeJsonExternalScript :: EncodeJson ExternalScript where
  encodeJson (ExternalScript script) =
    encodeJson $ String.replaceAll (Pattern "</script>") (Replacement "<\\/script>") script

type ArticleCommon =
  ( title     :: String
  , body      :: Array BodyElementJS
  , mainImage :: Maybe Image
  , uuid      :: String
  , preamble  :: Maybe String
  , authors   :: Array Author
  , premium   :: Boolean
  , externalScripts :: Maybe (Array ExternalScript)
  )

type JSArticle =
  { publishingTime :: String
  , updateTime     :: Maybe String
  , tags           :: Array String
  | ArticleCommon
  }

type Article =
  { publishingTime :: Maybe LocalDateTime
  , updateTime     :: Maybe LocalDateTime
  , tags           :: Array Tag
  | ArticleCommon
  }

-- There's no DraftArticle type since that content is identical to
-- what Article has.
type JSDraftArticle =
  { publishingTime :: Maybe String
  , updateTime     :: Maybe String
  , tags           :: Array String
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
      , tags           = map unwrap article.tags
      }

articleStubToJson :: ArticleStub -> Json
articleStubToJson = encodeJson <<< modify (Proxy :: Proxy "tags") (map unwrap) <<< modify (Proxy :: Proxy "publishingTime") (foldMap formatLocalDateTime)

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
                            , tags           = map Tag jsArticle.tags
                            }

parseArticleStubWithoutLocalizing :: Json -> (Either String ArticleStub)
parseArticleStubWithoutLocalizing =
  parseArticlePure (\jsStub -> jsStub { publishingTime = LocalDateTime <$> parseDateTime jsStub.publishingTime
                                      , tags           = map Tag jsStub.tags
                                      })

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
fromJSArticleStub jsStub@{ uuid, publishingTime, tags } = do
  localPublishingTime <- localizeArticleDateTimeString uuid publishingTime
  pure jsStub { publishingTime = localPublishingTime, tags = map Tag tags }

fromJSDraftArticle :: JSDraftArticle -> Effect Article
fromJSDraftArticle jsDraft@{ uuid, publishingTime, updateTime, tags } = do
  localPublishingTime <- maybe (pure Nothing) (localizeArticleDateTimeString uuid) publishingTime
  localUpdateTime <- maybe (pure Nothing) (localizeArticleDateTimeString uuid) updateTime
  pure $ jsDraft { publishingTime = localPublishingTime, updateTime = localUpdateTime, tags = map Tag tags }

fromJSArticle :: JSArticle -> Effect Article
fromJSArticle jsArticle@{ uuid, publishingTime, updateTime, tags } = do
  localPublishingTime <- localizeArticleDateTimeString uuid publishingTime
  localUpdateTime <- maybe (pure Nothing) (localizeArticleDateTimeString uuid) updateTime
  pure $ jsArticle { publishingTime = localPublishingTime, updateTime = localUpdateTime, tags = map Tag tags }

type BodyElementJS =
  { html     :: Maybe String
  , image    :: Maybe Image
  , box      :: Maybe BoxInfo
  , headline :: Maybe String
  , footnote :: Maybe String
  , question :: Maybe String
  , quote    :: Maybe QuoteInfo
  }

data BodyElement
  = Html String
  | Image Image
  | Box BoxInfo
  | Headline String
  | Footnote String
  | Question String
  | Quote QuoteInfo
derive instance bodyElementGeneric :: Generic BodyElement _
instance bodyElementShow :: Show BodyElement where show = genericShow

type BoxInfo =
  { title :: Maybe String
  , headline :: Maybe String
  , content :: Array String
  }

type QuoteInfo =
  { body :: String
  , author :: Maybe String
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

toString :: CategoryType -> String
toString Feed = "feed"
toString Webview = "webview"
toString Link = "link"

instance categoryTypeDecodeJson :: DecodeJson CategoryType where
  decodeJson json = do
    categoryTypeString <- decodeJson json
    case toLower categoryTypeString of
      "feed"    -> Right Feed
      "webview" -> Right Webview
      "link"    -> Right Link
      _         -> Left $ UnexpectedValue json

newtype CategoryLabel = CategoryLabel String
derive instance newtypeCategoryLabel :: Newtype CategoryLabel _

-- (CategoryLabel "Norden Och Världen") is equal to (CategoryLabel "norden-och-världen")
instance eqCategoryLabel :: Eq CategoryLabel where
  eq (CategoryLabel a) (CategoryLabel b) =
    a == b || normalize a == normalize b
    where
      normalize = toLower <<< removeKebabCase <<< removeWhitespace
      removeKebabCase = String.replaceAll (Pattern "-") (Replacement "")
      removeWhitespace = String.replaceAll (Pattern " ") (Replacement "")

-- Kebab cases a label
-- E.g. "Norden Och Världen" -> "norden-och-världen"
instance showCategoryLabel :: Show CategoryLabel where
  show (CategoryLabel a) = toLower $ String.kebabCase a

instance ordCategoryLabel :: Ord CategoryLabel where
  compare a b = compare (show a) (show b)

instance hashableCategoryLabel :: Hashable CategoryLabel where
  hash (CategoryLabel a) = hash a

newtype Category = Category
  { id            :: String
  , label         :: CategoryLabel
  , type          :: CategoryType
  , subCategories :: Array Category
  , url           :: Maybe String
  }

instance categoryDecodeJson :: DecodeJson Category where
  decodeJson json = do
    categoryObj   <- decodeJson json
    id            <- categoryObj .: "id"
    label         <- CategoryLabel <$> categoryObj .: "label"
    type_         <- categoryObj .: "type"
    subCategories <- categoryObj .:? "subcategories" .!= mempty
    url           <- categoryObj .:? "url"
    pure $ Category { id, label, type: type_, subCategories, url }

instance categoryEncodeJson :: EncodeJson Category where
  encodeJson (Category c) = do
    Object.singleton "id" (encodeJson c.id)
    # Object.insert "label" (encodeJson $ unwrap c.label)
    # Object.insert "type" (encodeJson $ toString c.type)
    # Object.insert "subcategories" (encodeJson c.subCategories)
    # Object.insert "url" (encodeJson c.url)
    # encodeJson

derive instance newtypeCategory :: Newtype Category _

newtype Tag = Tag String

uriComponentToTag :: String -> Tag
uriComponentToTag = Tag <<< String.replaceAll (String.Pattern "-") (String.Replacement " ")

tagToURIComponent :: Tag -> String
tagToURIComponent = String.toLower <<< String.replaceAll (String.Pattern " ") (String.Replacement "-") <<< un Tag

instance eqTag :: Eq Tag where
  eq (Tag a) (Tag b) = String.toLower a == String.toLower b
derive newtype instance showTag :: Show Tag

derive instance newtypeTag :: Newtype Tag _
