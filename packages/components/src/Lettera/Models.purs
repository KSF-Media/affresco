module Lettera.Models where

import Prelude

import Data.Argonaut.Core (Json, caseJsonObject)
import Data.Argonaut.Decode (class DecodeJson, JsonDecodeError(..), decodeJson, printJsonDecodeError, (.!=), (.:), (.:?))
import Data.Argonaut.Encode (class EncodeJson)
import Data.Argonaut.Encode.Class (encodeJson)
import Data.Array (catMaybes, mapMaybe)
import Data.DateTime (DateTime, adjust)
import Data.Either (Either(..), hush)
import Data.Foldable (foldMap, foldr, lookup)
import Data.Formatter.DateTime (format, unformat)
import Data.Generic.Rep (class Generic)
import Data.Hashable (class Hashable, hash)
import Data.Int (round, toNumber)
import Data.JSDate as JSDate
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe, maybe, fromJust)
import Data.Newtype (class Newtype, un, unwrap)
import Data.Show.Generic (genericShow)
import Data.String (toLower)
import Data.String (Pattern(..), Replacement(..), replaceAll, toLower) as String
import Data.String.Extra (kebabCase) as String
import Data.String.Pattern (Pattern(..), Replacement(..))
import Data.Time.Duration as Duration
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..), swap)
import Data.UUID as UUID
import Effect (Effect)
import Effect.Class.Console as Console
import Foreign.Object as Object
import KSF.Helpers (dateTimeFormatter)
import KSF.LocalDateTime (LocalDateTime(..), formatLocalDateTime, parseLocalDateTime)
import KSF.Paper as Paper
import Partial.Unsafe (unsafePartial)
import Record (merge, modify)
import Type.Prelude (Proxy(..))

data MosaicoArticleType = FullArticle | PreviewArticle | DraftArticle | ErrorArticle

derive instance eqMosaicoArticleType :: Eq MosaicoArticleType
instance showArticleType :: Show MosaicoArticleType where
  show FullArticle = "FullArticle"
  show PreviewArticle = "PreviewArticle"
  show DraftArticle = "DraftArticle"
  show ErrorArticle = "ErrorArticle"

readArticleType :: String -> Maybe MosaicoArticleType
readArticleType "FullArticle" = Just FullArticle
readArticleType "PreviewArticle" = Just PreviewArticle
readArticleType "DraftArticle" = Just DraftArticle
readArticleType "ErrorArticle" = Just ErrorArticle
readArticleType _ = Nothing

type FullArticle =
  { articleType :: MosaicoArticleType
  , article :: Article
  }

notFoundArticle :: FullArticle
notFoundArticle =
  { articleType: ErrorArticle
  , article:
    { title: "Hoppsan! Vi kan inte hitta sidan eller artikeln du sökte efter."
    , listTitle: Nothing
    , body: []
    , analyticsCategory: Nothing
    , analyticsSection: Nothing
    , charLength: 0
    , mainImage: Just
        { url: notFoundImage
        , caption: Nothing
        , thumb: notFoundImage
        , tinyThumb: notFoundImage
        , alignment: Nothing
        , byline: Nothing
        , aoiCropped: Nothing
      }
    , tags: []
    , uuid: "notfound"
    , preamble: []
    , authors: []
    , premium: false
    , removeAds: false
    , live: false
    , publishingTime: Nothing
    , publishingTimeUtc: Nothing
    , updateTime: Nothing
    , externalScripts: Nothing
    , articleType: KatastrofLiten
    , articleTypeDetails: Nothing
    , categories: []
    , shareUrl: Nothing
    -- This is okay, this value does not affect the render
    , paper: Paper.KSF
    }
  }

notFoundImage :: String
notFoundImage = "https://cdn.ksfmedia.fi/mosaico/illu-404.svg"

data ArticleType
  = NyhetStor
  | NyhetLiten
  | NyhetRelaterade
  | Opinion
  | KatastrofStor
  | KatastrofLiten
  | Advertorial

derive instance eqArticleType :: Eq ArticleType
derive instance genericArticleType :: Generic ArticleType _
instance showDataArticleType :: Show ArticleType where
  show = genericShow

articleTypes :: Array (Tuple ArticleType String)
articleTypes =
  [ Tuple NyhetStor "NyhetStor"
  , Tuple NyhetLiten "NyhetLiten"
  , Tuple NyhetRelaterade "NyhetRelaterade"
  , Tuple Opinion "Opinion"
  , Tuple KatastrofStor "KatastrofStor"
  , Tuple KatastrofLiten "KatastrofLiten"
  , Tuple Advertorial "Advertorial"
  ]

localizeArticleDateTimeString :: String -> String -> Effect (Maybe LocalDateTime)
localizeArticleDateTimeString uuid dateTimeString =
  case parseLocalDateTime dateTimeString of
    Just time
      | Just d <- fromLocal time -> fromUTCTime d
    _ -> do
      Console.warn $ "Could not parse timestamp for article " <> uuid
      pure Nothing

-- | As the article timestamps we get from Lettera are in UTC timezone,
--   we need to add the timezone offset to them before showing them to the reader.
--   We use the type `LocaDateTime` here to make it more obvious it's not a "regular"
--   `DateTime`.
fromUTCTime :: DateTime -> Effect (Maybe LocalDateTime)
fromUTCTime utcTime = do
  let jsDate = JSDate.fromDateTime utcTime
  offset <- negate <$> JSDate.getTimezoneOffset jsDate
  pure $ LocalDateTime (round offset) <$> adjust (Duration.Minutes offset) utcTime

fromLocal :: LocalDateTime -> Maybe DateTime
fromLocal (LocalDateTime offset time) = adjust (Duration.Minutes $ toNumber $ negate offset) time

type ArticleStubCommon =
  ( title     :: String
  , listTitle :: Maybe String
  , uuid      :: String
  , preamble  :: Array String
  , mainImage :: Maybe Image
  , listImage :: Maybe Image
  , premium   :: Boolean
  , removeAds :: Boolean
  , shareUrl  :: Maybe String
  , authors   :: Array Author
  )

type JSArticleStub =
  { publishingTime     :: String
  , updateTime         :: Maybe String
  , tags               :: Array String
  , articleType        :: String
  , articleTypeDetails :: Maybe ArticleTypeDetails
  | ArticleStubCommon
  }

type ArticleStub =
  { publishingTime     :: Maybe LocalDateTime
  , updateTime         :: Maybe LocalDateTime
  , tags               :: Array Tag
  , articleType        :: ArticleType
  , articleTypeDetails :: Maybe ArticleTypeDetails
  | ArticleStubCommon
  }

-- For representing HTML string with "<script>" tags.
-- This exists because we need to do some extra steps
-- when we decode/encode the thing
newtype ExternalScript = ExternalScript String
derive instance newtypeExternalScript :: Newtype ExternalScript _
derive instance genericExternalScript :: Generic ExternalScript _
instance showExternalScript :: Show ExternalScript where
  show = genericShow

instance decodeJsonExternalScript :: DecodeJson ExternalScript where
  decodeJson f =
    ExternalScript <<< String.replaceAll (Pattern "<\\/script>") (Replacement "</script>") <$> decodeJson f

-- We need to be extra careful when writing <script> tags to the HTML template.
-- Basically, we need an extra backslash before the closing tag, otherwise
-- the DOM gets messed up.
-- More info: https://stackoverflow.com/a/30231195
instance encodeJsonExternalScript :: EncodeJson ExternalScript where
  encodeJson (ExternalScript script) =
    encodeJson $ String.replaceAll (Pattern "</script>") (Replacement "<\\/script>") script

type ArticleCommon =
  ( title              :: String
  , listTitle          :: Maybe String
  , mainImage          :: Maybe Image
  , uuid               :: String
  , preamble           :: Array String
  , authors            :: Array Author
  , analyticsSection   :: Maybe String
  , analyticsCategory  :: Maybe String
  , charLength         :: Int
  , premium            :: Boolean
  , removeAds          :: Boolean
  , live               :: Boolean
  , externalScripts    :: Maybe (Array ExternalScript)
  , articleTypeDetails :: Maybe ArticleTypeDetails
  , categories         :: Array String
  , shareUrl           :: Maybe String
  )

type JSArticle =
  { publishingTime :: String
  , publishingTimeUtc :: Maybe String -- does not come from Lettera
  , updateTime     :: Maybe String
  , body           :: Array Json
  , tags           :: Array String
  , articleType    :: String
  , paper          :: String
  | ArticleCommon
  }

type Article =
  { publishingTime :: Maybe LocalDateTime
  , publishingTimeUtc :: Maybe DateTime -- does not come from Lettera
  , updateTime     :: Maybe LocalDateTime
  , body           :: Array BodyElement
  , tags           :: Array Tag
  , articleType    :: ArticleType
  , paper          :: Paper.Paper
  | ArticleCommon
  }

-- There's no DraftArticle type since that content is identical to
-- what Article has.
type JSDraftArticle =
  { publishingTime :: Maybe String
  , updateTime     :: Maybe String
  , tags           :: Array String
  , articleType    :: String
  , body           :: Array Json
  , paper          :: String
  | ArticleCommon
  }

type Author =
  { byline :: String
  , image  :: Maybe String
  , email  :: Maybe String
  }

articleToArticleStub :: Article -> ArticleStub
articleToArticleStub a =
  { title: a.title
  , listTitle: a.listTitle
  , uuid: a.uuid
  , preamble: a.preamble
  , mainImage: a.mainImage
  , listImage: Nothing
  , authors: a.authors
  , premium: a.premium
  , removeAds: a.removeAds
  , shareUrl: a.shareUrl
  , publishingTime: a.publishingTime
  , updateTime: a.updateTime
  , tags: a.tags
  , articleType: a.articleType
  , articleTypeDetails: a.articleTypeDetails
  }

articleToJson :: Article -> Json
articleToJson = articleToJsonWith formatLocalDateTime

articleToJsonWith :: (LocalDateTime -> String) -> Article -> Json
articleToJsonWith localTimeSerialize article =
  encodeJson $
    article
      { publishingTime = localTimeSerialize <$> article.publishingTime
      , publishingTimeUtc = (\x -> format dateTimeFormatter x) <$> article.publishingTimeUtc
      , updateTime     = localTimeSerialize <$> article.updateTime
      , tags           = map unwrap article.tags
      , body           = mapMaybe bodyElementToJson article.body
      , articleType    = fromMaybe "NyhetStor" $ lookup article.articleType articleTypes
      , paper          = Paper.toString article.paper
      }
  where
    bodyElementToJson (Html html)         = Just $ encodeSingleton "html" html
    bodyElementToJson (Image image)       = Just $ encodeSingleton "image" image
    bodyElementToJson (Box box)           = Just $ encodeSingleton "box" box
    bodyElementToJson (Headline headline) = Just $ encodeSingleton "headline" headline
    bodyElementToJson (Footnote footnote) = Just $ encodeSingleton "footnote" footnote
    bodyElementToJson (Question question) = Just $ encodeSingleton "question" question
    bodyElementToJson (Quote quote)       = Just $ encodeSingleton "quote" quote
    bodyElementToJson (Related related)   = Just $ encodeSingleton "related" $ map articleStubToJson related
    bodyElementToJson (Ad _)              = Nothing
    encodeSingleton :: forall a. EncodeJson a => String -> a -> Json
    encodeSingleton key val = encodeJson $ Object.singleton key val

articleStubToJson :: ArticleStub -> Json
articleStubToJson = encodeJson
                    <<< modify (Proxy :: Proxy "articleType") (fromMaybe "NyhetStor" <<< flip lookup articleTypes)
                    <<< modify (Proxy :: Proxy "tags") (map unwrap)
                    <<< modify (Proxy :: Proxy "publishingTime") (foldMap formatLocalDateTime)
                    <<< modify (Proxy :: Proxy "updateTime") (foldMap formatLocalDateTime)

parseArticleWith :: forall a b. DecodeJson b => (b -> Effect a) -> Json -> Effect (Either String a)
parseArticleWith parseFn articleResponse = do
  case decodeJson articleResponse of
    Right jsArticle -> map Right $ parseFn jsArticle
    Left err -> do
      let parsingErrors = printJsonDecodeError err
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
    \(jsArticle :: JSArticle)-> do
      body <- parseArticlePure (fromJSBody (parseArticleStubWithoutLocalizing <<< encodeJson)) $
              encodeJson (jsArticle.body :: Array Json)
      pure $ merge
        { publishingTime: parseLocalDateTime jsArticle.publishingTime
        , publishingTimeUtc: parseDateTime =<< jsArticle.publishingTimeUtc
        , updateTime: parseLocalDateTime =<< jsArticle.updateTime
        , tags: map Tag jsArticle.tags
        , body: body
        , articleType: fromMaybe NyhetStor $ lookup jsArticle.articleType $ map swap articleTypes
        , paper: fromMaybe Paper.KSF $ Paper.fromString jsArticle.paper
        } jsArticle

parseArticleStubWithoutLocalizing :: Json -> (Either String ArticleStub)
parseArticleStubWithoutLocalizing =
  parseArticlePure
    \jsStub -> pure $
               jsStub { publishingTime = parseLocalDateTime jsStub.publishingTime
                      , updateTime     = parseLocalDateTime jsStub.updateTime
                      , tags           = map Tag jsStub.tags
                      , articleType    = fromMaybe NyhetStor $ lookup jsStub.articleType $ map swap articleTypes
                      }

parseArticlePure :: forall b a. DecodeJson b => (b -> Either String a) -> Json -> (Either String a)
parseArticlePure convertJSArticle jsonArticle =
  case decodeJson jsonArticle of
    Right jsArticle -> convertJSArticle jsArticle
    Left err ->
      let parsingErrors = printJsonDecodeError err
      in Left $ "Parsing error: " <> parsingErrors


parseArticleStub :: Json -> Effect (Either String ArticleStub)
parseArticleStub = parseArticleWith fromJSArticleStub

parseDraftArticle :: Json -> Either String Article
parseDraftArticle =
  parseArticlePure
    \(jsDraftArticle :: JSDraftArticle) -> do
      body <- parseArticlePure (fromJSBody (parseArticleStubWithoutLocalizing <<< encodeJson)) $
              encodeJson (jsDraftArticle.body :: Array Json)
      pure $ merge
        { publishingTime: parseLocalDateTime =<< jsDraftArticle.publishingTime
        , publishingTimeUtc: Nothing
        , updateTime: parseLocalDateTime =<< jsDraftArticle.updateTime
        , tags: map Tag jsDraftArticle.tags
        , body: body
        , articleType: fromMaybe NyhetStor $ lookup jsDraftArticle.articleType $ map swap articleTypes
        , paper: fromMaybe Paper.KSF $ Paper.fromString jsDraftArticle.paper
        } jsDraftArticle

parseDateTime :: String -> Maybe DateTime
parseDateTime = hush <<< unformat dateTimeFormatter

fromJSArticleStub :: JSArticleStub -> Effect ArticleStub
fromJSArticleStub jsStub@{ uuid, publishingTime, updateTime, tags, articleType } = do
  localPublishingTime <- localizeArticleDateTimeString uuid publishingTime
  localUpdateTime <- maybe (pure Nothing) (localizeArticleDateTimeString uuid) updateTime
  pure jsStub
    { publishingTime = localPublishingTime
    , updateTime = localUpdateTime
    , tags = map Tag tags
    , articleType = fromMaybe NyhetStor $ lookup articleType $ map swap articleTypes
    }

fromJSArticle :: JSArticle -> Effect Article
fromJSArticle jsArticle@{ uuid, publishingTime, updateTime, tags, body, articleType, paper } = do
  localPublishingTime <- localizeArticleDateTimeString uuid publishingTime
  localUpdateTime <- maybe (pure Nothing) (localizeArticleDateTimeString uuid) updateTime
  resolvedBody <- fromJSBody fromJSArticleStub body
  pure $ merge
    { publishingTime: localPublishingTime
    , publishingTimeUtc: parseDateTime publishingTime
    , updateTime: localUpdateTime
    , tags: map Tag tags
    , body: resolvedBody
    , articleType: fromMaybe NyhetStor $ lookup articleType $ map swap articleTypes
    , paper: fromMaybe Paper.KSF $ Paper.fromString paper
    } jsArticle

fromJSBody :: forall m. Applicative m => (JSArticleStub -> m ArticleStub) -> Array Json -> m (Array BodyElement)
fromJSBody f = map catMaybes <<< traverse fromJSBodyElement
  where
    fromJSBodyElement :: Json -> m (Maybe BodyElement)
    fromJSBodyElement = caseJsonObject (pure Nothing) $ \o -> case Object.toUnfoldable o of
      [ Tuple "related" relatedJson ]
        | Just related <- hushDecode relatedJson -> Just <<< Related <$> traverse f related
      [ x ] -> pure $ fromPure x
      _ -> pure Nothing
    fromPure :: Tuple String Json -> Maybe BodyElement
    fromPure x = case x of
      Tuple "html" html         -> Html <$> hushDecode html
      Tuple "image" image       -> Image <$> hushDecode image
      Tuple "box" box           -> Box <$> hushDecode box
      Tuple "headline" headline -> Headline <$> hushDecode headline
      Tuple "footnote" footnote -> Footnote <$> hushDecode footnote
      Tuple "question" question -> Question <$> hushDecode question
      Tuple "quote" quote       -> Quote <$> hushDecode quote
      Tuple "ad" ad             -> Ad <$> hushDecode ad
      _ -> Nothing
    hushDecode :: forall a. DecodeJson a => Json -> Maybe a
    hushDecode = hush <<< decodeJson

data BodyElement
  = Html String
  | Image Image
  | Box BoxInfo
  | Headline String
  | Footnote String
  | Question String
  | Quote QuoteInfo
  -- Note that Ad does NOT come from Lettera, but was added here to make smart ad placement possible
  | Ad Ad
  | Related (Array ArticleStub)
derive instance bodyElementGeneric :: Generic BodyElement _
instance bodyElementShow :: Show BodyElement where
  show = genericShow

type BoxInfo =
  { title :: Maybe String
  , headline :: Maybe String
  , content :: Array String
  , type :: String
  }

type QuoteInfo =
  { body :: String
  , author :: Maybe String
  }

type Image =
  { url       :: String
  , caption   :: Maybe String
  , thumb     :: String
  , tinyThumb :: String
  , alignment :: Maybe String
  , byline    :: Maybe String
  , aoiCropped :: Maybe String
  }

type ArticleTypeDetails =
  { title       :: String
  , description :: Maybe String
  }

type DraftParams =
  { time        :: String
  , publication :: String
  , user        :: String
  , hash        :: String
  }

type Ad =
  { contentUnit :: String
  , inBody      :: Boolean
  }

data CategoryType
  = Feed
  | Prerendered
  | Webview
  | Link
  | CategoryTag

derive instance eqCategoryType :: Eq CategoryType
derive instance ordCategoryType :: Ord CategoryType

toString :: CategoryType -> String
toString Feed = "feed"
toString Webview = "webview"
toString Link = "link"
toString Prerendered = "prerendered"
toString CategoryTag = "tag"

instance categoryTypeDecodeJson :: DecodeJson CategoryType where
  decodeJson json = do
    categoryTypeString <- decodeJson json
    case toLower categoryTypeString of
      "feed"        -> Right Feed
      "webview"     -> Right Webview
      "link"        -> Right Link
      "prerendered" -> Right Prerendered
      "tag"         -> Right CategoryTag
      _             -> Left $ UnexpectedValue json

instance categoryTypeEncodeJson :: EncodeJson CategoryType where
  encodeJson Feed = encodeJson "feed"
  encodeJson Webview = encodeJson "webview"
  encodeJson Link = encodeJson "link"
  encodeJson Prerendered = encodeJson "prerendered"
  encodeJson CategoryTag = encodeJson "tag"

newtype CategoryLabel = CategoryLabel String
derive instance newtypeCategoryLabel :: Newtype CategoryLabel _

frontpageCategoryLabel :: CategoryLabel
frontpageCategoryLabel = CategoryLabel "Startsidan"

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
  , tag           :: Maybe Tag
  }

correctionsCategory :: Category
correctionsCategory = Category
  { id: "rättelser"
  , label: CategoryLabel "Rättelser"
  , type: Feed
  , subCategories: mempty
  , url: Nothing
  , tag: Nothing
  }

type Categories = Map CategoryLabel Category

instance categoryDecodeJson :: DecodeJson Category where
  decodeJson json = do
    categoryObj   <- decodeJson json
    id            <- categoryObj .: "id"
    label         <- CategoryLabel <$> categoryObj .: "label"
    type_         <- categoryObj .: "type"
    subCategories <- categoryObj .:? "subcategories" .!= mempty
    url           <- categoryObj .:? "url"
    tag           <- (map <<< map) uriComponentToTag (categoryObj .:? "tag")
    pure $ Category { id, label, type: type_, subCategories, url, tag }

instance categoryEncodeJson :: EncodeJson Category where
  encodeJson (Category c) = do
    Object.singleton "id" (encodeJson c.id)
    # Object.insert "label" (encodeJson $ unwrap c.label)
    # Object.insert "type" (encodeJson $ toString c.type)
    # Object.insert "subcategories" (encodeJson c.subCategories)
    # Object.insert "url" (encodeJson c.url)
    # Object.insert "tag" (encodeJson $ tagToURIComponent <$> c.tag)
    # encodeJson

derive instance newtypeCategory :: Newtype Category _

instance eqCategory :: Eq Category where
  eq (Category a) (Category b) = a.label == b.label

categoriesMap :: Array Category -> Categories
categoriesMap =
  foldr (\cat@(Category c) -> (Map.union $ categoriesMap c.subCategories) <<< Map.insert c.label cat) Map.empty

newtype Tag = Tag String

uriComponentToTag :: String -> Tag
uriComponentToTag = Tag <<< String.replaceAll (String.Pattern "_") (String.Replacement " ")

tagToURIComponent :: Tag -> String
tagToURIComponent = String.replaceAll (String.Pattern " ") (String.Replacement "_") <<< un Tag

instance eqTag :: Eq Tag where
  eq (Tag a) (Tag b) = String.toLower a == String.toLower b
instance hashTag :: Hashable Tag where
  hash (Tag t) = hash $ String.toLower t
derive newtype instance showTag :: Show Tag
derive newtype instance ordTag :: Ord Tag

derive instance newtypeTag :: Newtype Tag _

data Platform = Desktop | Mobile

-- This same deterministic function is in Lettera's code
editorialIdToUuid :: String -> UUID.UUID
editorialIdToUuid editorialId =
  UUID.genv5UUID ("https://hblmedia.fi/" <> editorialId) url_namespace

-- A constant defined in the UUIDv5 standard. Explanations:
--   - https://stackoverflow.com/questions/10867405/generating-v5-uuid-what-is-name-and-namespace
--   - https://en.wikipedia.org/wiki/Universally_unique_identifier#Versions_3_and_5_(namespace_name-based)
--   - https://datatracker.ietf.org/doc/html/rfc4122
url_namespace :: UUID.UUID
url_namespace = unsafePartial $ fromJust $ UUID.parseUUID "6ba7b811-9dad-11d1-80b4-00c04fd430c8"

