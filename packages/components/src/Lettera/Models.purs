module Lettera.Models where

import Prelude

import Control.Monad.Maybe.Trans (MaybeT(..), lift, runMaybeT)
import Data.Argonaut.Core (Json, stringify)
import Data.Array (fromFoldable)
import Data.DateTime (DateTime, adjust)
import Data.Either (Either(..), hush)
import Data.Formatter.DateTime (unformat)
import Data.Generic.Rep (class Generic)
import Data.JSDate as JSDate
import Data.Maybe (Maybe(..))
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
  { publishingTime :: LocalDateTime
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
  { publishingTime :: LocalDateTime
  , updateTime     :: Maybe LocalDateTime
  | ArticleCommon
  }

type Author =
  { byline :: String
  , image  :: Maybe String
  }

parseArticleWith :: forall a b. ReadForeign b => (b -> Effect (Maybe a)) -> Json -> Effect (Either String a)
parseArticleWith parseFn articleResponse = do
  case JSON.readJSON $ stringify articleResponse of
    Right jsArticle -> do
      maybeParsedArticle <- parseFn jsArticle
      case maybeParsedArticle of
        Just article -> pure $ Right article
        _            -> do
          Console.warn $ "Could not parse API article into article"
          pure $ Left "Parsing error"
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

fromJSArticleStub :: JSArticleStub -> Effect (Maybe ArticleStub)
fromJSArticleStub jsStub@{ publishingTime } = runMaybeT do
  pubTime      <- MaybeT (pure $ parseDateTime publishingTime)
  localPubTime <- MaybeT $ fromUTCTime pubTime
  pure $ jsStub { publishingTime = localPubTime }

fromJSArticle :: JSArticle -> Effect (Maybe Article)
fromJSArticle jsArticle@{ publishingTime, updateTime } = runMaybeT do
  publishingDateTime <- MaybeT (pure $ parseDateTime publishingTime)
  localPubTime <- MaybeT $ fromUTCTime publishingDateTime
  localUpdateTime <-
    lift case parseDateTime =<< updateTime of
      Just updTime -> fromUTCTime updTime
      _ -> pure Nothing
  pure $ jsArticle { publishingTime = localPubTime, updateTime = localUpdateTime }

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
