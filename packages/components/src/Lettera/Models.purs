module Lettera.Models where

import Data.Formatter.DateTime
import Prelude

import Control.Monad.Except (runExcept, runExceptT)
import Data.DateTime (DateTime(..))
import Data.Either (Either(..), hush)
import Data.Generic.Rep (class Generic)
import Data.JSDate (JSDate, toDateTime)
import Data.JSDate as JSDate
import Data.Maybe (Maybe(..))
import Data.Show.Generic (genericShow)
import Foreign (Foreign)
import KSF.Helpers (dateTimeFormatter)

data FullArticle
  = FullArticle Article
  | PreviewArticle Article

fromFullArticle :: FullArticle -> Article
fromFullArticle (FullArticle a) = a
fromFullArticle (PreviewArticle a) = a

isPreviewArticle :: FullArticle -> Boolean
isPreviewArticle (PreviewArticle _) = true
isPreviewArticle _ = false

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
  { publishingTime :: DateTime
  | ArticleStubCommon
  }

type Article =
  { title     :: String
  , body      :: Array BodyElementJS
  , mainImage :: Maybe Image
  , tags      :: Array String
  }

fromJSArticleStub :: JSArticleStub -> Maybe ArticleStub
fromJSArticleStub jsStub@{ publishingTime } = do
  pubTime <- hush $ unformat dateTimeFormatter publishingTime
  pure $ jsStub { publishingTime = pubTime }

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
