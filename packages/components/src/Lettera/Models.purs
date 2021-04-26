module Lettera.Models where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe)
import Data.Show.Generic (genericShow)

data FullArticle
  = FullArticle Article
  | PreviewArticle Article

fromFullArticle :: FullArticle -> Article
fromFullArticle (FullArticle a) = a
fromFullArticle (PreviewArticle a) = a

isPreviewArticle :: FullArticle -> Boolean
isPreviewArticle (PreviewArticle _) = true
isPreviewArticle _ = false

type ArticleStub =
  { title    :: String
  , uuid     :: String
  , preamble :: String
  , listImage :: Maybe Image
  , tags :: Array String
  , premium :: Boolean
  }

type Article =
  { title     :: String
  , body      :: Array BodyElementJS
  , mainImage :: Maybe Image
  , tags      :: Array String
  }

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
