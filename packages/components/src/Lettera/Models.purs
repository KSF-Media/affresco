module Lettera.Models where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Maybe (Maybe)
import Data.Nullable (Nullable)

type Article =
  { title     :: String
  , body      :: Array BodyElementJS
  , mainImage :: Image
  , tags      :: Array String
  }

type BodyElementJS =
  { html     :: Nullable String
  , image    :: Nullable Image
  , box      :: Nullable BoxInfo
  , headline :: Nullable String
  , footnote :: Nullable String
  , question :: Nullable String
  , quote    :: Nullable String
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
  { title :: Nullable String
  , headline :: Nullable String
  , content :: Array String
  }

type Image =
  { url       :: String
  , caption   :: Nullable String
  , thumb     :: Nullable String
  , alignment :: Nullable String
  , byline    :: Nullable String
  }
