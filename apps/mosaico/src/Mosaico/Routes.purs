module Mosaico.Routes where

import Prelude

import Data.Foldable (oneOf)
import Lettera.Models (Tag, uriComponentToTag)
import Routing.Match (Match, end, lit, root, str)

data MosaicoPage
  = Frontpage -- Should take Paper as parameter
  | DraftPage -- Ignore parameters on client side and just show server side content
  | ArticlePage String
  | NotFoundPage String
  | StaticPage String
  | TagPage Tag
  | MenuPage
derive instance eqR :: Eq MosaicoPage


routes :: Match MosaicoPage
routes = root *> oneOf
  [ DraftPage <$ (lit "artikel" *> lit "draft" *> str)
  , ArticlePage <$> (lit "artikel" *> str)
  , StaticPage <$> (lit "sida" *> str)
  , TagPage <<< uriComponentToTag <$> (lit "tagg" *> str)
  , Frontpage <$end
  , MenuPage <$ lit "meny"
  , NotFoundPage <$> str
  ]
