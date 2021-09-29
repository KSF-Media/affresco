module Mosaico.Routes where

import Prelude

import Data.Foldable (oneOf)
import Routing.Match (Match, end, lit, root, str)


data MosaicoPage
  = Frontpage -- Should take Paper as parameter
  | ArticlePage String
  | NotFoundPage String
  | MenuPage
derive instance eqR :: Eq MosaicoPage

routes :: Match MosaicoPage
routes = root *> oneOf
  [ ArticlePage <$> (lit "artikel" *> str)
  , Frontpage <$end
  , MenuPage <$ lit "meny"
  , NotFoundPage <$> str
  ]