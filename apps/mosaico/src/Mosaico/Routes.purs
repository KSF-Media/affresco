module Mosaico.Routes where

import Prelude

import Data.Foldable (foldl, oneOf)
import Data.List (List(..))
import Data.Semiring.Free (free)
import Data.Set as Set
import Data.String (toLower)
import Data.Tuple (Tuple(..))
import Data.Validation.Semiring (invalid)
import Lettera.Models (Category(..))
import Routing.Match (Match(..), end, lit, root, str)
import Routing.Match.Error (MatchError(..))
import Routing.Types (RoutePart(..))

data MosaicoPage
  = Frontpage -- Should take Paper as parameter
  | DraftPage -- Ignore parameters on client side and just show server side content
  | ArticlePage String
  | NotFoundPage String
  | StaticPage String
  | CategoryPage String
  | MenuPage
derive instance eqR :: Eq MosaicoPage


routes :: Array Category -> Match MosaicoPage
routes categories = root *> oneOf
  [ DraftPage <$ (lit "artikel" *> lit "draft" *> str)
  , ArticlePage <$> (lit "artikel" *> str)
  , StaticPage <$> (lit "sida" *> str)
  , Frontpage <$end
  , MenuPage <$ lit "meny"
  , CategoryPage <$> categoryRoute
  , NotFoundPage <$> str
  ]
  where
    categoriesSet = foldl (\acc (Category c) -> Set.insert (toLower c.label) acc) Set.empty categories
    categoryRoute =
      let matchRoute route
            | Cons (Path categoryRouteName) rs <- route
            , Set.member (toLower categoryRouteName) categoriesSet
            = pure $ Tuple rs categoryRouteName
            | otherwise = invalid $ free $ Fail "Not a category"
      in Match matchRoute
