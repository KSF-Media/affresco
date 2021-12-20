module Mosaico.Routes where

import Prelude

import Data.Foldable (foldr, oneOf)
import Data.List (List(..))
import Data.Maybe (Maybe(..))
import Data.Semiring.Free (free)
import Data.Map as Map
import Data.Tuple (Tuple(..))
import Data.Validation.Semiring (invalid)
import Lettera.Models (Category(..), CategoryLabel(..), Tag, uriComponentToTag)
import Routing.Match (Match(..), end, lit, optionalMatch, param, root, str)
import Routing.Match.Error (MatchError(..))
import Routing.Types (RoutePart(..))

data MosaicoPage
  = Frontpage -- Should take Paper as parameter
  | DraftPage -- Ignore parameters on client side and just show server side content
  | ArticlePage String
  | NotFoundPage String
  | StaticPage String
  | CategoryPage Category
  | TagPage Tag
  | SearchPage (Maybe String)
  | MenuPage
derive instance eqMosaicoPage :: Eq MosaicoPage

routes :: Array Category -> Match MosaicoPage
routes categories = root *> oneOf
  [ DraftPage <$ (lit "artikel" *> lit "draft" *> str)
  , ArticlePage <$> (lit "artikel" *> str)
  , StaticPage <$> (lit "sida" *> str)
  , TagPage <<< uriComponentToTag <$> (lit "tagg" *> str)
  , Frontpage <$ end
  , MenuPage <$ lit "meny"
  , SearchPage <$> (lit "sök" *> optionalMatch (param "q")) <* end
  , CategoryPage <$> categoryRoute
  , NotFoundPage <$> str
  ]
  where
    categoriesMap = foldr (\category@(Category c) -> Map.insert c.label category) Map.empty categories
    categoryRoute =
      let matchRoute route
            | Cons (Path categoryRouteName) rs <- route
            , Just category <- Map.lookup (CategoryLabel categoryRouteName) categoriesMap
            = pure $ Tuple rs category
            | otherwise = invalid $ free $ Fail "Not a category"
      in Match matchRoute
