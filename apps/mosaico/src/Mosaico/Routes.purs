module Mosaico.Routes where

import Prelude

import Data.Foldable (oneOf)
import Data.List (List(..))
import Data.Maybe (Maybe(..))
import Data.Semiring.Free (free)
import Data.Map as Map
import Data.Tuple (Tuple(..))
import Data.Validation.Semiring (invalid)
import Lettera.Models (Categories, Category, CategoryLabel(..), Tag, uriComponentToTag)
import Routing.Match (Match(..), end, lit, optionalMatch, param, root, str)
import Routing.Match.Error (MatchError(..))
import Routing.Types (RoutePart(..))

data MosaicoPage
  = Frontpage -- Should take Paper as parameter
  | DraftPage -- Ignore parameters on client side and just show server side content
  | EpaperPage
  | ProfilePage
  | ArticlePage String
  | NotFoundPage String
  | StaticPage String
  | CategoryPage Category
  | TagPage Tag
  | SearchPage (Maybe String)
  | DebugPage String -- Used for testing
  | MenuPage
derive instance eqMosaicoPage :: Eq MosaicoPage

routes :: Categories -> Match MosaicoPage
routes categories = root *> oneOf
  [ DraftPage <$ (lit "artikel" *> lit "draft" *> str)
  , ArticlePage <$> (lit "artikel" *> str)
  , StaticPage <$> (lit "sida" *> str)
  , EpaperPage <$ (lit "epaper" *> end)
  , ProfilePage <$ (lit "konto" *> end)
  , TagPage <<< uriComponentToTag <$> (lit "tagg" *> str)
  , Frontpage <$ end
  , MenuPage <$ lit "meny"
  , SearchPage <$> (lit "sök" *> optionalMatch (param "q")) <* end
  , DebugPage <$> (lit "debug" *> str)
  , CategoryPage <$> categoryRoute
  , NotFoundPage <$> str
  ]
  where
    categoryRoute =
      let matchRoute route
            | Cons (Path categoryRouteName) rs <- route
            , Just category <- Map.lookup (CategoryLabel categoryRouteName) categories
            = pure $ Tuple rs category
            | otherwise = invalid $ free $ Fail "Not a category"
      in Match matchRoute
