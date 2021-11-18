module Mosaico.Routes where

import Prelude

import Data.Foldable (foldl, oneOf, oneOf)
import Data.List (List(..))
import Data.Semiring.Free (free)
import Data.Set as Set
import Data.Tuple (Tuple(..))
import Data.Validation.Semiring (invalid)
import Lettera.Models (Category(..), CategoryLabel(..), Tag, uriComponentToTag)
import Routing.Match (Match(..), end, lit, root, str)
import Routing.Match.Error (MatchError(..))
import Routing.Types (RoutePart(..))

data MosaicoPage
  = Frontpage -- Should take Paper as parameter
  | DraftPage -- Ignore parameters on client side and just show server side content
  | ArticlePage String
  | NotFoundPage String
  | StaticPage String
  | CategoryPage CategoryLabel
  | TagPage Tag
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
  , CategoryPage <<< CategoryLabel <$> categoryRoute
  , NotFoundPage <$> str
  ]
  where
    categoriesSet cats = foldl (\acc (Category c) -> Set.insert c.label acc # Set.union (categoriesSet c.subCategories)) Set.empty cats
    categoryRoute =
      let matchRoute route
            | Cons (Path categoryRouteName) rs <- route
            , Set.member (CategoryLabel categoryRouteName) (categoriesSet categories)
            = pure $ Tuple rs categoryRouteName
            | otherwise = invalid $ free $ Fail "Not a category"
      in Match matchRoute
