module Mosaico.Routes where

import Prelude

import Data.Array (any)
import Data.Foldable (oneOf)
import Data.List (List(..))
import Data.Newtype (unwrap)
import Data.NonEmpty (NonEmpty(..))
import Data.Semiring.Free (free)
import Data.String (toLower)
import Data.String.NonEmpty (NonEmptyString)
import Data.Tuple (Tuple(..))
import Data.Validation.Semiring (invalid)
import Lettera.Models (Category(..))
import Routing.Match (Match(..), end, lit, nonempty, root, str)
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
routes c = root *> oneOf
  [ DraftPage <$ (lit "artikel" *> lit "draft" *> str)
  , ArticlePage <$> (lit "artikel" *> str)
  , StaticPage <$> (lit "sida" *> str)
  , Frontpage <$end
  , MenuPage <$ lit "meny"
  , CategoryPage <$> asd
  , NotFoundPage <$> str
  ]
  where
    asd = Match \route ->
      case route of
        Cons (Path input) rs ->
          if any ((\a -> toLower a.id == toLower input) <<< unwrap) c
          then pure $ Tuple rs input
          else invalid $ free ExpectedString
        _ ->
          invalid $ free ExpectedString
