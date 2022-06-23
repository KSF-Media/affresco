module Mosaico.Routes where

import Prelude

import Effect (Effect)
import Foreign (Foreign)
import Data.Foldable (oneOf)
import Data.List (List(..))
import Data.Maybe (Maybe(..))
import Data.Semiring.Free (free)
import Data.Map as Map
import Data.String as String
import Data.String.CodePoints (codePointFromChar)
import Data.Tuple (Tuple(..))
import Data.Validation.Semiring (invalid)
import Lettera.Models (Categories, Category, CategoryLabel(..), Tag, uriComponentToTag)
import Routing.Match (Match(..), end, lit, optionalMatch, param, params, root, str)
import Routing.PushState (PushStateInterface)
import Routing.Match.Error (MatchError(..))
import Routing.Types (RoutePart(..))
import Web.HTML (window)
import Web.HTML.Window (scrollY)

import Simple.JSON (write)

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
  | DeployPreview -- Used for deploy previews only
  | MenuPage
derive instance eqMosaicoPage :: Eq MosaicoPage

-- The URL given from Mosaico module shouldn't have the fragment
-- already, but in case it happens anyway, use this function
stripFragment :: String -> String
stripFragment = String.takeWhile (_ /= codePointFromChar '#')

routes :: Categories -> Match MosaicoPage
routes categories = root *> oneOf
  [ DraftPage <$ (lit "artikel" *> lit "draft" *> str)
  , ArticlePage <$> (lit "artikel" *> str)
  , StaticPage <$> (lit "sida" *> str)
  , EpaperPage <$ (lit "epaper" *> optionalMatch params *> end)
  , ProfilePage <$ (lit "konto" *> end)
  , TagPage <<< uriComponentToTag <$> (lit "tagg" *> str)
  , Frontpage <$ end
  , MenuPage <$ lit "meny"
  , SearchPage <$> (lit "sök" *> optionalMatch (param "q")) <* end
  , DebugPage <$> (lit "debug" *> str)
  , DeployPreview <$ str <* lit "mosaico" <* lit "index.html" <* end
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

type RouteState = { yPositionOnLeave :: Maybe Number }

changeRoute :: PushStateInterface -> String -> Effect Unit
changeRoute router route = do
  currentRoute <- (\l -> l.pathname <> l.search) <$> router.locationState
  currentY     <- scrollY =<< window
  -- Before changing the route, let's write the y scroll position to the state of the current
  -- location, as this is needed for recovery if users go back in browser history
  runRouteEffect router.replaceState { yPositionOnLeave: Just currentY } currentRoute
  runRouteEffect router.pushState { yPositionOnLeave: Nothing } route

runRouteEffect :: (Foreign -> String -> Effect Unit) -> RouteState -> String -> Effect Unit
runRouteEffect f state route = f (write state) route
