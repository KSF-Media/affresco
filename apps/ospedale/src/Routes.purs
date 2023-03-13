module Ospedale.Routes
  ( module Ospedale.Routes
  -- From Routing.PushState
  , module Routing.PushState
  ) where

import Prelude

import Data.Either (Either(..), note)
import Data.Foldable (oneOf)
import Data.Maybe (Maybe(..))
import Data.UUID (UUID, parseUUID)
import Data.UUID as UUID
import Effect (Effect)
import Foreign (unsafeToForeign)
import Routing as Routing
import Routing.Match (Match, eitherMatch, end, lit, root, str)
import Routing.PushState (makeInterface) as Routing.PushState
import Routing.PushState as PushState
import Debug

data OspedalePage
  = MainPage
  | ArticlePage (Maybe UUID)
  | MissingPage

routes :: Match OspedalePage
routes = root *> oneOf
  [ MainPage <$ end
  , ArticlePage <<< Just <$> (lit "artikel" *> eitherMatch ((note "not UUID" <<< parseUUID) <$> str))
  , ArticlePage Nothing <$ lit "artikel"
  , pure MissingPage
  ]

match :: String -> Either String OspedalePage
match = Routing.match routes

routeListener :: PushState.PushStateInterface -> (OspedalePage -> Effect Unit) -> Effect (Effect Unit)
routeListener nav sr = do
  flip PushState.locations nav $ \_ location -> do
    traceM { location, match: match location.pathname }
    case match location.pathname of
      Right path -> do
        sr path
      _ -> pure unit

setRoute :: PushState.PushStateInterface -> String -> Effect Unit
setRoute nav = nav.pushState (unsafeToForeign {})
