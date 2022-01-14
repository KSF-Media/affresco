module Mosaico.Cache where

import Prelude

import Control.Monad.Rec.Class (untilJust)
import Control.Plus (class Plus, empty)
import Data.Array (filter, fromFoldable)
import Data.DateTime (DateTime, adjust, diff)
import Data.Either (Either(..), hush)
import Data.Hashable (class Hashable)
import Data.HashMap (HashMap)
import Data.HashMap as HashMap
import Data.Int (toNumber, floor)
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Newtype (un)
import Data.Time.Duration (Seconds(..))
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Aff as Aff
import Effect.AVar (AVar)
import Effect.Aff.AVar as AVar
import Effect.AVar as Effect.AVar
import Effect.Class (liftEffect)
import Effect.Class.Console as Console
import Effect.Now (nowDateTime)
import KSF.Paper (Paper)
import Lettera (LetteraResponse(..))
import Lettera as Lettera
import Lettera.Models (ArticleStub, Category(..), CategoryLabel, CategoryType(..), Tag)
import Payload.Headers as Headers
import Payload.ResponseTypes (Response(..))

data Stamped a = Stamped
  { validUntil :: DateTime
  , content :: a
  }

emptyStamp :: forall m a. Plus m => Aff (Stamped (m a))
emptyStamp =
  Stamped <<< { validUntil: _, content: empty } <$> liftEffect nowDateTime

instance functorStamped :: Functor Stamped where
  map f (Stamped s@{ content }) = Stamped $ s { content = f content }

instance applyStamped :: Apply Stamped where
  apply (Stamped { validUntil: t1, content: f}) (Stamped { validUntil: t2, content: x}) =
    Stamped { validUntil: min t1 t2, content: f x }

getContent :: forall a. Stamped a -> a
getContent (Stamped { content }) = content

-- prerendered, mainCategoryFeed and mostRead are updated eagerly from
-- Lettera.  byTag and subCategoryFeed is only updated on request.
type Cache =
  { prerendered :: HashMap CategoryLabel (Aff (Stamped (Maybe String)))
  , mainCategoryFeed :: HashMap CategoryLabel (Aff (Stamped (Array ArticleStub)))
  , mostRead :: Aff (Stamped (Array ArticleStub))
  , subCategoryFeed :: AVar (HashMap CategoryLabel (Stamped (Array ArticleStub)))
  , byTag :: AVar (HashMap Tag (Stamped (Array ArticleStub)))
  , paper :: Paper
  }

startUpdates :: forall a. Aff (LetteraResponse a) -> Effect (Aff (Stamped (Maybe a)))
startUpdates fetch = do
  -- Var is empty only if the update thread is not running.
  var <- Effect.AVar.empty
  updateLock <- Effect.AVar.new unit
  updateDone :: AVar Unit <- Effect.AVar.empty
  let withLock :: forall b. Aff b -> Aff b
      withLock = Aff.bracket (AVar.take updateLock) (flip AVar.put updateLock) <<< const
      update = untilJust do
        LetteraResponse response <- fetch
        _ <- AVar.tryTake var
        now <- liftEffect nowDateTime
        let validUntil = fromMaybe now $ flip adjust now <<< Seconds <<< toNumber =<< response.maxAge
        case response.body of
          Right value -> do
            AVar.put (Stamped { validUntil, content: Just value }) var
          Left err -> do
            Console.warn $ show err
        -- Tell the possible caller to continue, we either have a
        -- value now or they'll just have to do without.
        _ <- AVar.tryTake updateDone
        case response.maxAge of
          -- Quit repeating on error.  Start again on the next request
          -- for the cached resource.
          Nothing -> pure $ Just unit
          Just maxAge -> do
            Aff.delay $ Aff.Milliseconds $ (_ * 1000.0) $ toNumber maxAge
            pure Nothing
      start = withLock do
        val <- AVar.tryRead var
        case val of
          Just v -> pure v
          Nothing -> do
            AVar.put unit updateDone
            _ <- Aff.forkAff update
            -- This will pass when the update worker has completed one
            -- access to the resource.
            AVar.put unit updateDone
            -- Leave it empty for the next caller.
            AVar.take updateDone
            AVar.tryRead var >>= maybe emptyStamp pure
  Aff.launchAff_ start
  pure start

withCat :: forall a m. Monad m => (String -> m a) -> Category -> m (Tuple CategoryLabel a)
withCat f (Category cat) =
  Tuple cat.label <$> f (show cat.label)

initCache :: Paper -> Array Category -> Effect Cache
initCache paper categoryStructure = do
  let prerenderedCategories = filter (\(Category c) -> c.type == Prerendered) categoryStructure
      mainFeedCategories = filter (\(Category c) -> c.type == Feed) categoryStructure
  mainCategoryFeed <-
    HashMap.fromArray
    <$> traverse ((map <<< map <<< map <<< map <<< map) (join <<< fromFoldable) $
                  withCat $ startUpdates <<< Lettera.getFrontpage paper <<< Just) mainFeedCategories
  prerendered <- HashMap.fromArray <$> traverse (withCat $ startUpdates <<< Lettera.getFrontpageHtml paper) prerenderedCategories
  mostRead <- (map <<< map <<< map) (join <<< fromFoldable) $ startUpdates $ Lettera.getMostRead 0 10 Nothing paper false

  subCategoryFeed <- Effect.AVar.new HashMap.empty
  byTag <- Effect.AVar.new HashMap.empty

  pure { prerendered
       , mainCategoryFeed
       , mostRead
       , subCategoryFeed
       , byTag
       , paper
       }

getUsingCache :: forall k. Hashable k => AVar (HashMap k (Stamped (Array ArticleStub))) -> k -> Aff (LetteraResponse (Array ArticleStub)) -> Aff (Stamped (Array ArticleStub))
getUsingCache cache key action = do
  store <- AVar.take cache
  let fetch = do
        LetteraResponse response <- action
        now <- liftEffect nowDateTime
        pure $ case (\content validUntil -> Stamped { content, validUntil })
                    <$> hush response.body
                    <*> (flip adjust now <<< Seconds <<< toNumber =<< response.maxAge) of
          Just value ->
            { value, newStore: HashMap.insert key value store }
          _ ->
            { value: Stamped { validUntil: now, content: mempty }, newStore: HashMap.delete key store }
  { value, newStore } <-
    case HashMap.lookup key store of
      Just value@(Stamped { validUntil }) -> do
        now <- liftEffect nowDateTime
        if now > validUntil then fetch else pure { value, newStore: store }
      Nothing -> fetch
  AVar.put newStore cache
  pure value

-- Assumes that only main categories may have prerendered HTML
getFrontpageHtml :: Cache -> CategoryLabel -> Aff (Stamped (Maybe String))
getFrontpageHtml cache category =
  maybe emptyStamp identity $ HashMap.lookup category cache.prerendered

getFrontpage :: Cache -> CategoryLabel -> Aff (Stamped (Array ArticleStub))
getFrontpage cache category = do
  case HashMap.lookup category cache.mainCategoryFeed of
    Just c -> c
    Nothing -> getUsingCache cache.subCategoryFeed category $
               Lettera.getFrontpage cache.paper $ Just $ show category

getMostRead :: Cache -> Aff (Stamped (Array ArticleStub))
getMostRead cache = cache.mostRead

getByTag :: Cache -> Tag -> Aff (Stamped (Array ArticleStub))
getByTag cache tag =
  getUsingCache cache.byTag tag $
  Lettera.getByTag 0 20 tag cache.paper

addHeader :: forall a b. DateTime -> Boolean -> Stamped b -> Response a -> Response a
addHeader now private (Stamped { validUntil }) =
  if maxAge <= 0 then identity
  else \(Response response) ->
    Response $ response { headers = Headers.set "cache-control" control response.headers }
  where
    maxAge = floor $ un Seconds $ diff validUntil now
    control = (if private then "private, " else "") <> "max-age=" <> show maxAge
