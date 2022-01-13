module Mosaico.Cache where

import Prelude

import Control.Monad.Rec.Class (untilJust)
import Data.Array (filter, fromFoldable)
import Data.DateTime (DateTime, adjust)
import Data.Either (Either(..), hush)
import Data.Hashable (class Hashable)
import Data.HashMap (HashMap)
import Data.HashMap as HashMap
import Data.Int (toNumber)
import Data.Maybe (Maybe(..), maybe)
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

type StampedArticles =
  { validUntil :: DateTime
  , articles :: Array ArticleStub
  }

-- prerendered, mainCategoryFeed and mostRead are updated eagerly from
-- Lettera.  byTag and subCategoryFeed is only updated on request.
type Cache =
  { prerendered :: HashMap CategoryLabel (Aff (Maybe String))
  , mainCategoryFeed :: HashMap CategoryLabel (Aff (Array ArticleStub))
  , mostRead :: Aff (Array ArticleStub)
  , subCategoryFeed :: AVar (HashMap CategoryLabel StampedArticles)
  , byTag :: AVar (HashMap Tag StampedArticles)
  , paper :: Paper
  }

startUpdates :: forall a. Aff (LetteraResponse a) -> Effect (Aff (Maybe a))
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
        case response.body of
          Right value -> do
            AVar.put (Just value) var
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
            join <$> AVar.tryRead var
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
    <$> traverse ((map <<< map <<< map <<< map) (join <<< fromFoldable) $
                  withCat $ startUpdates <<< Lettera.getFrontpage paper <<< Just) mainFeedCategories
  prerendered <- HashMap.fromArray <$> traverse (withCat $ startUpdates <<< Lettera.getFrontpageHtml paper) prerenderedCategories
  mostRead <- (map <<< map) (join <<< fromFoldable) $ startUpdates $ Lettera.getMostRead 0 10 Nothing paper false

  subCategoryFeed <- Effect.AVar.new HashMap.empty
  byTag <- Effect.AVar.new HashMap.empty

  pure { prerendered
       , mainCategoryFeed
       , mostRead
       , subCategoryFeed
       , byTag
       , paper
       }

getUsingCache :: forall k. Hashable k => AVar (HashMap k StampedArticles) -> k -> Aff (LetteraResponse (Array ArticleStub)) -> Aff (Array ArticleStub)
getUsingCache cache key action = do
  store <- AVar.take cache
  let fetch = do
        (LetteraResponse response) <- action
        now <- liftEffect nowDateTime
        pure $ case { value:_, validUntil:_ }
                    <$> hush response.body
                    <*> (flip adjust now <<< Seconds <<< toNumber =<< response.maxAge) of
          Just { value, validUntil } ->
            { value, newStore: HashMap.insert key ({ validUntil, articles: value }) store }
          _ ->
            { value: mempty, newStore: HashMap.delete key store }
  { value, newStore } <-
    case HashMap.lookup key store of
      Just { validUntil, articles } -> do
        now <- liftEffect nowDateTime
        if now > validUntil then fetch else pure { value: articles, newStore: store }
      Nothing -> fetch
  AVar.put newStore cache
  pure value

-- Assumes that only main categories may have prerendered HTML
getFrontpageHtml :: Cache -> CategoryLabel -> Aff (Maybe String)
getFrontpageHtml cache category =
  maybe (pure Nothing) identity $ HashMap.lookup category cache.prerendered

getFrontpage :: Cache -> CategoryLabel -> Aff (Array ArticleStub)
getFrontpage cache category = do
  case HashMap.lookup category cache.mainCategoryFeed of
    Just c -> c
    Nothing -> getUsingCache cache.subCategoryFeed category $
               Lettera.getFrontpage cache.paper $ Just $ show category

getMostRead :: Cache -> Aff (Array ArticleStub)
getMostRead cache = cache.mostRead

getByTag :: Cache -> Tag -> Aff (Array ArticleStub)
getByTag cache tag =
  getUsingCache cache.byTag tag $
  Lettera.getByTag 0 20 tag cache.paper
