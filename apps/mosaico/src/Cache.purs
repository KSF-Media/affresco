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
import Data.String (null)
import Data.Time.Duration (Seconds(..))
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..), fst, snd)
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Aff as Aff
import Effect.AVar (AVar)
import Effect.Aff.AVar as AVar
import Effect.AVar as Effect.AVar
import Effect.Class (liftEffect)
import Effect.Class.Console as Console
import Effect.Exception as Exception
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

toValidUntil :: DateTime -> Maybe Int -> Maybe DateTime
toValidUntil now maxAge =
  flip adjust now <<< Seconds <<< toNumber =<< maxAge

-- Reset handle and a value that's being kept updated
type UpdateWatch a = Tuple (Aff Unit) (Aff (Stamped a))

-- prerendered, mainCategoryFeed and mostRead are updated eagerly from
-- Lettera.  byTag and subCategoryFeed is only updated on request.
type Cache =
  { prerendered :: HashMap CategoryLabel (UpdateWatch (Maybe String))
  , mainCategoryFeed :: HashMap CategoryLabel (UpdateWatch (Array ArticleStub))
  , mostRead :: Aff (Stamped (Array ArticleStub))
  , latest :: Aff (Stamped (Array ArticleStub))
  , subCategoryFeed :: AVar (HashMap CategoryLabel (Stamped (Array ArticleStub)))
  , byTag :: AVar (HashMap Tag (Stamped (Array ArticleStub)))
  , subPrerendered :: AVar (HashMap CategoryLabel (Stamped String))
  , paper :: Paper
  }

startUpdates :: forall a. (Boolean -> Aff (LetteraResponse a)) -> Effect (UpdateWatch (Maybe a))
startUpdates fetch = do
  -- Var is empty only if the update thread is not running.
  var <- Effect.AVar.empty
  resetLock <- Effect.AVar.new unit
  resetRequested <- Effect.AVar.new false
  updateLock <- Effect.AVar.new unit
  updateDone <- Effect.AVar.empty
  updateStop <- Effect.AVar.empty
  let withLock :: forall b. AVar Unit -> Aff b -> Aff b
      withLock lock = Aff.bracket (AVar.take lock) (flip AVar.put lock) <<< const
      update = untilJust do
        rst <- withLock resetLock do
          rst <- AVar.take resetRequested
          AVar.put false resetRequested
          pure rst
        LetteraResponse response <- fetch rst
        _ <- AVar.tryTake var
        now <- liftEffect nowDateTime
        let validUntil = fromMaybe now $ toValidUntil now response.maxAge
        case response.body of
          Right value -> do
            AVar.put (Stamped { validUntil, content: Just value }) var
          Left err -> do
            Console.warn $ show err
            when rst $ withLock resetLock do
              _ <- AVar.take resetRequested
              AVar.put true resetRequested
        -- Tell the possible caller to continue, we either have a
        -- value now or they'll just have to do without.
        _ <- AVar.tryTake updateDone
        case response.maxAge of
          -- Quit repeating on error.  Start again on the next request
          -- for the cached resource.
          Nothing -> pure $ Just unit
          Just maxAge -> do
            delay <- Aff.forkAff do
              Aff.delay $ Aff.Milliseconds $ (_ * 1000.0) $ toNumber maxAge
              withLock resetLock $ AVar.put unit updateStop
            AVar.take updateStop
            Aff.killFiber (Exception.error "stop update") delay
            _ <- AVar.tryTake updateStop
            pure Nothing
      start = withLock updateLock do
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
      reset = withLock resetLock do
        _ <- AVar.take resetRequested
        AVar.put true resetRequested
        AVar.put unit updateStop

  Aff.launchAff_ start
  pure $ Tuple reset start

withCat :: forall a m. Monad m => (String -> m a) -> Category -> m (Tuple CategoryLabel a)
withCat f (Category cat) =
  Tuple cat.label <$> f (show cat.label)

initCache :: Paper -> Array Category -> Effect Cache
initCache paper categoryStructure = do
  let prerenderedCategories = filter (\(Category c) -> c.type == Prerendered) categoryStructure
      mainFeedCategories = filter (\(Category c) -> c.type == Feed) categoryStructure
  mainCategoryFeed <-
    HashMap.fromArray
    <$> traverse ((map <<< map <<< map <<< map <<< map <<< map) (join <<< fromFoldable) $
                  withCat $ startUpdates <<< Lettera.getFrontpage paper <<< Just) mainFeedCategories
  prerendered <- HashMap.fromArray <$> traverse (withCat $ startUpdates <<< Lettera.getFrontpageHtml paper) prerenderedCategories
  mostRead <- (map <<< map <<< map) (join <<< fromFoldable) $
              -- Ditch reset handle for mostRead
              map snd $
              -- const to ditch reset parameter from the Lettera call
              startUpdates $ const $ Lettera.getMostRead 0 10 Nothing paper false

  latest <- (map <<< map <<< map) (join <<< fromFoldable) $
              map snd $
              startUpdates $ const $ Lettera.getLatest 0 10 paper

  subCategoryFeed <- Effect.AVar.new HashMap.empty
  byTag <- Effect.AVar.new HashMap.empty
  subPrerendered <- Effect.AVar.new HashMap.empty

  pure { prerendered
       , mainCategoryFeed
       , mostRead
       , latest
       , subCategoryFeed
       , byTag
       , subPrerendered
       , paper
       }

getUsingCache :: forall k m. Hashable k => Monoid m  => AVar (HashMap k (Stamped m)) -> k -> Aff (LetteraResponse m) -> Aff (Stamped m)
getUsingCache cache key action = do
  store <- AVar.take cache
  let fetch = do
        LetteraResponse response <- action
        now <- liftEffect nowDateTime
        pure $ case (\content validUntil -> Stamped { content, validUntil })
                    <$> hush response.body
                    <*> toValidUntil now response.maxAge of
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

-- Only main category prerendered contents are actively cached, others
-- will be fetched on demand.
getFrontpageHtml :: Cache -> CategoryLabel -> Aff (Stamped (Maybe String))
getFrontpageHtml cache category =
  maybe useSubPrerendered snd $ HashMap.lookup category cache.prerendered
  where
    useSubPrerendered =
      (map <<< map) (\h -> if null h then Nothing else Just h) $
      getUsingCache cache.subPrerendered category $
      Lettera.getFrontpageHtml cache.paper (show category) false

getFrontpage :: Cache -> CategoryLabel -> Aff (Stamped (Array ArticleStub))
getFrontpage cache category = do
  case HashMap.lookup category cache.mainCategoryFeed of
    Just c -> snd c
    Nothing -> getUsingCache cache.subCategoryFeed category $
               Lettera.getFrontpage cache.paper (Just $ show category) false

getMostRead :: Cache -> Aff (Stamped (Array ArticleStub))
getMostRead cache = cache.mostRead

getLatest :: Cache -> Aff (Stamped (Array ArticleStub))
getLatest cache = cache.latest

getByTag :: Cache -> Tag -> Aff (Stamped (Array ArticleStub))
getByTag cache tag =
  getUsingCache cache.byTag tag $
  Lettera.getByTag 0 20 tag cache.paper

resetCategory :: Cache -> CategoryLabel -> Aff Unit
resetCategory cache category = do
  removeFromActive cache.prerendered
  removeFromActive cache.mainCategoryFeed
  removeFromPassive cache.subCategoryFeed
  removeFromPassive cache.subPrerendered
  -- TODO Tell Lettera to invalidate on demand cached resources as
  -- well (or let it know some other way)
  where
    removeFromActive :: forall a. HashMap CategoryLabel (UpdateWatch a) -> Aff Unit
    removeFromActive = HashMap.lookup category >>> maybe (pure unit) fst
    removeFromPassive :: forall a. (AVar (HashMap CategoryLabel a)) -> Aff Unit
    removeFromPassive c =
      flip AVar.put c =<< HashMap.delete category <$> AVar.take c

addHeader :: forall a b. DateTime -> Boolean -> Stamped b -> Response a -> Response a
addHeader now private (Stamped { validUntil }) =
  if maxAge <= 0 then identity
  else \(Response response) ->
    Response $ response { headers = Headers.set "cache-control" control response.headers }
  where
    maxAge = floor $ un Seconds $ diff validUntil now
    control = (if private then "private, " else "") <> "max-age=" <> show maxAge
