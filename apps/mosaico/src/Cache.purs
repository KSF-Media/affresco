module Mosaico.Cache where

import Prelude

import Control.Monad.Rec.Class (untilJust)
import Control.Parallel.Class (parallel, sequential)
import Control.Plus (class Plus, empty)
import Data.Array (filter, fromFoldable, mapMaybe)
import Data.DateTime (DateTime, adjust, diff)
import Data.Either (Either(..), hush)
import Data.Foldable (foldMap)
import Data.Formatter.DateTime (formatDateTime)
import Data.Int (toNumber, floor)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Newtype (un, unwrap)
import Data.String (null)
import Data.Time.Duration (Seconds(..))
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..), fst, snd, uncurry)
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
import Lettera (LetteraResponse(..))
import Lettera as Lettera
import Lettera.Models (ArticleStub, Category(..), CategoryLabel, CategoryType(..), Categories, Tag)
import Mosaico.Feed (ArticleFeedType(..), ArticleFeed(..))
import Mosaico.Paper (mosaicoPaper)
import Payload.Headers as Headers
import Payload.ResponseTypes (Response(..))

data Stamped a = Stamped
  { validUntil :: DateTime
  , content :: a
  }

emptyStamp :: forall m a. Plus m => Effect (Stamped (m a))
emptyStamp =
  Stamped <<< { validUntil: _, content: empty } <$> nowDateTime

instance functorStamped :: Functor Stamped where
  map f (Stamped s@{ content }) = Stamped $ s { content = f content }

instance applyStamped :: Apply Stamped where
  apply (Stamped { validUntil: t1, content: f}) (Stamped { validUntil: t2, content: x}) =
    Stamped { validUntil: min t1 t2, content: f x }

getContent :: forall a. Stamped a -> a
getContent (Stamped { content }) = content

toValidUntil :: DateTime -> Maybe Int -> Maybe DateTime
toValidUntil now maxAge =
  -- Sanity check, allow at most 1 minute
  flip adjust now <<< Seconds <<< toNumber =<< max 60 <$> maxAge

-- Reset handle and a value that's being kept updated
type UpdateWatch a = Tuple (Aff Unit) (Aff (Stamped a))

data CacheMode = Client | Server

-- prerendered, mainCategoryFeed and mostRead are updated eagerly from
-- Lettera.  byTag and subCategoryFeed is only updated on request.
type Cache =
  { prerendered :: Map CategoryLabel (UpdateWatch (Maybe String))
  , mainCategoryFeed :: Map CategoryLabel (UpdateWatch (Array ArticleStub))
  , mostRead :: Aff (Stamped (Array ArticleStub))
  , latest :: Aff (Stamped (Array ArticleStub))
  , feedList :: AVar (Map ArticleFeedType (Stamped (Array ArticleStub)))
  , feedString :: AVar (Map ArticleFeedType (Stamped String))
  , mode :: CacheMode
  -- Not using UpdateWatch for this, this is controlled by Main.
  , storedCategoryRender :: AVar (Map CategoryLabel (Stamped String))
  }

startUpdates :: forall a. (Maybe String -> Aff (LetteraResponse a)) -> Effect (UpdateWatch (Maybe a))
startUpdates fetch = do
  -- Var is empty only if the update thread is not running.
  var <- Effect.AVar.empty
  resetLock <- Effect.AVar.new unit
  updateLock <- Effect.AVar.new unit
  updateDone <- Effect.AVar.empty
  updateStop <- Effect.AVar.empty
  let withLock :: forall b. AVar Unit -> Aff b -> Aff b
      withLock lock = Aff.bracket (AVar.take lock) (flip AVar.put lock) <<< const
      update = untilJust do
        LetteraResponse response <- fetch =<< hush <<< formatDateTime "YYYYMMDDHHmmss"
                                    <$> liftEffect nowDateTime
        _ <- AVar.tryTake var
        now <- liftEffect nowDateTime
        let validUntil = fromMaybe now $ toValidUntil now response.maxAge
        case response.body of
          Right content -> do
            AVar.put (Stamped { validUntil, content }) var
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
          Just v -> pure $ map Just v
          Nothing -> do
            AVar.put unit updateDone
            _ <- Aff.forkAff update
            -- This will pass when the update worker has completed one
            -- access to the resource.
            AVar.put unit updateDone
            -- Leave it empty for the next caller.
            AVar.take updateDone
            AVar.tryRead var >>= maybe (liftEffect emptyStamp) (map Just >>> pure)
      reset = withLock resetLock do
        AVar.put unit updateStop

  Aff.launchAff_ $ void start
  pure $ Tuple reset start

withCat :: forall a m. Monad m => (String -> m a) -> Category -> m (Tuple CategoryLabel a)
withCat f (Category cat) =
  Tuple cat.label <$> f (show cat.label)

initClientCache :: Array (Tuple ArticleFeedType ArticleFeed) -> Effect Cache
initClientCache initial = do
  now <- nowDateTime
  let validUntil = fromMaybe now $ toValidUntil now (Just 60)
      getList (ArticleList content) = Just $ Stamped { validUntil: validUntil, content }
      getList _ = Nothing
      getHtml (Html _ content) = Just $ Stamped {validUntil: validUntil, content }
      getHtml _ = Nothing
      initFeed :: forall a. (ArticleFeed -> Maybe a) -> Effect (AVar (Map ArticleFeedType a))
      initFeed f = Effect.AVar.new $ Map.fromFoldable $ mapMaybe (traverse f) initial
  feedList <- initFeed getList
  feedString <- initFeed getHtml
  storedCategoryRender <- Effect.AVar.new Map.empty
  static <- pure <$> emptyStamp
  pure $
    { prerendered: Map.empty
    , mainCategoryFeed: Map.empty
    , mostRead: static
    , latest: static
    , feedList
    , feedString
    , mode: Client
    , storedCategoryRender
    }

initServerCache :: Array Category -> Effect Cache
initServerCache categoryStructure = do
  cache <- initClientCache []
  let prerenderedCategories = filter (\(Category c) -> c.type == Prerendered) categoryStructure
  mainCategoryFeed <-
    Map.fromFoldable
    <$> traverse ((map <<< map <<< map <<< map <<< map <<< map) (join <<< fromFoldable) $
                  withCat $ startUpdates <<< Lettera.getFrontpage mosaicoPaper <<< Just) categoryStructure
  prerendered <- Map.fromFoldable <$> traverse
                 (withCat $ startUpdates <<< Lettera.getFrontpageHtml mosaicoPaper) prerenderedCategories
  mostRead <- (map <<< map <<< map) (join <<< fromFoldable) $
              -- Ditch reset handle for mostRead
              map snd $
              -- const to ditch cache stamp from the Lettera call
              startUpdates $ const $ Lettera.getMostRead 0 10 Nothing mosaicoPaper false

  latest <- (map <<< map <<< map) (join <<< fromFoldable) $
              map snd $
              startUpdates $ const $ Lettera.getLatest 0 10 mosaicoPaper

  pure $ cache
    { prerendered = prerendered
    , mainCategoryFeed = mainCategoryFeed
    , mostRead = mostRead
    , latest = latest
    , mode = Server
    }

getUsingCache :: forall k m. Ord k => Monoid m  => AVar (Map k (Stamped m)) -> k -> Aff (LetteraResponse m) -> Aff (Stamped m)
getUsingCache var key action = do
  store <- AVar.read var
  let fetch = do
        LetteraResponse response <- action
        now <- liftEffect nowDateTime
        pure $ case (\content validUntil -> Stamped { content, validUntil })
                    <$> hush response.body
                    <*> toValidUntil now response.maxAge of
          Just value ->
            { value, newStore: Just $ Map.insert key value }
          _ ->
            { value: Stamped { validUntil: now, content: mempty }, newStore: Just $ Map.delete key }
  { value, newStore } <-
    case Map.lookup key store of
      Just value@(Stamped { validUntil }) -> do
        now <- liftEffect nowDateTime
        if now > validUntil then fetch else pure { value, newStore: Nothing }
      Nothing -> fetch
  -- TODO: This is good enough locking to allow concurrent requests to
  -- different resources, but better yet would be to let at most one
  -- concurrent fetch for each.
  foldMap (\f -> do
              oldStore <- AVar.take var
              AVar.put (f oldStore) var) newStore
  pure value

-- Only main category prerendered contents are actively cached, others
-- will be fetched on demand.
getFrontpageHtml :: Cache -> CategoryLabel -> Aff (Stamped (Maybe String))
getFrontpageHtml cache category =
  maybe useFeed snd $ Map.lookup category cache.prerendered
  where
    useFeed =
      (map <<< map) (\h -> if null h then Nothing else Just h) $
      getUsingCache cache.feedString (CategoryFeed category) $
      Lettera.getFrontpageHtml mosaicoPaper (show category) Nothing

getBreakingNewsHtml :: Cache -> Aff (Stamped String)
getBreakingNewsHtml cache =
  getUsingCache cache.feedString BreakingNewsFeed $
    Lettera.getBreakingNewsHtml mosaicoPaper Nothing

getFrontpage :: Cache -> CategoryLabel -> Aff (Stamped (Array ArticleStub))
getFrontpage cache category = do
  case Map.lookup category cache.mainCategoryFeed of
    Just c -> snd c
    Nothing -> getUsingCache cache.feedList (CategoryFeed category) $
               Lettera.getFrontpage mosaicoPaper (Just $ show category) Nothing

getMostRead :: Cache -> Aff (Stamped (Array ArticleStub))
getMostRead cache@{ mode: Client } =
  getUsingCache cache.feedList MostReadFeed $
  Lettera.getMostRead 0 10 Nothing mosaicoPaper false
getMostRead cache = cache.mostRead

getLatest :: Cache -> Aff (Stamped (Array ArticleStub))
getLatest cache@{ mode: Client } =
  getUsingCache cache.feedList LatestFeed $
  Lettera.getLatest 0 10 mosaicoPaper
getLatest cache = cache.latest

getByTag :: Cache -> Tag -> Aff (Stamped (Array ArticleStub))
getByTag cache tag =
  getUsingCache cache.feedList (TagFeed tag) $
  Lettera.getByTag 0 20 tag mosaicoPaper

resetCategory :: Cache -> CategoryLabel -> Aff Unit
resetCategory cache category = do
  removeFromActive cache.prerendered
  removeFromActive cache.mainCategoryFeed
  removeFromPassive cache.feedString
  removeFromPassive cache.feedList
  where
    removeFromActive :: forall a. Map CategoryLabel (UpdateWatch a) -> Aff Unit
    removeFromActive = Map.lookup category >>> maybe (pure unit) fst
    removeFromPassive :: forall a. (AVar (Map ArticleFeedType a)) -> Aff Unit
    removeFromPassive c =
      flip AVar.put c =<< Map.delete (CategoryFeed category) <$> AVar.take c

saveCategoryRender :: Cache -> CategoryLabel -> Stamped String -> Aff Unit
saveCategoryRender cache category content = do
  let store = cache.storedCategoryRender
  flip AVar.put store =<< Map.insert category content <$> AVar.take store

readCategoryRender :: Cache -> CategoryLabel -> Aff (Maybe (Stamped String))
readCategoryRender cache category = do
  let store = cache.storedCategoryRender
  cached <- Map.lookup category <$> AVar.read store
  case cached of
    Nothing -> pure Nothing
    Just value@(Stamped {validUntil}) -> do
      now <- liftEffect nowDateTime
      if now > validUntil
        then do
        flip AVar.put store =<< Map.delete category <$> AVar.take store
        pure Nothing
        else pure $ Just value

addHeader :: forall a b. DateTime -> Stamped b -> Response a -> Response a
addHeader now (Stamped { validUntil }) =
  if maxAge <= 0 then identity
  else addHeaderAge maxAge
  where
    maxAge = floor $ un Seconds $ diff validUntil now

addHeaderAge :: forall a. Int -> Response a -> Response a
addHeaderAge maxAge (Response response) =
  Response $ response { headers = Headers.set "cache-control" control response.headers }
  where
    control = "max-age=" <> show maxAge

-- Only used in client context
isFresh :: Cache -> Categories -> ArticleFeedType -> Aff Boolean
isFresh cache catMap feed = do
  now <- liftEffect nowDateTime
  let stampValid :: forall a. Stamped a -> Boolean
      stampValid (Stamped {validUntil}) = validUntil >= now
      isValid :: forall a. AVar (Map ArticleFeedType (Stamped a)) -> Aff Boolean
      isValid = AVar.read >=> Map.lookup feed >>> maybe false stampValid >>> pure
  case feed of
    CategoryFeed c
      | Just cat <- unwrap <$> Map.lookup c catMap
      , Prerendered <- cat.type -> isValid cache.feedString
    BreakingNewsFeed -> isValid cache.feedString
    _ -> isValid cache.feedList

type CommonLists a =
  { pageContent :: a
  , breakingNews :: Stamped String
  , mostReadArticles :: Stamped (Array ArticleStub)
  , latestArticles :: Stamped (Array ArticleStub)
  }

parallelWithCommonLists :: forall a. Cache -> Aff a -> Aff (CommonLists a)
parallelWithCommonLists cache f =
  sequential $ { pageContent: _, breakingNews: _, mostReadArticles: _, latestArticles: _ }
  <$> parallel f
  <*> parallel (getBreakingNewsHtml cache)
  <*> parallel (getMostRead cache)
  <*> parallel (getLatest cache)

parallelWithCommonActions
  :: forall a. Cache
  -> (ArticleFeedType -> ArticleFeed -> Aff Unit)
  -> Aff a
  -> Aff a
parallelWithCommonActions cache f action = do
  { pageContent, breakingNews, mostReadArticles, latestArticles } <- parallelWithCommonLists cache action
  f BreakingNewsFeed $ Html [] $ getContent breakingNews
  f MostReadFeed $ ArticleList $ getContent mostReadArticles
  f LatestFeed $ ArticleList $ getContent latestArticles
  pure pageContent

parallelLoadFeeds
  :: Cache
  -> (ArticleFeedType -> ArticleFeed -> Aff Unit)
  -> Aff (Tuple ArticleFeedType ArticleFeed)
  -> Aff Unit
parallelLoadFeeds cache f action = do
  parallelWithCommonActions cache f action >>= uncurry f

loadFeed :: Cache -> Categories -> ArticleFeedType -> Maybe (Aff ArticleFeed)
loadFeed cache catMap feedName =
  case feedName of
    TagFeed t -> Just $
      ArticleList <<< getContent <$> getByTag cache t
    CategoryFeed c
      | Just cat <- unwrap <$> Map.lookup c catMap ->
        case cat.type of
          Prerendered -> Just do
            {list, html} <- sequential $
              {list: _, html: _}
              <$> parallel (getContent <$> getFrontpage cache cat.label)
              <*> parallel (getContent <$> getFrontpageHtml cache cat.label)
            pure $ maybe (ArticleList list) (Html list) html
          Feed -> Just $ ArticleList <<< getContent <$> getFrontpage cache cat.label
          _ -> Nothing
    CategoryFeed _ -> Nothing
    SearchFeed q -> Just $ ArticleList <<< join <<< fromFoldable <$> Lettera.search 0 20 mosaicoPaper q
  -- loadFeed isn't called for these values but for completeness' sake
    LatestFeed -> Just $ ArticleList <<< getContent <$> getLatest cache
    MostReadFeed -> Just $ ArticleList <<< getContent <$> getMostRead cache
    BreakingNewsFeed -> Just $ Html [] <<< getContent <$> getBreakingNewsHtml cache
