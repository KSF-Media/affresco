module Lettera where

import Prelude

import Affjax (Error, Response, defaultRequest, request, printError, get) as AX
import Affjax.RequestHeader (RequestHeader(..)) as AX
import Affjax.ResponseFormat (json, string) as AX
import Affjax.ResponseFormat as ResponseFormat
import Affjax.StatusCode (StatusCode(..))
import Data.Argonaut.Core (Json, toArray, toObject)
import Data.Argonaut.Decode (decodeJson)
import Data.Array (foldl, partition, snoc)
import Data.Date (Date, Day, Month, Year, day, month, year)
import Data.Either (Either(..), either, hush, isRight)
import Data.Enum (class BoundedEnum, fromEnum)
import Data.Foldable (class Foldable, foldMap)
import Data.Foldable as Foldable
import Data.HTTP.Method (Method(..))
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Newtype (un, unwrap)
import Data.Traversable (traverse, traverse_)
import Data.UUID (UUID, toString)
import Data.UUID as UUID
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Class.Console as Console
import Foreign.Object (lookup)
import KSF.Api (Token(..), UserAuth)
import KSF.Auth as Auth
import KSF.Driver (getDriver)
import KSF.Paper (Paper)
import KSF.Paper as Paper
import Lettera.Header as Cache
import Lettera.Models (ArticleStub, Category, DraftParams, FullArticle, MosaicoArticleType(..), Platform(..), Tag(..), parseArticle, parseArticleStub, parseDraftArticle)

foreign import letteraBaseUrl :: String
foreign import _encodeURIComponent :: String -> String

letteraArticleUrl :: String
letteraArticleUrl = letteraBaseUrl <>  "/article/"

letteraDraftUrl :: String
letteraDraftUrl = letteraBaseUrl <> "/article/draft/"

letteraArticleSlugUrl :: String
letteraArticleSlugUrl = letteraBaseUrl <> "/article/slug/"

letteraFrontPageUrl :: String
letteraFrontPageUrl = letteraBaseUrl <> "/list/frontpage"

letteraFrontPageHtmlUrl :: String
letteraFrontPageHtmlUrl = letteraBaseUrl <> "/list/frontpage/html"

letteraBreakingNewsUrl :: String
letteraBreakingNewsUrl = letteraBaseUrl <> "/list/breaking-news/html"

letteraMostReadUrl :: String
letteraMostReadUrl = letteraBaseUrl <> "/list/mostread/"

letteraLatestUrl :: String
letteraLatestUrl = letteraBaseUrl <> "/list/latest/"

letteraByDayUrl :: String
letteraByDayUrl = letteraBaseUrl <> "/article/by-day"

letteraCategoryUrl :: String
letteraCategoryUrl = letteraBaseUrl <> "/categories"

letteraTagUrl :: String
letteraTagUrl = letteraBaseUrl <> "/list/tag/"

letteraSearchUrl :: String
letteraSearchUrl = letteraBaseUrl <> "/list/search"

letteraAdvertorialUrl :: String
letteraAdvertorialUrl = letteraBaseUrl <> "/list/active-advertorial"

letteraArchiveUrl :: String
letteraArchiveUrl = letteraBaseUrl <> "/archive"

data LetteraError
  = ResponseError AX.Error
  | HttpError Int
  | ParseError

instance showLetteraError :: Show LetteraError where
  show (ResponseError err) = "ResponseError " <> AX.printError err
  show (HttpError code)    = "HttpError " <> show code
  show ParseError          = "ParseError"

data LetteraResponse a = LetteraResponse
  { maxAge :: Maybe Int
  , body :: Either LetteraError a
  }

responseBody :: forall a. LetteraResponse a -> Maybe a
responseBody (LetteraResponse a) = hush a.body

instance functorLetteraResponse :: Functor LetteraResponse where
  map f (LetteraResponse r@{ body }) =
    LetteraResponse $ r { body = f <$> body }

instance foldableLetteraResponse :: Foldable LetteraResponse where
  foldl f z = Foldable.foldl f z <<< responseBody
  foldr f z = Foldable.foldr f z <<< responseBody
  foldMap f = foldMap f <<< responseBody

handleLetteraError :: LetteraError -> Effect Unit
handleLetteraError = Console.warn <<< show

getArticleAuth :: UUID -> Paper -> Aff (Either String FullArticle)
getArticleAuth articleId paper = do
  tokens <- Auth.loadToken
  getArticle articleId paper tokens Nothing

-- TODO: Instead of String, use some sort of LetteraError or something
getArticle :: UUID -> Paper -> Maybe UserAuth -> Maybe String -> Aff (Either String FullArticle)
getArticle articleId paper = getArticleWithUrl (letteraArticleUrl <> (toString articleId)) (Just paper)

getArticleWithSlug :: String -> Paper -> Maybe UserAuth -> Maybe String -> Aff (Either String FullArticle)
getArticleWithSlug slug = getArticleWithUrl (letteraArticleSlugUrl <> slug) <<< Just

getArticleWithUrl :: String -> Maybe Paper -> Maybe UserAuth -> Maybe String -> Aff (Either String FullArticle)
getArticleWithUrl url paper auth clientip = do
  let request = AX.defaultRequest
        { url = url <> foldMap (\p -> "?paper=" <> Paper.toString p) paper
        , method = Left GET
        , responseFormat = AX.json
        , headers =
            case auth of
              Just { userId, authToken: Token authToken } ->
                [ AX.RequestHeader "AuthUser" $ UUID.toString userId
                , AX.RequestHeader "Authorization" ("OAuth " <> authToken)
                , AX.RequestHeader "X-Real-Ip" (fromMaybe "" clientip)
                ]
              _ ->
                [ AX.RequestHeader "X-Real-Ip" (fromMaybe "" clientip) ]
        }
  driver <- liftEffect getDriver
  articleResponse <- AX.request driver request
  case articleResponse of
    Left err -> pure $ Left $ "Article GET response failed to decode: " <> AX.printError err
    Right response
      | (StatusCode 200) <- response.status ->
        either Left (Right <<< { articleType: FullArticle, article: _ }) <$>
        (liftEffect $ parseArticle response.body)
      | (StatusCode 403) <- response.status -> do
          {- If we get a Forbidden response, that means the user is not entitled to read the article.
             However in this case, we have the article preview in the response,
             we just need to dig it out.

             The response JSON looks something like:

                { "http_status":"Forbidden",
                  "http_code":403,
                  "not_entitled": {
                    "description": "Not entitled to article.",
                    "articlePreview":
                      { "body": [ .. ] }
                 }
          -}
        case articlePreviewJson response.body of
          Just articlePreview -> do
            map { articleType: PreviewArticle, article: _ } <$>
              (liftEffect $ parseArticle articlePreview)
          Nothing -> do
            -- TODO: Sentry and whatnot
            Console.warn "Did not find article preview from response!"
            pure $ Left "Parsing error"
      | (StatusCode s) <- response.status -> pure $ Left $ "Unexpected HTTP status: " <> show s

getArticleStub :: UUID -> Aff (Either String ArticleStub)
getArticleStub uuid = do
  driver <- liftEffect getDriver
  articleResponse <- AX.get driver ResponseFormat.json $ letteraArticleUrl <> toString uuid <> "/stub"
  case articleResponse of
    Left err -> pure $ Left $ "Article GET response failed to decode: " <> AX.printError err
    Right response
      | (StatusCode 200) <- response.status ->
        liftEffect $ parseArticleStub response.body
      | (StatusCode s) <- response.status -> pure $ Left $ "Unexpected HTTP status: " <> show s

articlePreviewJson :: Json -> Maybe Json
articlePreviewJson =
  toObject
  >=> lookup "not_entitled"
  >=> toObject
  >=> lookup "articlePreview"

getDraftArticle :: String -> DraftParams -> Aff (Either String FullArticle)
getDraftArticle aptomaId { time, publication, user, hash } = do
  let request = AX.defaultRequest
        { url = letteraDraftUrl <> aptomaId
                <> "?time=" <> time
                <> "&publication=" <> publication
                <> "&user=" <> user
                <> "&hash=" <> hash
        , method = Left GET
        , responseFormat = AX.json
        }
  driver <- liftEffect getDriver
  articleResponse <- AX.request driver request
  pure $ case articleResponse of
    Left err -> Left $ "Article GET response failed to decode: " <> AX.printError err
    Right response
      | (StatusCode 200) <- response.status ->
        { articleType: DraftArticle, article: _ } <$> parseDraftArticle response.body
      | (StatusCode 403) <- response.status ->
        Left "Unauthorized"
      | (StatusCode s) <- response.status -> Left $ "Unexpected HTTP status: " <> show s

useResponse :: forall a b. (a -> Aff (Either LetteraError b)) -> Either AX.Error (AX.Response a) -> Aff (LetteraResponse b)
useResponse _ (Left err) = pure $ LetteraResponse { maxAge: Nothing, body: Left $ ResponseError err }
useResponse f (Right response)
  | (StatusCode 200) <- response.status = do
    result <- f response.body
    pure $ case result of
      Right body -> LetteraResponse { maxAge: Cache.getMaxAge $ Cache.parseResponseHeaders response.headers
                                    , body: Right body
                                    }
      Left err -> LetteraResponse { maxAge: Nothing, body: Left err }
  | otherwise =
      pure $ LetteraResponse { maxAge: Nothing, body: Left $ HttpError $ unwrap $ response.status }

getFrontpageHtml :: Paper -> String -> Maybe String -> Aff (LetteraResponse String)
getFrontpageHtml paper category cacheToken = do
  let request = AX.defaultRequest
        { url = letteraFrontPageHtmlUrl
                <> "?paper=" <> Paper.toString paper
                <> "&category=" <> _encodeURIComponent category
                <> foldMap ("&cacheToken=" <> _) cacheToken
        , method = Left GET
        , responseFormat = AX.string
        }
  driver <- liftEffect getDriver
  useResponse (pure <<< pure) =<< AX.request driver request

getBreakingNewsHtml :: Paper -> Maybe String -> Aff (LetteraResponse String)
getBreakingNewsHtml paper cacheToken = do
  let request = AX.defaultRequest
        { url = letteraBreakingNewsUrl
                <> "?paper=" <> Paper.toString paper
                <> foldMap ("&cacheToken=" <> _) cacheToken
        , method = Left GET
        , responseFormat = AX.string
        }
  driver <- liftEffect getDriver
  useResponse (pure <<< pure) =<< AX.request driver request

parseArticleStubs :: Json -> Aff (Either LetteraError (Array ArticleStub))
parseArticleStubs response
  | Just (responseArray :: Array Json) <- toArray response =
      map (Right <<< takeRights) $ liftEffect $ traverse parseArticleStub responseArray
  | otherwise = pure $ Left ParseError

getFrontpage :: Paper -> Maybe Int -> Maybe Int -> Maybe String -> Maybe String -> Aff (LetteraResponse (Array ArticleStub))
getFrontpage paper start limit categoryId cacheToken = do
  let request = AX.defaultRequest
        { url = letteraFrontPageUrl
                <> "?paper=" <> Paper.toString paper
                <> foldMap (("&category=" <> _) <<< _encodeURIComponent ) categoryId
                <> foldMap (("&start=" <> _) <<< show) start
                <> foldMap (("&limit=" <> _) <<< show) limit
                <> foldMap ("&cacheToken=" <> _) cacheToken
        , method = Left GET
        , responseFormat = AX.json
        }
  driver <- liftEffect getDriver
  useResponse parseArticleStubs =<< AX.request driver request

getMostRead :: Int -> Int -> Maybe String -> Paper -> Boolean -> Aff (LetteraResponse (Array ArticleStub))
getMostRead start limit category paper onlySubscribers = do
  driver <- liftEffect getDriver
  useResponse parseArticleStubs =<< AX.get driver ResponseFormat.json (letteraMostReadUrl
          <> "?start=" <> show start
          <> "&limit=" <> show limit
          <> (foldMap (("&category=" <> _) <<< _encodeURIComponent) category)
          <> "&paper=" <> Paper.toString paper
          <> "&onlySubscribers=" <> show onlySubscribers
  )

getLatest :: Int -> Int -> Paper -> Aff (LetteraResponse (Array ArticleStub))
getLatest start limit paper = do
  driver <- liftEffect getDriver
  useResponse parseArticleStubs =<<
    AX.get driver ResponseFormat.json (letteraLatestUrl
                                <> "?start=" <> show start
                                <> "&limit=" <> show limit
                                <> "&paper=" <> Paper.toString paper
                              )

getByDay :: Date -> Paper -> Aff (LetteraResponse (Array ArticleStub))
getByDay date paper = do
  let url = letteraByDayUrl
            <> "/" <> show (fromEnum (year date))
            <> "/" <> show (fromEnum (month date))
            <> "/" <> show (fromEnum (day date))
            <> "?paper=" <> Paper.toString paper
  driver <- liftEffect getDriver
  useResponse parseArticleStubs =<<
    AX.get driver ResponseFormat.json url

getByTag :: Int -> Int -> Tag -> Paper -> Aff (LetteraResponse (Array ArticleStub))
getByTag start limit tag paper = do
  driver <- liftEffect getDriver
  useResponse parseArticleStubs =<<
    AX.get driver ResponseFormat.json (letteraTagUrl <> _encodeURIComponent(un Tag tag)
                                <> "?start=" <> show start
                                <> "&limit=" <> show limit
                                <> "&paper=" <> Paper.toString paper
                               )

search :: Int -> Int -> Paper -> String -> Aff (LetteraResponse (Array ArticleStub))
search start limit paper query = do
  driver <- liftEffect getDriver
  useResponse parseArticleStubs =<<
    AX.get driver ResponseFormat.json (letteraSearchUrl
                                       <> "?start=" <> show start
                                       <> "&limit=" <> show limit
                                       <> "&paper=" <> Paper.toString paper
                                       <> "&contentQuery=" <> _encodeURIComponent query)

getCategoryStructure :: Maybe Platform -> Paper -> Aff (Array Category)
getCategoryStructure platform p = do
  driver <- liftEffect getDriver
  categoriesRes <- AX.get driver ResponseFormat.json $ letteraCategoryUrl <> "?paper=" <> Paper.toString p <> platformQuery
  case categoriesRes of
    Right r
      | Just (responseArray :: Array Json) <- toArray r.body -> do
          let { no, yes: categories } = partition isRight $ map decodeJson responseArray
          traverse_ (Console.warn <<< ("Could not parse category: " <> _) <<< show) $ takeLefts no
          pure $ takeRights categories
      | otherwise -> do
        Console.warn "Expected category response to be an array, but got something else"
        pure mempty
    Left err -> do
      Console.warn $ "Error while getting categories: " <> AX.printError err
      pure mempty
  where
    platformQuery = case platform of
      Nothing      -> ""
      Just Mobile  -> "&platform=Mobile"
      Just Desktop -> "&platform=Desktop"

getAdvertorials :: Paper -> Aff (LetteraResponse (Array ArticleStub))
getAdvertorials paper = do
  driver <- liftEffect getDriver
  useResponse parseArticleStubs
    =<< AX.get driver ResponseFormat.json
        ( letteraAdvertorialUrl
            <> "?paper="
            <> Paper.toString paper
        )

data ArchiveRequest
  = MonthSelection
  | DateSelection Year Month
  | ArticleSelection Date Paper

getArchiveHtml :: ArchiveRequest -> Aff (LetteraResponse String)
getArchiveHtml params = do
  let showToken :: forall a . BoundedEnum a => Show a => a -> String
      showToken = ("/" <> _) <<< show <<< fromEnum
  response <- do
    driver <- liftEffect getDriver
    AX.get driver ResponseFormat.string $ case params of
      MonthSelection ->
        letteraArchiveUrl
      DateSelection year month ->
        letteraArchiveUrl
        <> showToken year
        <> showToken month
      ArticleSelection date paper ->
        letteraArchiveUrl
        <> showToken (year date)
        <> showToken (month date)
        <> showToken (day date)
        <> "?paper=" <> Paper.toString paper
  useResponse (pure <<< pure) response

takeRights :: forall a b. Array (Either b a) -> Array a
takeRights =
  let go acc = either (const acc) (acc `snoc` _)
  in foldl go []

takeLefts :: forall a b. Array (Either b a) -> Array b
takeLefts =
  let go acc = either (acc `snoc` _) (const acc)
  in foldl go []
