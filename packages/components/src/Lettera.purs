module Lettera where

import Prelude

import Affjax (Error, Response, defaultRequest, request, printError, get) as AX
import Affjax.RequestHeader (RequestHeader(..)) as AX
import Affjax.ResponseFormat (json) as AX
import Affjax.ResponseFormat as ResponseFormat
import Affjax.StatusCode (StatusCode(..))
import Data.Argonaut.Core (Json, toArray, toObject)
import Data.Argonaut.Decode (decodeJson)
import Data.Array (foldl, partition, snoc)
import Data.Either (Either(..), either, hush, isRight)
import Data.Foldable (class Foldable, foldMap)
import Data.Foldable as Foldable
import Data.HTTP.Method (Method(..))
import Data.Maybe (Maybe(..))
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
import KSF.Paper (Paper)
import KSF.Paper as Paper
import Lettera.Models (ArticleStub, Category, DraftParams, FullArticle(..), Tag(..), parseArticle, parseArticleStub, parseDraftArticle)
import Lettera.Header as Cache

foreign import letteraBaseUrl :: String
foreign import encodeURIComponent :: String -> String

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

letteraMostReadUrl :: String
letteraMostReadUrl = letteraBaseUrl <> "/list/mostread/"

letteraCategoryUrl :: String
letteraCategoryUrl = letteraBaseUrl <> "/categories"

letteraTagUrl :: String
letteraTagUrl = letteraBaseUrl <> "/list/tag/"

letteraSearchUrl :: String
letteraSearchUrl = letteraBaseUrl <> "/list/search"

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
  getArticle articleId paper tokens

-- TODO: Instead of String, use some sort of LetteraError or something
getArticle :: UUID -> Paper -> Maybe UserAuth -> Aff (Either String FullArticle)
getArticle articleId = getArticleWithUrl (letteraArticleUrl <> (toString articleId)) <<< Just

getArticleWithSlug :: String -> Maybe UserAuth -> Aff (Either String FullArticle)
getArticleWithSlug slug = getArticleWithUrl (letteraArticleSlugUrl <> slug) Nothing

getArticleWithUrl :: String -> Maybe Paper -> Maybe UserAuth -> Aff (Either String FullArticle)
getArticleWithUrl url paper auth = do
  let request = AX.defaultRequest
        { url = url <> foldMap (\p -> "?paper=" <> Paper.toString p) paper
        , method = Left GET
        , responseFormat = AX.json
        , headers =
            case auth of
              Just { userId, authToken: Token authToken } ->
                [ AX.RequestHeader "AuthUser" $ UUID.toString userId
                , AX.RequestHeader "Authorization" ("OAuth " <> authToken)
                ]
              _ -> mempty
        }
  articleResponse <- AX.request request
  case articleResponse of
    Left err -> pure $ Left $ "Article GET response failed to decode: " <> AX.printError err
    Right response
      | (StatusCode 200) <- response.status ->
        either Left (Right <<< FullArticle) <$> (liftEffect $ parseArticle response.body)
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
            map PreviewArticle <$> (liftEffect $ parseArticle articlePreview)
          Nothing -> do
            -- TODO: Sentry and whatnot
            Console.warn "Did not find article preview from response!"
            pure $ Left "Parsing error"
      | (StatusCode s) <- response.status -> pure $ Left $ "Unexpected HTTP status: " <> show s

getArticleStub :: UUID -> Aff (Either String ArticleStub)
getArticleStub uuid = do
  articleResponse <- AX.get ResponseFormat.json $ letteraArticleUrl <> toString uuid <> "/stub"
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
  articleResponse <- AX.request request
  case articleResponse of
    Left err -> pure $ Left $ "Article GET response failed to decode: " <> AX.printError err
    Right response
      | (StatusCode 200) <- response.status ->
        map DraftArticle <$> (liftEffect $ parseDraftArticle response.body)
      | (StatusCode 403) <- response.status ->
        pure $ Left "Unauthorized"
      | (StatusCode s) <- response.status -> pure $ Left $ "Unexpected HTTP status: " <> show s

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

getFrontpageHtml :: Paper -> String -> Aff (LetteraResponse String)
getFrontpageHtml paper category = do
  let request = letteraFrontPageHtmlUrl <> "?paper=" <> Paper.toString paper  <> "&category=" <> category
  useResponse (pure <<< pure) =<< AX.get ResponseFormat.string request

parseArticleStubs :: Json -> Aff (Either LetteraError (Array ArticleStub))
parseArticleStubs response
  | Just (responseArray :: Array Json) <- toArray response =
      map (Right <<< takeRights) $ liftEffect $ traverse parseArticleStub responseArray
  | otherwise = pure $ Left ParseError

getFrontpage :: Paper -> Maybe String -> Aff (LetteraResponse (Array ArticleStub))
getFrontpage paper categoryId = do
  let letteraUrl =
        letteraFrontPageUrl
        <> "?paper=" <> Paper.toString paper
        <> foldMap ("&category=" <> _) categoryId
  useResponse parseArticleStubs =<< AX.get ResponseFormat.json letteraUrl

getMostRead :: Int -> Int -> Maybe String -> Paper -> Boolean -> Aff (LetteraResponse (Array ArticleStub))
getMostRead start limit category paper onlySubscribers =
  useResponse parseArticleStubs =<< AX.get ResponseFormat.json (letteraMostReadUrl
          <> "?start=" <> show start
          <> "&limit=" <> show limit
          <> (foldMap ("&category=" <> _) category)
          <> "&paper=" <> Paper.toString paper
          <> "&onlySubscribers=" <> show onlySubscribers
  )

getByTag :: Int -> Int -> Tag -> Paper -> Aff (LetteraResponse (Array ArticleStub))
getByTag start limit tag paper = do
  useResponse parseArticleStubs =<<
    AX.get ResponseFormat.json (letteraTagUrl <> encodeURIComponent(un Tag tag)
                                <> "?start=" <> show start
                                <> "&limit=" <> show limit
                                <> "&paper=" <> Paper.toString paper
                               )

search :: Int -> Int -> Paper -> String -> Aff (Array ArticleStub)
search start limit paper query = do
  searchResponse <- AX.get ResponseFormat.json (letteraSearchUrl
                                                <> "?start=" <> show start
                                                <> "&limit=" <> show limit
                                                <> "&paper=" <> Paper.toString paper
                                                <> "&contentQuery=" <> encodeURIComponent query)
  case searchResponse of
    Left err -> do
      Console.warn $ "Search response failed to decode: " <> AX.printError err
      pure mempty
    Right response
      | Just (responseArray :: Array Json) <- toArray response.body -> do
        liftEffect $ takeRights <$> traverse parseArticleStub responseArray
      | otherwise -> do
        Console.warn "Failed to read API reponse!"
        pure mempty

getCategoryStructure :: Paper -> Aff (Array Category)
getCategoryStructure p = do
  categoriesRes <- AX.get ResponseFormat.json $ letteraCategoryUrl <> "?paper=" <> Paper.toString p
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

takeRights :: forall a b. Array (Either b a) -> Array a
takeRights =
  let go acc = either (const acc) (acc `snoc` _)
  in foldl go []

takeLefts :: forall a b. Array (Either b a) -> Array b
takeLefts =
  let go acc = either (acc `snoc` _) (const acc)
  in foldl go []
