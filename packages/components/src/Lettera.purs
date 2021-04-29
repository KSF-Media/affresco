module Lettera where

import Prelude

import Affjax (defaultRequest, request, printError, get) as AX
import Affjax.RequestHeader (RequestHeader(..)) as AX
import Affjax.ResponseFormat (json) as AX
import Affjax.ResponseFormat as ResponseFormat
import Affjax.StatusCode (StatusCode(..))
import Data.Argonaut.Core (Json, stringify, toArray, toObject)
import Data.Array (foldM, snoc)
import Data.Either (Either(..), note)
import Data.HTTP.Method (Method(..))
import Data.Maybe (Maybe(..))
import Data.Traversable (traverse_)
import Data.UUID (UUID, toString)
import Data.UUID as UUID
import Effect (Effect)
import Effect.Aff (Aff, throwError)
import Effect.Class (liftEffect)
import Effect.Class.Console as Console
import Effect.Exception (error)
import Foreign (MultipleErrors, renderForeignError)
import Foreign.Object (lookup)
import KSF.Api (Token(..), UserAuth)
import KSF.Auth as Auth
import KSF.Paper (Paper)
import KSF.Paper as Paper
import Lettera.Models (Article, ArticleStub, FullArticle(..), JSArticleStub, fromJSArticleStub)
import Simple.JSON (E)
import Simple.JSON as JSON

foreign import letteraBaseUrl :: String

letteraArticleUrl :: String
letteraArticleUrl = letteraBaseUrl <>  "/article/"

letteraFrontPageUrl :: String
letteraFrontPageUrl = letteraBaseUrl <> "/frontpage"

getArticle' :: UUID -> Aff Article
getArticle' u = do
  getArticle u Nothing >>= \a ->
    case a of
      Right (FullArticle a') -> pure a'
      Right (PreviewArticle a') -> pure a'
      Left err -> throwError $ error $ "Failed to get article: " <> err

getArticleAuth :: UUID -> Aff (Either String FullArticle)
getArticleAuth articleId = do
  tokens <- Auth.loadToken
  getArticle articleId tokens

-- TODO: Instead of String, use some sort of LetteraError or something
getArticle :: UUID -> Maybe UserAuth -> Aff (Either String FullArticle)
getArticle articleId auth = do
  let request = AX.defaultRequest
        { url = letteraArticleUrl <> (toString articleId)
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
      | (StatusCode 200) <- response.status -> do
        let s = stringify response.body
        case JSON.readJSON s of
          Right r -> pure $ Right $ FullArticle r
          Left err -> do
            liftEffect $ traverse_ (Console.log <<< show) err
            pure $ Left "parsing error"
      | (StatusCode 403) <- response.status ->
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
          (toObject response.body)
            >>= lookup "not_entitled"
            >>= toObject
            >>= lookup "articlePreview"
            >>= (stringify >>> parseArticle)
            # map PreviewArticle
            # note "preview article parsing error" >>> pure
      | otherwise -> pure $ Left "Unexpected HTTP status"

parseArticle :: String -> (Maybe Article)
parseArticle a =
  case JSON.readJSON a of
    Right (r :: Article) -> Just r
    Left e -> Nothing

getFrontpage :: Paper -> Aff (Either String (Array ArticleStub))
getFrontpage paper = do
  frontpageResponse <- AX.get ResponseFormat.json (letteraFrontPageUrl <> "?paper=" <> Paper.toString paper)
  case frontpageResponse of
    Left err -> pure $ Left $ "Article GET response failed to decode: " <> AX.printError err
    Right response
      | Just (responseArray :: Array Json) <- toArray response.body -> do
        let (s :: Array (E JSArticleStub)) = map (JSON.readJSON <<< stringify) responseArray
        articleStubs <- liftEffect $ foldM parseArticleStub [] s
        pure $ Right articleStubs
        where
          parseArticleStub :: Array ArticleStub -> Either MultipleErrors JSArticleStub -> Effect (Array ArticleStub)
          parseArticleStub acc (Left err) = do
            traverse_ (Console.log <<< renderForeignError) err
            pure acc
          parseArticleStub acc (Right jsArticleStub)
            | Just articleStub <- fromJSArticleStub jsArticleStub = pure $ acc `snoc` articleStub
            | otherwise = do
              let articleId = jsArticleStub.uuid
              -- TODO: Sentry etc.
              Console.log $ "Could not parse Lettera response to ArticleStub. Article id: " <> articleId
              pure acc

          logParsingErrors :: Array JSArticleStub -> Either MultipleErrors JSArticleStub -> Effect (Array JSArticleStub)
          logParsingErrors acc (Left err) = do
            traverse_ (Console.log <<< renderForeignError) err
            pure acc
          logParsingErrors acc (Right articleStub) = pure $ acc `snoc` articleStub
      | otherwise -> pure $ Left "Got weird response"
