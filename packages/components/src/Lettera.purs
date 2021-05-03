module Lettera where

import Prelude

import Affjax (defaultRequest, request, printError, get) as AX
import Affjax.RequestHeader (RequestHeader(..)) as AX
import Affjax.ResponseFormat (json) as AX
import Affjax.ResponseFormat as ResponseFormat
import Affjax.StatusCode (StatusCode(..))
import Control.Monad.Except (runExcept)
import Control.Monad.Except.Trans (except, runExceptT)
import Data.Argonaut.Core (Json, stringify, toArray, toObject)
import Data.Array (foldM, foldl, fromFoldable, partition, snoc)
import Data.Either (Either(..), either, hush, isLeft, note)
import Data.HTTP.Method (Method(..))
import Data.List.NonEmpty (toList)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String (joinWith)
import Data.Traversable (traverse, traverse_)
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
import KSF.Helpers as Helpers
import KSF.Paper (Paper)
import KSF.Paper as Paper
import Lettera.Models (Article, ArticleStub, FullArticle(..), JSArticle, JSArticleStub, fromJSArticle, fromJSArticleStub, parseArticle, parseArticleStub)
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
        let articlePreviewJson =
              (toObject response.body)
                 >>= lookup "not_entitled"
                 >>= toObject
                 >>= lookup "articlePreview"

        case articlePreviewJson of
          Just articlePreview -> do
            either Left (Right <<< PreviewArticle) <$> (liftEffect $ parseArticle articlePreview)
          Nothing -> do
            -- TODO: Sentry and whatnot
            Console.warn "Did not find article preview from response!"
            pure $ Left "Parsing error"

      | otherwise -> pure $ Left "Unexpected HTTP status"

getFrontpage :: Paper -> Aff (Array ArticleStub)
getFrontpage paper = do
  frontpageResponse <- AX.get ResponseFormat.json (letteraFrontPageUrl <> "?paper=" <> Paper.toString paper)
  case frontpageResponse of
    Left err -> do
      Console.warn $ "Frontpage response failed to decode: " <> AX.printError err
      pure mempty
    Right response
      | Just (responseArray :: Array Json) <- toArray response.body -> do
        a <- liftEffect $ traverse parseArticleStub responseArray
        pure $ foldl takeRights [] a
        where
          takeRights acc eitherArticle
            | Right article <- eitherArticle = acc `snoc` article
            | otherwise = acc

      | otherwise -> do
        Console.warn "Failed to read API response!"
        pure mempty
