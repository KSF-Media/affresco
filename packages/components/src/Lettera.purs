module Lettera where

import Prelude

import Affjax as AX
import Affjax.ResponseFormat as ResponseFormat
import Affjax.StatusCode (StatusCode(..))
import Data.Argonaut.Core (stringify, toArray, toObject)
import Data.Array (foldM, snoc)
import Data.Either (Either(..), note)
import Data.Maybe (Maybe(..))
import Data.Traversable (traverse_)
import Data.UUID (UUID, toString)
import Effect (Effect)
import Effect.Aff (Aff, throwError)
import Effect.Class (liftEffect)
import Effect.Class.Console as Console
import Effect.Exception (error)
import Foreign (MultipleErrors, renderForeignError)
import Foreign.Object (lookup)
import KSF.Paper (Paper)
import KSF.Paper as Paper
import Lettera.Models (Article, ArticleStub, FullArticle(..))
import Simple.JSON as JSON

letteraArticleUrl :: String
letteraArticleUrl = "https://lettera.api.ksfmedia.fi/v3/article/"

letteraFrontPageUrl :: String
letteraFrontPageUrl = "https://lettera.api.ksfmedia.fi/v3/frontpage"

getArticle' :: UUID -> Aff Article
getArticle' u = do
  getArticle u >>= \a ->
    case a of
      Right (FullArticle a') -> pure  a'
      Right (PreviewArticle a') -> pure a'
      Left err -> throwError $ error $ "Failed to get article: " <> err

-- TODO: Instead of String, use some sort of LetteraError or something
getArticle :: UUID -> Aff (Either String FullArticle)
getArticle articleId = do
  -- TODO: Add authentication
  articleResponse <- AX.get ResponseFormat.json (letteraArticleUrl <> (toString articleId))
  case articleResponse of
    Left err -> pure $ Left $ "Article GET response failed to decode: " <> AX.printError err
    Right response
      | (StatusCode 200) <- response.status -> do
        response.body
          # stringify
          # map FullArticle <<< parseArticle
          # note "Article parsing error"
          # pure
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
      | Just responseArray <- toArray response.body -> do
        map (JSON.readJSON <<< stringify) responseArray
          # (liftEffect <<< foldM logParsingErrors [])
          >>= (Right >>> pure)
        where
          logParsingErrors :: Array ArticleStub -> Either MultipleErrors ArticleStub -> Effect (Array ArticleStub)
          logParsingErrors acc (Left err) = do
            traverse_ (Console.log <<< renderForeignError) err
            pure acc
          logParsingErrors acc (Right articleStub) = pure $ acc `snoc` articleStub
      | otherwise -> pure $ Left "Got weird response"
