module Lettera.Fallback where

import Prelude

import Affjax (Response, defaultRequest, request, printError) as AX
import Affjax.RequestHeader (RequestHeader(..))
import Affjax.RequestBody as AX.RequestBody
import Affjax.ResponseFormat (json) as AX
import Affjax.StatusCode (StatusCode(..))
import Data.Either (Either(..), either)
import Data.Formatter.DateTime (format)
import Data.HTTP.Method (Method(..))
import Data.Maybe (Maybe(..), maybe)
import Data.UUID (UUID)
import Data.UUID as UUID
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import KSF.Driver (getDriver)
import KSF.Helpers (dateTimeFormatter)
import KSF.Paper (Paper)
import KSF.Paper as Paper
import Lettera (letteraFallbackUrl, parseArticlesWith)
import Lettera.Models (parseArticle)
import Lettera.Models as Lettera

data FallbackError = PermissionDenied | FallbackError String

getFallbackArticles :: String -> Int -> Int -> Paper -> Aff (Either FallbackError (Array Lettera.Article))
getFallbackArticles token start limit paper = do
  let request = AX.defaultRequest
        { url = letteraFallbackUrl
                <> "?start=" <> show start
                <> "&limit=" <> show limit
                <> "&paper=" <> Paper.toString paper
        , method = Left GET
        , responseFormat = AX.json
        , headers = [ RequestHeader "Authorization" ("Bearer " <> token) ]
        }
  driver <- liftEffect getDriver
  --useResponse (parseArticlesWith parseArticle) =<<
  articlesResponse <- AX.request driver request
  case articlesResponse of
    Left err -> pure $ Left $ FallbackError $ "Fallback article list decode error: " <> AX.printError err
    Right response -> liftEffect $ handleFallbackResponse (map showLeft <<< parseArticlesWith parseArticle) response
  where
    showLeft = either (Left <<< show) Right

handleFallbackResponse :: forall a b . (a -> Effect (Either String b)) -> AX.Response a -> Effect (Either FallbackError b)
handleFallbackResponse f response | (StatusCode 200) <- response.status = either (Left <<< FallbackError) Right <$> f response.body
handleFallbackResponse _ response | (StatusCode 403) <- response.status = pure $ Left PermissionDenied
handleFallbackResponse _ response | (StatusCode n) <- response.status = pure $ Left $ FallbackError $ "Unexpeted response status " <> show n

getSingleFallbackArticle :: String -> UUID -> Aff (Either FallbackError Lettera.Article)
getSingleFallbackArticle token uuid = do
  let request = AX.defaultRequest
        { url = letteraFallbackUrl <> "/" <> UUID.toString uuid
        , method = Left GET
        , responseFormat = AX.json
        , headers = [ RequestHeader "Authorization" ("Bearer " <> token) ]
        }
  driver <- liftEffect getDriver
  articleResponse <- AX.request driver request
  case articleResponse of
    Left err -> pure $ Left $ FallbackError $ "Fallback article decode error: " <> AX.printError err
    Right response -> liftEffect $ handleFallbackResponse parseArticle response

upsertFallbackArticle :: Method -> String -> Lettera.Article -> Aff (Either FallbackError Lettera.Article)
upsertFallbackArticle method token article = do
  let request = AX.defaultRequest
        { url = url
        , method = Left method
        , responseFormat = AX.json
        , headers = [ RequestHeader "Authorization" ("Bearer " <> token) ]
        -- Lettera expects UTC
        , content = Just $ AX.RequestBody.Json $ Lettera.articleToJsonWith localToUTCStamp article
        }
  driver <- liftEffect getDriver
  handleResponse =<< AX.request driver request
  where
    url = case method of
      PUT -> letteraFallbackUrl <> "/" <> article.uuid
      _ -> letteraFallbackUrl
    localToUTCStamp =
      maybe "" (format dateTimeFormatter) <<< Lettera.fromLocal
    handleResponse (Left err) = pure $ Left $ FallbackError $ "Response decode error: " <> AX.printError err
    handleResponse (Right response) = liftEffect $ handleFallbackResponse parseArticle response

createFallbackArticle :: String -> Lettera.Article -> Aff (Either FallbackError Lettera.Article)
createFallbackArticle = upsertFallbackArticle POST

updateFallbackArticle :: String -> Lettera.Article -> Aff (Either FallbackError Lettera.Article)
updateFallbackArticle = upsertFallbackArticle PUT

data FrontpagePurgeMode = Purge | Reenable

prerenderedCachePurge :: String -> FrontpagePurgeMode -> Aff (Either FallbackError Unit)
prerenderedCachePurge token purge = do
  driver <- liftEffect getDriver
  let request = AX.defaultRequest
        { url = letteraFallbackUrl <> "/onlyLists"
        , method = Left $ case purge of
            Purge -> PUT
            Reenable -> DELETE
        , headers = [ RequestHeader "Authorization" ("Bearer " <> token) ]
        }
  cacheResponse <- AX.request driver request
  case cacheResponse of
    -- Impossible case
    Left _ -> pure $ Left $ FallbackError "Parse error"
    Right response -> liftEffect $ handleFallbackResponse (const $ pure $ Right unit) response

