module Lettera where

import Data.UUID (UUID, parseUUID, toString)
import Effect.Aff (Aff)
import Foreign (unsafeToForeign)
import Lettera.Models (Article)
import OpenApiClient (Api, callApi)

foreign import articlesApi :: Api

getArticle :: UUID -> Aff Article
getArticle articleId =
  callApi articlesApi "articleUuidGet" [ unsafeToForeign articleId ] {}
