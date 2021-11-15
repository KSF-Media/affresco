module Mosaico.StaticPage where

import Prelude

import Affjax (get) as AX
import Affjax.ResponseFormat (string) as AX
import Affjax.StatusCode (StatusCode(..))
import Data.Either (Either(..))

data StaticPageResponse
  = StaticPageResponse StaticPage
  | StaticPageNotFound
  | StaticPageOtherError

type StaticPage =
  { pageName :: String
  , pageContent :: String
  }

fetchStaticPage :: String -> Aff StaticPageResponse
fetchStaticPage pageName = do
  let staticPageUrl = "/assets/" <> pageName <> ".html"
  res <- AX.get AX.string staticPageUrl
  pure case res of
    Right pageContentResponse ->
      case pageContentResponse.status of
        StatusCode 200 -> StaticPageResponse { pageName, pageContent: pageContentResponse.body }
        StatusCode 404 -> StaticPageNotFound
        _ -> StaticPageOtherError
    Left _err -> StaticPageOtherError




