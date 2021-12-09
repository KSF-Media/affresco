module Mosaico.StaticPage where

import Prelude

import Affjax (get) as AX
import Affjax.ResponseFormat (string) as AX
import Affjax.StatusCode (StatusCode(..))
import Data.Either (Either(..), hush)
import Data.Maybe (Maybe(..))
import Data.Monoid (guard)
import Effect.Aff (Aff)

data StaticPageResponse
  = StaticPageResponse StaticPage
  | StaticPageNotFound
  | StaticPageOtherError

type StaticPage =
  { pageName :: String
  , pageContent :: String
  , pageScript :: Maybe String
  }

fetchStaticPage :: String -> Aff StaticPageResponse
fetchStaticPage pageName = do
  let staticPageUrl = "/assets/" <> pageName <> ".html"
  let staticPageJsUrl = "/assets/" <> pageName <> ".js"
  resPage <- AX.get AX.string staticPageUrl
  resJs <- AX.get AX.string staticPageJsUrl
  let jsContent eitherJs = do
          jsRes <- hush eitherJs
          guard (jsRes.status == StatusCode 200) Just jsRes.body
  pure case resPage of
    Right pageContentResponse ->
      case pageContentResponse.status of
        StatusCode 200 -> StaticPageResponse { pageName, pageContent: pageContentResponse.body, pageScript: jsContent resJs }
        StatusCode 404 -> StaticPageNotFound
        _ -> StaticPageOtherError
    Left _err -> StaticPageOtherError




