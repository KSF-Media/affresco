module Mosaico.StaticPage where

import Prelude

import Affjax (get) as AX
import Affjax.ResponseFormat (string) as AX
import Affjax.StatusCode (StatusCode(..))
import Data.Either (Either(..))
import Effect.Aff (Aff)
import Web.HTML.Event.EventTypes (offline)

data StaticPageResponse
  = StaticPageResponse StaticPage
  | StaticPageNotFound
  | StaticPageOtherError

type StaticPage =
  { pageName :: String
  , pageContent :: String
  , pageScript :: String
  }

fetchStaticPage :: String -> Aff StaticPageResponse
fetchStaticPage pageName = do
  let staticPageUrl = "/assets/" <> pageName <> ".html"
  let staticPageJsUrl = "/assets/" <> pageName <> ".js"
  resPage <- AX.get AX.string staticPageUrl
  resJs <- AX.get AX.string staticPageJsUrl
  let jsContent eitherJs =
          case eitherJs of
            Right r ->
              case r.status of
                StatusCode 200 -> r.body
                _ -> mempty
            Left _ -> mempty
  pure case resPage of
    Right pageContentResponse ->
      case pageContentResponse.status of
        StatusCode 200 -> StaticPageResponse { pageName, pageContent: pageContentResponse.body, pageScript: jsContent resJs }
        StatusCode 404 -> StaticPageNotFound
        _ -> StaticPageOtherError
    Left _err -> StaticPageOtherError




