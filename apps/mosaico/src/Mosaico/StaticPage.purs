module Mosaico.StaticPage where

import Prelude

import Affjax (get) as AX
import Affjax.ResponseFormat (string) as AX
import Affjax.StatusCode (StatusCode(..))
import Effect.Aff (Aff)
import Data.Either (Either(..))

data StaticPageResponse
  = StaticPageResponse String
  | StaticPageNotFound
  | StaticPageOtherError
  
fetchStaticPage :: String -> Aff StaticPageResponse
fetchStaticPage pageName = do
  let staticPageUrl = "https://cdn.ksfmedia.fi/mosaico/static/" <> pageName <> ".html"
  res <- AX.get AX.string staticPageUrl
  pure $ case res of 
    Right pageContentResponse ->
      case pageContentResponse.status of
        StatusCode 200 -> StaticPageResponse pageContentResponse.body
        StatusCode 404 -> StaticPageNotFound
        _ -> StaticPageOtherError
    Left _err ->  StaticPageOtherError
  


