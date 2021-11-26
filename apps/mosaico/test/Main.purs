module Test.Main where

import Prelude

import Data.Maybe (Maybe(..), fromMaybe)
import Effect (Effect)
import Effect.Aff (Aff, bracket, launchAff_)
import Effect.Class.Console (log)
import Puppeteer as Chrome

import Mosaico.Test.Article as Test

foreign import testUser :: String
foreign import testPassword :: String
foreign import entitledUser :: String
foreign import entitledPassword :: String

defaultArticleId :: String
defaultArticleId = "df6e4abe-a43c-4a75-af04-52a09eb5e335"

defaultPremiumArticleId :: String
defaultPremiumArticleId = "cf100445-d2d8-418a-b190-79d0937bf7fe"

main :: Effect Unit
main = launchAff_ do
  { articleId, premiumArticleId } <- withBrowserPage Test.testFrontPage
  let premiumUuid = fromMaybe defaultPremiumArticleId premiumArticleId

  withBrowserPage $ Test.testFreeArticle (fromMaybe defaultArticleId articleId)

  if testUser == "" || testPassword == ""
    then log "skip unentitled paywall test, user or password not set"
    else do
    case premiumArticleId of
      Just uuid -> withBrowserPage $
        Test.testPaywallLogin false uuid testUser testPassword Test.testPaywallHolds
      _ -> log "Skip paywall hold test via navigation"
  withBrowserPage $ Test.testPaywallLogin true premiumUuid testUser testPassword Test.testPaywallHolds

  if entitledUser == "" || entitledPassword == ""
    then log "skip entitled paywall test, user or password not set"
    else do
    case premiumArticleId of
      Just uuid -> withBrowserPage $
        Test.testPaywallLogin false uuid entitledUser entitledPassword Test.testPaywallOpen
      _ -> log "Skip paywall open test via navigation"
  withBrowserPage $ Test.testPaywallLogin true premiumUuid entitledUser entitledPassword Test.testPaywallOpen
  where
    withBrowser :: forall a. (Chrome.Browser -> Aff a) -> Aff a
    withBrowser = bracket Chrome.launch Chrome.close

    withBrowserPage :: forall a. (Chrome.Page -> Aff a) -> Aff a
    withBrowserPage f = withBrowser (f <=< Chrome.newPage)
