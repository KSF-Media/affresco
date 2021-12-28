module Test.Main where

import Prelude

import Data.Maybe (Maybe(..), fromMaybe)
import Effect (Effect)
import Effect.Aff (Aff, bracket, launchAff_)
import Effect.Class.Console (log)
import Puppeteer as Chrome

import Mosaico.Test.Account as Account
import Mosaico.Test.Article as Article
import Mosaico.Test.Embeds as Embeds
import Mosaico.Test.Search as Search
import Mosaico.Test.Tags as Tags

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
  if testUser == "" || testPassword == ""
    then log "skip login and logout test, user or password not set"
    else withBrowserPage $ Account.loginLogout testUser testPassword
  log "Test news page and get free article and premium article"
  { articleId, premiumArticleId } <- withBrowserPage Article.testNewsPage
  log $ "Free article " <> show articleId <> " premium " <> show premiumArticleId
  let premiumUuid = fromMaybe defaultPremiumArticleId premiumArticleId
  log $ "Using premium " <> premiumUuid

  log "Test free article"
  withBrowserPage $ Article.testFreeArticle (fromMaybe defaultArticleId articleId)

  if testUser == "" || testPassword == ""
    then log "skip unentitled paywall test, user or password not set"
    else do
    case premiumArticleId of
      Just uuid -> do
        log "Test paywall holds, navigation"
        withBrowserPage $
          Article.testPaywallLogin false uuid testUser testPassword Article.testPaywallHolds
      _ -> log "Skip paywall hold test via navigation"
    log "Test paywall holds, direct"
    withBrowserPage $ Article.testPaywallLogin true premiumUuid testUser testPassword Article.testPaywallHolds

  if entitledUser == "" || entitledPassword == ""
    then log "skip entitled paywall test, user or password not set"
    else do
    case premiumArticleId of
      Just uuid -> do
        log "Test paywall opens, navigation"
        withBrowserPage $
          Article.testPaywallLogin false uuid entitledUser entitledPassword Article.testPaywallOpen
      _ -> log "Skip paywall open test via navigation"
    log "Test paywall opens, direct"
    withBrowserPage $ Article.testPaywallLogin true premiumUuid entitledUser entitledPassword Article.testPaywallOpen

  log "Test most read list"
  withBrowserPage $ Article.testMostRead false
  log "Test embed render via navigation"
  withBrowserPage Embeds.testEmbedNavigation
  log "Test embed render, direct"
  withBrowserPage Embeds.testEmbedServerRender
  log "Test search via navigation"
  withBrowserPage Search.testSearchNavigation
  log "Test search direct"
  withBrowserPage Search.testSearchServerRender
  log "Search with a not found search word"
  withBrowserPage Search.testFailingSearch
  log "Test tagless article, navigation"
  withBrowserPage Tags.testTaglessArticleNavigation
  log "Test tagless article, direct"
  withBrowserPage Tags.testTaglessArticleServerRender
  log "Test tag list"
  withBrowserPage Tags.testTagList
  where
    withBrowser :: forall a. (Chrome.Browser -> Aff a) -> Aff a
    withBrowser = bracket Chrome.launch Chrome.close

    withBrowserPage :: forall a. (Chrome.Page -> Aff a) -> Aff a
    withBrowserPage f = withBrowser (f <=< Chrome.newPage)
