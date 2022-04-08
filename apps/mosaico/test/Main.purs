module Test.Main where

import Prelude

import Data.Maybe (Maybe(..), fromMaybe)
import Effect (Effect)
import Effect.Aff (Aff, bracket, launchAff_)
import Effect.Class.Console (log)
import Mosaico.Test.Account as Account
import Mosaico.Test.Article as Article
import Mosaico.Test.Embeds as Embeds
import Mosaico.Test.Frontpage as Frontpage
import Mosaico.Test.Layout as Layout
import Mosaico.Test.Lettera as Lettera
import Mosaico.Test.Search as Search
import Mosaico.Test.Static as Static
import Mosaico.Test.Tags as Tags
import KSF.Puppeteer as Chrome


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
    else do
    log "Test login and logout"
    withBrowserPage $ Account.loginLogout testUser testPassword
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
        withDesktopBrowserPage $
          Article.testPaywallLogin false uuid entitledUser entitledPassword Article.testPaywallOpen
      _ -> log "Skip paywall open test via navigation"
    log "Test paywall opens, direct"
    withBrowserPage $ Article.testPaywallLogin true premiumUuid entitledUser entitledPassword Article.testPaywallOpen

  log "Test related article links"
  withBrowserPage Article.testRelated

  log "Test CSS has loaded"
  withBrowserPage Layout.testLayout
  log "Test front page embedded HTML"
  withBrowserPage Frontpage.testHtmlEmbed
  withBrowserPage Frontpage.testHtmlEmbedNavigation
  log "Test most read list"
  withDesktopBrowserPage $ Frontpage.testMostRead
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
  log "Test tag list"
  withBrowserPage Tags.testTagList
  log "Test static pages"
  withBrowserPage Static.testNavigateToStatic
  withBrowserPage Static.testStaticEmbeds
  log "Test listTitle field"
  withBrowserPage Lettera.testListTitle
  withBrowserPage Lettera.testDefaultListTitle
  log "Test categories"
  withBrowserPage Lettera.testCategoryLists
  where
    withBrowser :: forall a. Aff Chrome.Browser -> (Chrome.Browser -> Aff a) -> Aff a
    withBrowser = flip bracket Chrome.close

    withBrowserPage :: forall a. (Chrome.Page -> Aff a) -> Aff a
    withBrowserPage f = withBrowser Chrome.launch (f <=< Chrome.newPage)

    withDesktopBrowserPage :: forall a. (Chrome.Page -> Aff a) -> Aff a
    withDesktopBrowserPage f = withBrowser Chrome.launchDesktop (f <=< Chrome.newPage)
