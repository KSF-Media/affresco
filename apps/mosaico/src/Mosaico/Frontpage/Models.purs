module Mosaico.Frontpage.Models where

import Prelude

import Data.Array (head)
import Data.Maybe (Maybe(..), isJust, maybe)
import Data.Set (Set)
import Data.Set as Set
import Data.String (Pattern(..), contains, stripPrefix)
import KSF.Api (UserAuth, Token(..))
import KSF.HtmlRenderer.Models as HtmlRenderer
import KSF.Paper (Paper(..))
import KSF.Paper as Paper
import Lettera.Models (ArticleStub)
import Mosaico.Ad (ad) as Mosaico
import Mosaico.EpaperBanner as EpaperBanner
import Mosaico.MostReadList as MostReadList
import Mosaico.LatestList as LatestList
import React.Basic.Events (EventHandler)
import React.Basic.DOM as DOM

data Hook
  = MostRead (Array ArticleStub) (ArticleStub -> EventHandler)
  | Latest (Array ArticleStub) (ArticleStub -> EventHandler)
  | Ad String String
  | ArticleUrltoRelative
  | EpaperBanner
  | RemoveTooltips

toHookRep :: Hook -> HtmlRenderer.HookRep
toHookRep (MostRead articles onClickHandler) = mostReadHook { articles, onClickHandler }
toHookRep (Latest articles onClickHandler)   = latestHook { articles, onClickHandler }
toHookRep ArticleUrltoRelative               = articleUrltoRelativeHook
toHookRep RemoveTooltips                     = removeTooltipsHook
toHookRep EpaperBanner                       = epaperBannerHook
toHookRep (Ad placeholderText targetId)      = adHook { placeholderText, targetId }

mostReadHook
  :: { articles :: Array ArticleStub
     , onClickHandler :: ArticleStub -> EventHandler
     }
     -> HtmlRenderer.HookRep
mostReadHook { articles, onClickHandler } = HtmlRenderer.replacingHook
  { shouldProcessNode: (\n ->
                          let info = do
                                name      <- HtmlRenderer.getName n
                                attribs   <- HtmlRenderer.getAttribs n
                                className <- attribs.class
                                children  <- HtmlRenderer.getChildren n
                                textChild <- head children
                                text      <- HtmlRenderer.getData textChild
                                pure { name, className, text }
                          in case info of
                            Just { name, className, text }
                              | name      == "div"
                              , className == "dre-item__title"
                              , text      == "Andra läser DESKTOP" -> true
                            _                                      -> false
                       )
  , processNode: (\_ _ _ -> pure $ MostReadList.render
                                     { mostReadArticles: articles
                                     , onClickHandler
                                     }
                 )
  }

latestHook
  :: { articles :: Array ArticleStub
     , onClickHandler :: ArticleStub -> EventHandler
     }
     -> HtmlRenderer.HookRep
latestHook { articles, onClickHandler } = HtmlRenderer.replacingHook
  { shouldProcessNode: (\n ->
                          let info = do
                                name      <- HtmlRenderer.getName n
                                attribs   <- HtmlRenderer.getAttribs n
                                className <- attribs.class
                                children  <- HtmlRenderer.getChildren n
                                textChild <- head children
                                text      <- HtmlRenderer.getData textChild
                                pure { name, className, text }
                          in case info of
                            Just { name, className, text }
                              | name      == "div"
                              , className == "dre-item__title"
                              , text      == "Senaste nytt DESKTOP" -> true
                            _                                      -> false
                       )
  , processNode: (\_ _ _ -> pure $ LatestList.render
                                     { latestArticles: articles
                                     , onClickHandler
                                     }
                 )
  }

adHook
  :: { placeholderText :: String
     , targetId :: String
     }
     -> HtmlRenderer.HookRep
adHook { placeholderText, targetId } = HtmlRenderer.replacingHook
  { shouldProcessNode: (\n ->
                          let info = do
                                name      <- HtmlRenderer.getName n
                                attribs   <- HtmlRenderer.getAttribs n
                                className <- attribs.class
                                children  <- HtmlRenderer.getChildren n
                                textChild <- head children
                                text      <- HtmlRenderer.getData textChild
                                pure { name, className, text }
                          in case info of
                            Just { name, className, text }
                              | name      == "div"
                              , className == "dre-item__title"
                              , text      == placeholderText -> true
                            _                                   -> false
                       )
  , processNode: (\_ _ _ -> pure $ Mosaico.ad { contentUnit: targetId }
                 )
  }

epaperBannerHook :: HtmlRenderer.HookRep
epaperBannerHook = HtmlRenderer.replacingHook
  { shouldProcessNode: (\n ->
                          let info = do
                                name      <- HtmlRenderer.getName n
                                attribs   <- HtmlRenderer.getAttribs n
                                className <- attribs.class
                                children  <- HtmlRenderer.getChildren n
                                textChild <- head children
                                text      <- HtmlRenderer.getData textChild
                                pure { name, className, text }
                          in case info of
                            Just { name, className, text }
                              | name      == "div"
                              , className == "dre-item__title"
                              , text      == "Senaste tidning" -> true
                            _                                  -> false
                       )
  , processNode: (\_ _ _ -> pure $ DOM.div_ [ DOM.text "Test" ]
                 )
  }

articleUrltoRelativeHook :: HtmlRenderer.HookRep
articleUrltoRelativeHook = HtmlRenderer.modifyingHook
  { shouldProcessNode: (\n ->
                          let info = do
                                name    <- HtmlRenderer.getName n
                                attribs <- HtmlRenderer.getAttribs n
                                href    <- attribs.href
                                pure { name, href }
                          in case info of
                            Just { name, href}
                              | name == "a"
                              , contains hblArticleUrlPrefix href -> true
                            _                                     -> false
                       )
  , processNode: (\n _ -> do
                    let updatedAttribs = do
                          attribs      <- HtmlRenderer.getAttribs n
                          href         <- attribs.href
                          relativeHref <- stripPrefix hblUrlPrefix href
                          pure $ attribs { href = Just relativeHref }
                    case updatedAttribs of
                      Just attribs -> do
                        HtmlRenderer.setAttribs n attribs
                      Nothing      -> pure unit
                    pure n
                 )
  }
  where
    hblUrlPrefix        = Pattern "https://www.hbl.fi"
    hblArticleUrlPrefix = Pattern "https://www.hbl.fi/artikel"

removeTooltipsHook :: HtmlRenderer.HookRep
removeTooltipsHook = HtmlRenderer.modifyingHook
  { shouldProcessNode: maybe false (isJust <<< _.title) <<< HtmlRenderer.getAttribs
  , processNode: \n _ -> do
    HtmlRenderer.setAttrib n "title" Nothing
    pure n
  }
