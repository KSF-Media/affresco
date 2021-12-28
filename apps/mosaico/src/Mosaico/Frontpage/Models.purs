module Mosaico.Frontpage.Models where

import Prelude

import Data.Array (head)
import Data.Maybe (Maybe(..))
import Data.Tuple.Nested ((/\))
import KSF.HtmlRenderer.Models as HtmlRenderer
import Lettera.Models (ArticleStub)
import Mosaico.Models as Mosaico
import Mosaico.MostReadList as MostReadList

data Content
  = ArticleList (Array ArticleStub)
  | Html String (Array Hook)

fromArticleFeed :: Mosaico.ArticleFeed -> Array Hook -> Content
fromArticleFeed (Mosaico.ArticleList articles) _ = ArticleList articles
fromArticleFeed (Mosaico.Html html) hooks = Html html hooks

fromArticleFeedNoHooks :: Mosaico.ArticleFeed -> Content
fromArticleFeedNoHooks = flip fromArticleFeed []

data Hook
  = AndraLaser MostReadList.Props

toHookRep :: Hook -> HtmlRenderer.HookRep
toHookRep (AndraLaser props) = andraLaserHook props

andraLaserHook :: MostReadList.Props -> HtmlRenderer.HookRep
andraLaserHook props = HtmlRenderer.replacingHook
  { shouldProcessNode: (\n ->
                          let info = do
                                name      <- HtmlRenderer.getName n
                                attribs   <- HtmlRenderer.getAttribs n
                                className <- attribs.class
                                children  <- HtmlRenderer.getChildren n
                                textChild <- head children
                                text      <- HtmlRenderer.getData textChild
                                pure $ name /\ className /\ text
                          in case info of
                            Just (name /\ className /\ text)
                              | name      == "div"
                              , className == "dre-item__title"
                              , text      == "Andra läser DESKTOP" -> true
                            _                                      -> false
                       )
  , processNode: (\_ _ _ -> pure $ MostReadList.render props)
  }