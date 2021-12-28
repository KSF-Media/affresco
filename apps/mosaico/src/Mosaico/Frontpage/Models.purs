module Mosaico.Frontpage.Models where

import Prelude

import Data.Array (head)
import Data.Maybe (Maybe(..))
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import KSF.HtmlRenderer.Models as HtmlRenderer
import Lettera.Models (ArticleStub)
import Mosaico.MostReadList as MostReadList

data Hook
  = AndraLaser (Array ArticleStub) (ArticleStub -> Effect Unit)

toHookRep :: Hook -> HtmlRenderer.HookRep
toHookRep (AndraLaser articles onClickHandler) = andraLaserHook { articles, onClickHandler }

andraLaserHook 
  :: { articles :: Array ArticleStub
     , onClickHandler :: ArticleStub -> Effect Unit 
     } 
     -> HtmlRenderer.HookRep
andraLaserHook { articles, onClickHandler } = HtmlRenderer.replacingHook
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
  , processNode: (\_ _ _ -> pure $ MostReadList.render 
                                     { mostReadArticles: articles
                                     , onClickHandler
                                     }
                 )
  }