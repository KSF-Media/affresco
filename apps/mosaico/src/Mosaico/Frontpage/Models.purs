module Mosaico.Frontpage.Models where

import Prelude

import Data.Array (head)
import Data.Maybe (Maybe(..))
import Data.String (Pattern(..), contains, stripPrefix)
import Effect (Effect)
import KSF.HtmlRenderer.Models as HtmlRenderer
import Lettera.Models (ArticleStub)
import Mosaico.MostReadList as MostReadList

data Hook
  = MostRead (Array ArticleStub) (ArticleStub -> Effect Unit)
  | ArticleUrltoRelative

toHookRep :: Hook -> HtmlRenderer.HookRep
toHookRep (MostRead articles onClickHandler) = mostReadHook { articles, onClickHandler }
toHookRep ArticleUrltoRelative               = articleUrltoRelativeHook

mostReadHook
  :: { articles :: Array ArticleStub
     , onClickHandler :: ArticleStub -> Effect Unit
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
  , processNode: (\n _ ->
                    let updatedAttribs = do
                          attribs      <- HtmlRenderer.getAttribs n
                          href         <- attribs.href
                          relativeHref <- stripPrefix hblUrlPrefix href
                          pure $ attribs { href = Just relativeHref }
                    in case updatedAttribs of
                      Just attribs -> do
                        HtmlRenderer.setAttribs n attribs
                        pure unit
                      Nothing      -> pure unit
                 )
  }
  where
    hblUrlPrefix        = Pattern "https://www.hbl.fi"
    hblArticleUrlPrefix = Pattern "https://www.hbl.fi/artikel"