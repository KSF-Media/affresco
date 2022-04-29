module Mosaico.Article.Renderer where

import Prelude

import Data.Array (cons, head, replicate)
import Data.Foldable (fold, foldMap)
import Data.Maybe (Maybe(..))
import KSF.HtmlRenderer (render)
import KSF.HtmlRenderer.Models as HtmlRenderer
import React.Basic (JSX)
import React.Basic.DOM as DOM

renderReviewStars :: String -> JSX
renderReviewStars content = render { content, hooks: Just [ reviewStarsHook ] }

reviewStarsHook :: HtmlRenderer.HookRep
reviewStarsHook = HtmlRenderer.replacingHook
  { shouldProcessNode: \n ->
     HtmlRenderer.getName n == Just "code" &&
     HtmlRenderer.getAttrib "data-name" n == Just "stars"
  , processNode: \n _ _ ->
     pure $
     let stars = case HtmlRenderer.getChildren n >>= head >>= HtmlRenderer.getAttrib "data-value" of
           Just "0" -> Just 0
           Just "1" -> Just 1
           Just "2" -> Just 2
           Just "3" -> Just 3
           Just "4" -> Just 4
           Just "5" -> Just 5
           _        -> Nothing
     in foldMap (\i -> fold <<< cons (DOM.text $ show i <> " ") $
                       replicate i pointStar <> replicate (5-i) emptyStar) stars
  }
  where
    pointStar = DOM.span { className: "glyphicon glyphicon-star" }
    emptyStar = DOM.span { className: "glyphicon glyphicon-star-empty" }
