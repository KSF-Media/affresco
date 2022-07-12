module Mosaico.BreakingNews
  ( BreakingNewsProps
  , render
  )
  where

import Data.Maybe (Maybe)
import Data.Foldable (foldMap)
import React.Basic (JSX)
import React.Basic.DOM as DOM

type BreakingNewsProps = { content :: Maybe String }

render :: BreakingNewsProps -> JSX
render {content} = foldMap (
  \html -> DOM.div
             { className: "mosaico--breaking-news"
             , dangerouslySetInnerHTML: {__html: html }
             }
  ) content