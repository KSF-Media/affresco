module Mosaico.BreakingNews
  ( BreakingNewsProps
  , render
  )
  where

import React.Basic (JSX)
import React.Basic.DOM as DOM

type BreakingNewsProps = { content :: String }

render :: BreakingNewsProps -> JSX
render {content} =
  DOM.div
    { className: "mosaico--breaking-news"
    , dangerouslySetInnerHTML: {__html: content }
    }
