module KSF.Grid where

import Prelude

import Data.Maybe (Maybe(..))
import Data.String (joinWith)
import React.Basic (JSX)
import React.Basic.DOM as DOM

type RowOptions =
  { extraClasses :: Array String }

-- | Row with two columns
row2 :: JSX -> JSX -> Maybe RowOptions -> JSX
row2 leftElem rightElem opts =
  DOM.div
    { className: classes
    , children:
        [ column2 [ leftElem ]
        , column2 [ rightElem ]
        ]
    }
  where
    classes =
      let baseClass = "clearfix"
      in case opts of
        Just o  -> baseClass <> joinWith " " o.extraClasses
        Nothing -> baseClass

column2 :: Array JSX -> JSX
column2 children =
  DOM.div
    { className: "col col-6"
    , children
    }
