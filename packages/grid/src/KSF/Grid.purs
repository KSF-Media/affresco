module KSF.Grid where

import Prelude

import Data.Array ((:))
import Data.Maybe (Maybe(..))
import Data.String (joinWith)
import React.Basic (JSX)
import React.Basic.DOM as DOM

type RowOptions =
  { extraClasses :: Array String }

-- | Row with two columns
row2 :: JSX -> JSX -> Maybe RowOptions -> JSX
row2 leftElem rightElem opts =
  row [ columnHalf leftElem, columnHalf rightElem ] opts

row :: Array JSX -> Maybe RowOptions -> JSX
row children opts =
  DOM.div
    { className: classes
    , children: children
    }
  where
    classes =
      let baseClasses = [ "clearfix", "grid--row" ]
      in joinWith " "
         case opts of
           Just o  -> baseClasses <> o.extraClasses
           Nothing -> baseClasses

-- | Row without options
row_ :: Array JSX -> JSX
row_ children = row children Nothing

columnHalf :: JSX -> JSX
columnHalf = column [ "grid--column-half" ]

columnThird :: JSX -> JSX
columnThird = column [ "col-4" ]

column :: Array String -> JSX -> JSX
column classes child =
  DOM.div
    { className: joinWith " " $ columnBaseClasses <> classes
    , children: [ child ]
    }
  where
    columnBaseClasses = [ "col", "grid--column"]
