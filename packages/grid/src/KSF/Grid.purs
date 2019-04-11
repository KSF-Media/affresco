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
    , children
    }
  where
    classes =
      let baseClasses = [ "clearfix" ]
      in joinWith " "
         case opts of
           Just o  -> baseClasses <> o.extraClasses
           Nothing -> baseClasses

columnHalf :: JSX -> JSX
columnHalf = column "col-6"

columnThird :: JSX -> JSX
columnThird = column "col-4"

column :: String -> JSX -> JSX
column widthClass child =
  DOM.div
    { className: "col " <> widthClass
    , children: [ child ]
    }
