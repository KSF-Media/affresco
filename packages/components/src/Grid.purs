module KSF.Grid where

import Prelude

import Data.Array as Array
import Data.Nullable as Nullable
import Data.String (joinWith)
import Data.Tuple (Tuple)
import Foreign.Object as Object
import Prim.Row (class Nub, class Union)
import React.Basic (JSX)
import React.Basic.DOM as DOM
import Record as Record
import Unsafe.Coerce (unsafeCoerce)

type RowOptions =
  ( extraClasses :: Array String
  , _data :: Array (Tuple String String)
  , id :: String
  )

-- | Row with two columns
row2 :: forall a b. Union a RowOptions b => Nub b RowOptions => JSX -> JSX -> Record a -> JSX
row2 leftElem rightElem opts =
  row [ columnHalf leftElem, columnHalf rightElem ] opts

row :: forall a b. Union a RowOptions b => Nub b RowOptions => Array JSX -> Record a -> JSX
row children userOpts =
  DOM.div
    { className: classes
    , children: children
    , id: unsafeCoerce $ if opts.id == "" then Nullable.null else Nullable.notNull opts.id
    , _data: unsafeCoerce $
        if Array.null opts._data
          then Nullable.null
          else Nullable.notNull $ Object.fromFoldable opts._data
    }
  where
    defaultOpts :: Record RowOptions
    defaultOpts =
      { extraClasses: []
      , _data: []
      , id: ""
      }
    opts :: Record RowOptions
    opts = Record.merge userOpts defaultOpts
    classes =
      let baseClasses = [ "clearfix", "grid--row" ]
      in joinWith " " $ baseClasses <> opts.extraClasses

-- | Row without options
row_ :: Array JSX -> JSX
row_ children = row children { }

columnHalf :: JSX -> JSX
columnHalf = col6

columnThird :: JSX -> JSX
columnThird = column [ "col-4" ]

col10 :: JSX -> JSX
col10 = column [ "col-10" ]


col8 :: JSX -> JSX
col8 = column [ "col-8" ]

col6 :: JSX -> JSX
col6 = column [ "grid--column-half" ]

col4 :: JSX -> JSX
col4 = column [ "col-4" ]

col2 :: JSX -> JSX
col2 = column [ "col-2" ]


column :: Array String -> JSX -> JSX
column classes child =
  DOM.div
    { className: joinWith " " $ columnBaseClasses <> classes
    , children: [ child ]
    }
  where
    columnBaseClasses = [ "col", "grid--column"]
