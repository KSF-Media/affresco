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
      let baseClasses = [ "grid--row" ]
      in joinWith " " $ baseClasses <> opts.extraClasses

-- | Row without options
row_ :: Array JSX -> JSX
row_ children = row children { }

columnHalf :: JSX -> JSX
columnHalf = col6

columnThird :: JSX -> JSX
columnThird = column [ "grid--column-third" ]

col10 :: JSX -> JSX
col10 = column [ "grid--column-10" ]


col8 :: JSX -> JSX
col8 = column [ "grid--column-8" ]

col6 :: JSX -> JSX
col6 = column [ "grid--column-half" ]

col4 :: JSX -> JSX
col4 = column [ "grid--column-4" ]

col2 :: JSX -> JSX
col2 = column [ "grid--column-2" ]


column :: Array String -> JSX -> JSX
column classes child =
  DOM.div
    { className: joinWith " " $ columnBaseClasses <> classes
    , children: [ child ]
    }
  where
    columnBaseClasses = [ "grid--column"]
