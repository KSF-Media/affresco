module KSF.InputField.View where

import Prelude

import Data.Array ((:))
import Effect.Uncurried (EffectFn1)
import React.Basic (JSX)
import React.Basic.DOM as DOM
import React.Basic.Events (SyntheticEvent)

type Attributes =
  { type_ :: String
  , placeholder :: String
  , name :: String
  , value :: String
  , required :: Boolean
  , onChange :: EffectFn1 SyntheticEvent Unit
  , onBlur :: EffectFn1 SyntheticEvent Unit
  , children :: Array JSX
  }

inputField :: Attributes -> JSX
inputField attrs =
    DOM.div
    { className: "input-field"
    , children: input : attrs.children
    }
  where
    input =
      DOM.input
        { type: attrs.type_
        , placeholder: attrs.placeholder
        , name: attrs.name
        , value: attrs.value
        , required: attrs.required
        , onChange: attrs.onChange
        , onBlur: attrs.onBlur
        }
