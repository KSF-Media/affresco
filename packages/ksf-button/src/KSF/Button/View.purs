module KSF.Button.View where

import Prelude

import Data.Maybe (Maybe(..))
import Effect (Effect)
import React.Basic.DOM as DOM
import React.Basic.Events as Event
import React.Basic.Extended (JSX, Style)
import React.Basic.Extended as React
import Record (merge)

foreign import buttonStyles :: Style

type Attributes =
  { destination :: Maybe String
  , description :: String
  , onClick :: Effect Unit
  }

button :: Attributes -> JSX
button { destination, description, onClick } =
  React.requireStyle
    buttonStyles
    $ case destination of
        Just href -> DOM.a $ merge attrs { href, target: "_blank" }
        _ -> DOM.a attrs
  where
    attrs =
      { className: "button--blank-button button--noselect"
      , children:
          [ DOM.text description ]
      , onClick: Event.handler_ onClick
      }
