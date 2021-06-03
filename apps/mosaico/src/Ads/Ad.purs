module Mosaico.Ad where

import Prelude

import Data.Array (length, insertAt)
import Data.Maybe (fromMaybe)
import Effect (Effect)
import Effect.Class.Console as Console
import Lettera.Models (BodyElement)
import React.Basic (JSX)
import React.Basic.Classic (make)
import React.Basic.Classic as React
import React.Basic.DOM as DOM
import React.Basic.DOM as DOM
import Web.HTML.Event.EventTypes (offline)

foreign import fetchAd :: Effect Unit

component :: React.Component Props
component = React.createComponent "ad"

type Self =
  { state :: State
  , setState :: (State -> State) -> Effect Unit
  , props :: Props
  }

type Props =
  { contentUnit :: String
  }

type State =
  { populated :: Boolean
  }

insertIntoBody :: JSX -> Array JSX -> Array JSX  
insertIntoBody adBox body =
  case length body of
    count
      | count > 4 -> fromMaybe body $ insertAt (count/2) adBox body
      | otherwise -> fromMaybe body $ insertAt count adBox body

ad :: Props -> JSX
ad = make component
  { initialState: { populated: false }
  , didMount: \self -> do
      fetchAd

  , render: \self ->
      DOM.div
        { id: self.props.contentUnit
        , className: blockClass
        , children:
          [ DOM.header
              { className: blockClass <> "__header"
              , children: [ DOM.text "Annons" ]
              }
          , DOM.div
              { id: self.props.contentUnit
              , className: blockClass <> "__content-unit"
              }
          ]
        }
      }
      where
        blockClass = "mosaico-ad"

