module Mosaico.Ad where

import Prelude

import Data.Array (length, insertAt)
import Data.Maybe (fromMaybe)
import Effect (Effect)
import React.Basic.Classic (Component, JSX, createComponent, make)
import React.Basic.DOM as DOM
import Web.HTML.Event.EventTypes (offline)
import Lettera.Models (BodyElement)

component :: Component Props
component = createComponent "ad"

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
      | count > 2 -> fromMaybe body $ insertAt (count/2) adBox body
      | otherwise -> body

ad :: Props -> JSX
ad = make component
  { initialState: { populated: false }
  , didMount: \self ->
      self.setState \_ -> { populated: false }
  , render: \self ->
      DOM.div
        { id: self.props.contentUnit
        , className: "placeholder-ad"
        , children: []
        }
  }
