module SubscribePaper.Package where

import Prelude

import Data.Foldable (fold)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import React.Basic (JSX, StateUpdate(..), make, send)
import React.Basic as React
import React.Basic.DOM as DOM
import SubscribePaper.SubscribePaper (Package)

type Self = React.Self Props State Void

type Props =
  { package :: Package
  , image :: String
  , button :: Maybe JSX
  }

type State =
  { package :: Maybe Package }

data Action =
  SetPackage Package

component :: React.Component Props
component = React.createComponent "Package"

package :: Props -> JSX
package = make component { initialState, render, didMount, update }

initialState :: State
initialState =
  { package: Nothing }

didMount :: Self -> Effect Unit
didMount self =
  send self (SetPackage self.props.package)

update :: Self -> Action -> StateUpdate Props State Action
update self = case _ of
  SetPackage p ->
    Update self.state { package = Just p }

render :: Self -> JSX
render self@{ state: { package: Just thisPackage } } =
  DOM.div
    { className: "subscribe-paper--package col-2 center clearfix"
    , children:
        [ DOM.h2_ [ DOM.text thisPackage.name ]
        , DOM.div
            { className: "subscribe-paper--package-days p2"
            , children: [ DOM.strong_ [ DOM.text thisPackage.days ] ]
            }
        , DOM.div
            { className: "subscribe-paper--package-description pt1"
            , children:
                [ DOM.img { src: self.props.image } ]
            }
        , fold self.props.button
        , mkChecklist thisPackage.checklist
        ]
    }
render _ = mempty

mkChecklist :: Array { title :: String, content :: String } -> JSX
mkChecklist checklist =
  DOM.ul
    { className: "subscribe-paper--package-check-list"
    , children: map listItem checklist
    }
  where
    listItem { title, content } =
      DOM.li_
        [ DOM.p_
            [ DOM.strong_ [ DOM.text title ]
            , DOM.br {}
            , DOM.text content ]
        ]
