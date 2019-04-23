module KSF.Editable.Component where

import Prelude

import Data.Array as Array
import Data.Maybe (Maybe(..))
import Effect (Effect)
import KSF.Grid as Grid
import KSF.InputField.Component as Input
import React.Basic (JSX, runUpdate)
import React.Basic as React
import React.Basic.DOM as DOM
import React.Basic.DOM.Events as React.Events
import React.Basic.Extended (Style, requireStyle)

foreign import editableStyles :: Style

type Props =
  { values :: Array String
  , onSave :: Maybe (Array String) -> Effect Unit
  }


type State =
  { changes :: Maybe (Array String) }


data Action
  = StartEdit
  | Change Int String
  | Undo
  | Save

type SetState = (State -> State) -> Effect Unit

type Self = React.Self Props State

editable :: Props -> JSX
editable = React.make component { render, initialState }
  where
    initialState = { changes: Nothing }

component :: React.Component Props
component = React.createComponent "EditableField"

update :: Self -> Action -> React.StateUpdate Props State
update self = case _ of
  StartEdit -> React.Update self.state { changes = Just self.props.values }
  Change i v -> React.Update self.state { changes = join $ map (Array.updateAt i v) self.state.changes }
  Undo -> React.Update self.state { changes = Nothing }
  Save ->
    let state' = self.state { changes = Nothing }
    in React.UpdateAndSideEffects state' (const $ self.props.onSave self.state.changes)


send :: Self -> Action -> Effect Unit
send = runUpdate update


render :: Self -> JSX
render self@{ state, props } =
  requireStyle
    editableStyles
    $ DOM.div
      { className: "editable"
      , children: Array.singleton $ case state.changes of
          Nothing -> renderRow (map mkString props.values) [ iconEdit ]
          Just _vals -> renderRow (Array.mapWithIndex mkInput props.values) [ iconSuccess, iconClose ] -- TODO: is this right? like, do we need to merge the props with the state?
      }
  where
    renderRow items icons = Grid.row_
      [ Grid.col10 $ DOM.div_ items
      , Grid.col2 $ DOM.div { className: "right", children: icons }
      ]

    iconEdit = DOM.div
      { className: "edit-icon"
      , children: []
      , onClick: React.Events.capture_ $ send self StartEdit
      }

    iconClose = DOM.div
      { className: "close-icon"
      , children: []
      , onClick: React.Events.capture_ $ send self Undo
      }

    iconSuccess = DOM.div
      { className: "success-icon"
      , children: []
      , onClick: React.Events.capture_ $ send self Save
      }

    mkString = Grid.row_ <<< Array.singleton <<< DOM.text

    mkInput i v = Input.inputField
      { type_: "text"
      , name: show i <> "-field"
      , placeholder: v
      , defaultValue: Just v
      , required: true
      , children: []
      , onChange: send self <<< Change i
      }
