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
  , onSave :: Array String  -> Effect Unit
  }


type State =
  { content :: Progress (Array String) }


data Progress a
  = Ready
  | Editing a
  | Loading a
  | Error

derive instance functorProgress :: Functor Progress

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
    initialState = { content: Ready }

component :: React.Component Props
component = React.createComponent "EditableField"




update :: Self -> Action -> React.StateUpdate Props State
update self = case _ of
  StartEdit -> React.Update self.state { content = Editing self.props.values }
  Change i v -> React.Update self.state
    { content = case self.state.content of
         Editing values -> case Array.updateAt i v values of
           Nothing -> Ready -- TODO: what do here
           Just newValues -> Editing newValues
         _ -> Ready
    }
  Undo -> React.Update self.state { content = Ready }
  Save -> case self.state.content of
    Editing values ->
      let state' = self.state { content = Loading values }
      in React.UpdateAndSideEffects state' (const $ self.props.onSave values)
    _ -> React.Update self.state { content = Error }


send :: Self -> Action -> Effect Unit
send = runUpdate update


render :: Self -> JSX
render self@{ state, props } =
  requireStyle
    editableStyles
    $ DOM.div
      { className: "editable"
      , children:
        let
          staticValues = map mkString props.values

        in Array.singleton $ case state.content of
          Ready -> renderRow staticValues [ iconEdit ]
          Editing values -> renderRow (Array.mapWithIndex mkInput values) [ iconSuccess, iconClose ]
          Loading values -> renderRow (map mkString values) [ DOM.text "some loading icon" ] -- TODO
          Error -> renderRow staticValues [ DOM.text "some error", iconClose ] -- TODO
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
