module KSF.Editable.Component where

import Prelude

import Data.Array as Array
import Data.Maybe (Maybe(..))
import Effect (Effect)
import React.Basic (JSX, runUpdate)
import React.Basic as React
import React.Basic.DOM as DOM
import React.Basic.DOM.Events as React.Events
import React.Basic.Extended (Style, requireStyle)

import KSF.AsyncWrapper (Progress(..))
import KSF.AsyncWrapper as Wrapper
import KSF.Grid as Grid
import KSF.InputField.Component as Input

foreign import editableStyles :: Style

type Props =
  { values :: Array String
  , onSave :: Effect Unit -> Array String -> Effect Unit
  }

type State =
  { content :: Progress (Array String) }

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


send :: Self -> Action -> Effect Unit
send = runUpdate update
  where
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
          let
            state' = self.state { content = Loading values }
          in React.UpdateAndSideEffects state' (\_ -> self.props.onSave (send self Undo) values)
        _ -> React.Update self.state { content = Ready }


render :: Self -> JSX
render self@{ state, props } =
  requireStyle
    editableStyles
    $ DOM.div
      { className: "editable"
      , children: Array.singleton $ case state.content of
          Ready          -> renderRow (map mkString props.values)
          Editing values -> renderRow (Array.mapWithIndex mkInput values)
          Loading values -> renderRow (map mkString values)
          Error _err     -> renderRow (map mkString props.values)
      }
  where
    renderRow items = Grid.row_
      [ Grid.col10 $ DOM.div_ items
      , Grid.col2 $ DOM.div
        { className: "right"
        , children: [ Wrapper.asyncWrapper
          { wrapperState: state.content
          , readyView: iconEdit
          , editingView: \_ -> DOM.div_ [ iconSuccess, iconClose ]
          , errorView: \err -> DOM.div_ [ DOM.text err, iconClose ]
          } ]
        }
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
