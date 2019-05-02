module KSF.Editable.Component where

import Prelude

import Data.Array as Array
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Aff as Aff
import KSF.AsyncWrapper (Progress(..))
import KSF.AsyncWrapper as Wrapper
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
  , onSave :: (String -> Effect Unit) -> Array String -> Aff Unit
  }

type State =
  { content :: Progress (Array String) }

data Action
  = StartEdit
  | Change Int String
  | Undo (Maybe String)
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
      Undo Nothing -> React.Update self.state { content = Ready }
      Undo (Just err) -> React.Update self.state { content = Error err }
      Save -> case self.state.content of
        Editing values ->
          let
            state' = self.state { content = Loading values }
            onError = send self <<< Undo <<< Just
            sideEffect _ = Aff.launchAff_ do
              self.props.onSave onError values
              liftEffect $ send self (Undo Nothing)
          in React.UpdateAndSideEffects state' sideEffect
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
      [ Grid.col8 $ DOM.div_ items
      , Grid.col4 $ Wrapper.asyncWrapper
          { wrapperState: state.content
          , readyView: editButton
          , editingView: \_ -> flexDiv { children: [ iconSuccess, iconClose ], className: "" }
          , errorView: \err -> flexDiv { children: [ DOM.text err, iconClose ], className: "" }
          }
      ]

    flexDiv ps = DOM.div (ps { className = "editable--edit-container flex" })

    editButton = flexDiv
      { children:
        [ DOM.div
          { className: "edit-icon circle"
          , onClick: handler
          }
        , DOM.span
          { className: "editable--edit-text"
          , onClick: handler
          , children:
            [ DOM.u_ [ DOM.text "Ändra" ] ]
          }
        ]
      , className: ""
      }
      where
        handler = React.Events.capture_ $ send self StartEdit

    iconClose = DOM.div
      { className: "close-icon"
      , onClick: React.Events.capture_ $ send self (Undo Nothing)
      }

    iconSuccess = DOM.div
      { className: "success-icon"
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
