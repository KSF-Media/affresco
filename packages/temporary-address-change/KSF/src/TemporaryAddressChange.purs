module KSF.TemporaryAddressChange.Component where

import Prelude

import Data.DateTime (DateTime)
import Data.Maybe (Maybe)
import Effect (Effect)
import KSF.Grid as Grid
import Persona as Persona
import React.Basic (JSX, make)
import React.Basic as React
import React.Basic.DOM as DOM
import React.Basic.Events (handler_)
import React.Basic.Extended (Style)
import React.Basic.Extended as React.Extended

foreign import temporaryAddressChangeStyles :: Style

type State = {}
  -- { startDate    :: Maybe DateTime
  -- , minStartDate :: Maybe DateTime
  -- , endDate      :: Maybe DateTime
  -- , minEndDate   :: Maybe DateTime
  -- , maxEndDate   :: Maybe DateTime
  -- }

type Self = React.Self Props State

type Props =
  { onCancel :: Effect Unit
  }
  -- { subsno    :: Int
  -- , userUuid  :: Persona.UUID
  -- , onCancel  :: Effect Unit
  -- , onLoading :: Effect Unit
  -- , onSuccess :: Persona.Subscription -> Effect Unit
  -- , onError   :: Persona.InvalidPauseDateError -> Effect Unit
  -- }

data Action
  = SetStartDate (Maybe DateTime)
  | SetMinStartDate (Maybe DateTime)
  | SetEndDate (Maybe DateTime)

temporaryAddressChange :: Props -> JSX
temporaryAddressChange = make component { initialState, render }

initialState :: State
initialState = {}

component :: React.Component Props
component = React.createComponent "TemporaryAddressChange"

render :: Self -> JSX
render self =
  DOM.div
    { className: "clearfix temporary-address-change--container"
    , children:
        [ Grid.row_
           [ DOM.div
               { className: "col col-11"
               , children: [ DOM.h3_ [ DOM.text "Gör tillfällig adressändring" ] ]
               }
           , DOM.div
               { className: "col-1 flex temporary-address-change--close-icon"
               , children: [ DOM.div { className: "close-icon" } ]
               , onClick: handler_ self.props.onCancel
               }
           ]
        ]
    }

withStyles :: JSX -> JSX
withStyles = React.Extended.requireStyle temporaryAddressChangeStyles
