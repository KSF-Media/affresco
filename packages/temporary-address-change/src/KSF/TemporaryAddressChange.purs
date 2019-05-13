module KSF.TemporaryAddressChange.Component where

import Prelude

import Data.DateTime (DateTime)
import Data.Maybe (Maybe)
import Effect (Effect)
import Persona as Persona
import React.Basic (JSX, make)
import React.Basic as React
import React.Basic.DOM as DOM

type State = {}
  -- { startDate    :: Maybe DateTime
  -- , minStartDate :: Maybe DateTime
  -- , endDate      :: Maybe DateTime
  -- , minEndDate   :: Maybe DateTime
  -- , maxEndDate   :: Maybe DateTime
  -- }

type Self = React.Self Props State

type Props = {}
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
render self = DOM.text "COOLIO"
