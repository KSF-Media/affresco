module MittKonto.Main.Types where

import Prelude

import Data.Date (Date)
import Data.Maybe (Maybe(Nothing))
import Effect (Effect)
import KSF.Alert.Component (Alert)
import KSF.Paper (Paper)
import KSF.Spinner as Spinner
import KSF.User (User, SubscriptionPayments)
import KSF.User.Login as Login
import React.Basic (JSX)

type State =
  { paper :: Paper
  , adminMode :: Boolean
  , activeUser :: Maybe User
  , loading :: Maybe Spinner.Loading
  , showWelcome :: Boolean
  , alert :: Maybe Alert
  , payments :: Maybe (Array SubscriptionPayments)
  , now :: Date
  , news :: Maybe JSX
  , loginComponent :: Login.Props -> JSX
  }

type Self =
  { state :: State
  , setState :: (State -> State) -> Effect Unit
  }

type UserView =
  { state :: State
  , setState :: (State -> State) -> Effect Unit
  , user :: User
  }

setLoading :: Maybe Spinner.Loading -> State -> State
setLoading loading = _ { loading = loading }

-- Force reload of payments whenever user changes
setActiveUser :: Maybe User -> State -> State
setActiveUser activeUser = _ { activeUser = activeUser
                             , payments = Nothing
                             }

setAlert :: Maybe Alert -> State -> State
setAlert alert = _ { alert = alert }

setPayments :: Maybe (Array SubscriptionPayments) -> State -> State
setPayments payments = _ { payments = payments }
