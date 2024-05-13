module MittKonto.Main.Types where

import Prelude

import Bottega (BottegaError)
import Bottega.Models (CreditCard)
import Data.Date (Date)
import Data.Either (Either)
import Data.Maybe (Maybe)
import Effect (Effect)
import KSF.Alert (Alert)
import KSF.Paper (Paper)
import KSF.Spinner as Spinner
import KSF.User (User, SubscriptionPayments)
import KSF.User.Login as Login
import React.Basic (JSX)
import Web.HTML.Window (Window)

type State =
  { paper :: Paper
  , adminMode :: Boolean
  , creditCards :: Maybe (Either BottegaError (Array CreditCard))
  , activeUser :: Maybe User
  , loading :: Maybe Spinner.Loading
  , showWelcome :: Boolean
  , alert :: Maybe Alert
  , payments :: Maybe (Array SubscriptionPayments)
  , now :: Date
  , news :: Maybe JSX
  , loginComponent :: Login.Props -> JSX
  , window :: Maybe (Maybe Window) -- Nothing=window.open has not been called, Just Nothing=window.open returned null
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
                             }

setAlert :: Maybe Alert -> State -> State
setAlert alert = _ { alert = alert }

setPayments :: Maybe (Array SubscriptionPayments) -> State -> State
setPayments payments = _ { payments = payments }
