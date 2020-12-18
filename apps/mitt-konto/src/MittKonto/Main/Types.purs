module MittKonto.Main.Types where

import Prelude

import Bottega.Models (CreditCard)
import Data.Maybe (Maybe)
import Effect (Effect)
import KSF.Alert.Component (Alert)
import KSF.AsyncWrapper as AsyncWrapper
import KSF.Paper (Paper)
import KSF.Spinner as Spinner
import KSF.User (User, SubscriptionPayments)
import React.Basic.Classic (JSX)

type State =
  { paper :: Paper
  , adminMode :: Boolean
  , activeUser :: Maybe User
  , loading :: Maybe Spinner.Loading
  , showWelcome :: Boolean
  , alert :: Maybe Alert
  , payments :: Maybe (Array SubscriptionPayments)
  , creditCards :: Array CreditCard
  , wrapperProgress :: AsyncWrapper.Progress JSX
  }

type Self =
  { state :: State
  , setState :: (State -> State) -> Effect Unit
  }

setLoading :: Maybe Spinner.Loading -> State -> State
setLoading loading = _ { loading = loading }

setActiveUser :: Maybe User -> State -> State
setActiveUser activeUser = _ { activeUser = activeUser }

setAlert :: Maybe Alert -> State -> State
setAlert alert = _ { alert = alert }

setPayments :: Maybe (Array SubscriptionPayments) -> State -> State
setPayments payments = _ { payments = payments }
