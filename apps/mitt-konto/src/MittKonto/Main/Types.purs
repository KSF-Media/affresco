module MittKonto.Main.Types where

import Prelude

import Bottega.Models (CreditCard)
import Data.Maybe (Maybe)
import Effect (Effect)
import KSF.Alert.Component (Alert)
import KSF.Paper (Paper)
import KSF.User (User, SubscriptionPayments)

type State =
  { paper :: Paper
  , loggedInUser :: Maybe User
  , loading :: Maybe Loading
  , showWelcome :: Boolean
  , alert :: Maybe Alert
  , payments :: Maybe (Array SubscriptionPayments)
  , creditCards :: Array CreditCard
  }

data Loading = Loading

type Self =
  { state :: State
  , setState :: (State -> State) -> Effect Unit
  }