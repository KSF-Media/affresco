module MittKonto.Main.Types where

import Prelude

import Bottega.Models (CreditCard)
import Data.Maybe (Maybe)
import Effect (Effect)
import KSF.Alert.Component (Alert)
import KSF.Paper (Paper)
import KSF.Spinner as Spinner
import KSF.User (User, SubscriptionPayments)

type State =
  { paper :: Paper
  , loggedInUser :: Maybe User
  , loading :: Maybe Spinner.Loading
  , showWelcome :: Boolean
  , alert :: Maybe Alert
  , payments :: Maybe (Array SubscriptionPayments)
  , creditCards :: Array CreditCard
  }

type Self =
  { state :: State
  , setState :: (State -> State) -> Effect Unit
  }