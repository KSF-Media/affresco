module MittKonto.Main.UserView.Subscription.Types where

import Prelude

import Data.Date (Date)
import Data.Maybe (Maybe)
import Effect (Effect)
import KSF.AsyncWrapper as AsyncWrapper
import KSF.Sentry as Sentry
import KSF.User as User
import KSF.User (User)
import React.Basic (JSX)
import Routing.PushState (PushStateInterface)
import Web.HTML.Window (Window)

type Self =
  { props :: Props
  , state :: State
  , setState :: (State -> State) -> Effect Unit
  }

type Props =
  { subscription :: User.Subscription
  , user :: User
  , logger :: Sentry.Logger
  , now :: Date
  , router :: PushStateInterface
  , updateWindow :: Maybe (Maybe Window) -> Effect Unit
  }

type State =
  { pausedSubscriptions :: Maybe (Array User.PausedSubscription)
  , pendingAddressChanges :: Maybe (Array User.PendingAddressChange)
  }

type Subscription =
  { package :: { name :: String
               , paper :: { name :: String }
               }
  , state :: String
  , dates :: User.SubscriptionDates
  }
