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
  }

type State =
  { wrapperProgress :: AsyncWrapper.Progress JSX
  , pausedSubscriptions :: Maybe (Array User.PausedSubscription)
  , pendingAddressChanges :: Maybe (Array User.PendingAddressChange)
  , updateAction :: Maybe SubscriptionUpdateAction
  }

data SubscriptionUpdateAction
  = PauseSubscription
  | EditSubscriptionPause User.PausedSubscription
  | TemporaryAddressChange
  | EditTemporaryAddressChange User.PendingAddressChange
  | DeliveryReclamation

type Subscription =
  { package :: { name :: String
               , paper :: { name :: String }
               }
  , state :: String
  , dates :: User.SubscriptionDates
  }
