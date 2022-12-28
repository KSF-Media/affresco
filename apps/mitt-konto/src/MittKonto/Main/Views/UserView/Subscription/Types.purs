module MittKonto.Main.UserView.Subscription.Types where

import Prelude

import Data.Date (Date)
import Data.Maybe (Maybe)
import Effect (Effect)
import KSF.Api.Subscription (Subsno)
import KSF.AsyncWrapper as AsyncWrapper
import KSF.Sentry as Sentry
import KSF.User as User
import KSF.User (User)
import Prenumerera.Package as Prenumerera.Package
import Prenumerera.Package.Description (Description)
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
  , renewSubscription :: RenewSubscription -> JSX
  , setRenewingSubscription :: Maybe Subsno -> Effect Unit
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
  | RenewSubscription Description Prenumerera.Package.Package

type Subscription =
  { package :: { name :: String
               , paper :: { name :: String }
               }
  , state :: String
  , dates :: User.SubscriptionDates
  }

type RenewSubscription =
  { subscription :: User.Subscription
  , user :: User
  , package :: Prenumerera.Package.Package
  , description :: Description
  , onCancel :: Effect Unit
  , onSuccess :: Effect Unit
  , onError :: User.UserError -> Effect Unit
  }
