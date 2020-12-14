module MittKonto.Main.Types where
  
import Prelude

import Bottega.Models (CreditCard)
import Data.Array (snoc, sortBy, (:))
import Data.Either (Either(..), either, isLeft)
import Data.Foldable (foldMap, oneOf)
import Data.JSDate (JSDate, parse)
import Data.Maybe (Maybe(..))
import Data.Nullable (toNullable)
import Data.Set as Set
import Data.String (toUpper)
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Aff as Aff
import Effect.Class (liftEffect)
import Effect.Class.Console as Console
import Effect.Exception (Error, error, message)
import Effect.Unsafe (unsafePerformEffect)
import MittKonto.AccountEdit as AccountEdit
import MittKonto.IconAction as IconAction
import KSF.Alert.Component (Alert)
import KSF.Alert.Component as Alert
import KSF.Api.Subscription (isSubscriptionCanceled) as Subscription
import KSF.Error as KSF.Error
import KSF.Footer.Component as Footer
import KSF.JSError as Error
import KSF.Paper (Paper(..))
import KSF.Navbar.Component as Navbar
import KSF.PaymentAccordion as PaymentAccordion
import KSF.Profile.Component as Profile
import KSF.Sentry as Sentry
import KSF.Subscription.Component (subscription) as Subscription
import KSF.User (User, UserError(..), SubscriptionPayments)
import KSF.User (logout, getPayments) as User
import KSF.User.Login (login) as Login
import React.Basic (JSX, element)
import React.Basic.DOM as DOM
import React.Basic.Hooks (Component, component, useState, (/\))
import React.Basic.Hooks as React
import React.Basic.Router as Router

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