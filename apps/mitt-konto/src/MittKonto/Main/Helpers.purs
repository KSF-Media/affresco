module MittKonto.Main.Helpers where

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
import MittKonto.Main.Types as Types
import React.Basic (JSX, element)
import React.Basic.DOM as DOM
import React.Basic.Hooks (Component, component, useState, (/\))
import React.Basic.Hooks as React
import React.Basic.Router as Router

setLoading :: Maybe Types.Loading -> Types.State -> Types.State
setLoading loading = _ { loading = loading }

setLoggedInUser :: Maybe User -> Types.State -> Types.State
setLoggedInUser loggedInUser = _ { loggedInUser = loggedInUser }

setAlert :: Maybe Alert -> Types.State -> Types.State
setAlert alert = _ { alert = alert }

setPayments :: Maybe (Array SubscriptionPayments) -> Types.State -> Types.State
setPayments payments = _ { payments = payments }

errorAlert :: Error -> Maybe Alert
errorAlert err = oneOf
  [ do { method, url } <- KSF.Error.networkError err
       pure
         { level: Alert.danger
         , title: "Anslutningen misslyckades."
         , message: "Vänligen kontrollera din internetanslutning och försök om en stund igen."
         }
  , pure
      { level: Alert.warning
      , title: "Något gick fel vid inloggningen."
      , message: "Vänligen försök om en stund igen."
      }
  ]

classy
  :: ({ className :: String, children :: Array JSX} -> JSX)
  -> String
  -> (Array JSX -> JSX)
classy element className children = element { className, children }

anchor :: String -> String -> Array JSX -> JSX
anchor href description children = DOM.a { href, children: DOM.text description : children, target: "_blank" }

-- | Allows to run the asynchronous action while showing the loading indicator
--   and handling the result.
withSpinner :: forall a. (Maybe Types.Loading -> Effect Unit) -> Aff a -> Aff a
withSpinner setLoadingState action = do
   let timeoutDelay = Aff.Milliseconds $ 30.0 * 1000.0
       flickerDelay = Aff.Milliseconds $ 200.0
   -- The "loading" thread turns the spinner on (when started) and off (when killed).
   -- Prevent the spinner from flickering.
   loadingFiber <-
     Aff.forkAff $ (do
       -- delay turning on the spinner, in case if the action is "instantanious"
       Aff.delay flickerDelay
       -- invincibly sleep for a bit more (would still wait if killed here)
       Aff.invincible $ do
         -- turn the spinner on
         liftEffect $ setLoadingState $ Just Types.Loading
         Aff.delay flickerDelay
       -- in the end we sleep indefinetely. When killed, remove the spinner
       Aff.never) `Aff.cancelWith` Aff.effectCanceler (setLoadingState Nothing)
   -- The "action" thread runs the asynchronous task
   actionFiber <- Aff.forkAff action
   -- The "timeout" thread would kill the "action" thread after the delay.
   timeoutFiber <- Aff.forkAff do
     Aff.delay timeoutDelay
     Aff.killFiber (error "Timeout reached") actionFiber
     Aff.killFiber (error "Timeout reached") loadingFiber
   Aff.joinFiber actionFiber # Aff.finally do
     -- finally in the end, when the action has been completed
     -- we kill all other threads and switch the loading off
     Aff.killFiber (error "Action is done") timeoutFiber
     Aff.killFiber (error "Action is done") loadingFiber