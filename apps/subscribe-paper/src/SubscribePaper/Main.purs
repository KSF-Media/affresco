module SubscribePaper.Main where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Nullable (toNullable)
import Effect (Effect)
import Effect.Aff (Aff, error)
import Effect.Aff as Aff
import Effect.Class (liftEffect)
import Effect.Uncurried (EffectFn1)
import KSF.Footer.Component as Footer
import KSF.Login.Component as Login
import KSF.Navbar.Component (Paper(..))
import KSF.Navbar.Component as Navbar
import Persona as Persona
import React.Basic (JSX, element, make, send)
import React.Basic as React
import React.Basic.DOM as DOM
import Router as Router
import SubscribePaper.Confirm as Confirm
import SubscribePaper.PaymentSelect as PaymentSelect
import SubscribePaper.ProductSelect as ProductSelect
import SubscribePaper.User as User

foreign import startNavigation :: EffectFn1 (String -> Effect Unit) Unit

type Self = React.Self Props State Action
type Props = {}

type State =
  { loading :: Maybe Loading
  , loggedInUser :: Maybe Persona.User
  , paper :: Navbar.Paper
  }

data Action =
  SetUser (Maybe Persona.User)

data Loading = Loading

component :: React.Component Props
component = React.createComponent "SubscribePaper"

app :: Props -> JSX
app = make component
  { initialState:
      { loading: Nothing
      , paper: HBL
      , loggedInUser: Nothing
      }
  , render
  }

render :: Self -> JSX
render self  =
  React.fragment
    [ navbarView self
    , classy DOM.div "clearfix"
        [ classy DOM.div "subscribe-paper--main-container col-10 lg-col-7 mx-auto"
            [ element Router.switch { children: [ confirmPurchase, selectPayment, productRoute, buyRoute, noMatchRoute ] } ]
        ]
    , footerView
    ]
  where
    confirmPurchase = element Router.route { exact: true, path: toNullable $ Just "/confirm", component: Confirm.jsComponent }
    selectPayment   = element Router.route { exact: true, path: toNullable $ Just "/payment", component: PaymentSelect.jsComponent }
    buyRoute        = element Router.route { exact: true, path: toNullable $ Just "/buy/:product", component: User.jsComponent }
    productRoute    = element Router.route { exact: true,  path: toNullable $ Just "/", component: ProductSelect.reactComponent }
    noMatchRoute    = element Router.route { exact: false, path: toNullable $ Nothing, component: ProductSelect.reactComponent }

setLoggedInUser :: Maybe Persona.User -> State -> State
setLoggedInUser loggedInUser = _ { loggedInUser = loggedInUser }

setLoading :: Maybe Loading -> State -> State
setLoading loading = _ { loading = loading }

-- | Allows to run the asynchronous action while showing the loading indicator
--   and handling the result.
withSpinner :: forall a. (Maybe Loading -> Effect Unit) -> Aff a -> Aff a
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
         liftEffect $ setLoadingState $ Just Loading
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

-- | Navbar with logo, contact info, logout button, language switch, etc.
navbarView :: Self -> JSX
navbarView self  =
  Navbar.navbar
    { paper: self.state.paper
    , loggedInUser: self.state.loggedInUser
    , logout: do
        Aff.launchAff_ do
          Login.logout
          liftEffect $
            send { props: self.props, state: self.state, instance_: self.instance_ } $ SetUser Nothing
    }

footerView :: React.JSX
footerView = Footer.footer {}

classy
  :: ({ className :: String, children :: Array JSX} -> JSX)
  -> String
  -> (Array JSX -> JSX)
classy element className children = element { className, children }
