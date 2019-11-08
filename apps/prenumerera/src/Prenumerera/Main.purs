module Prenumerera.Main where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Nullable (toNullable)
import Effect (Effect)
import Effect.Aff (Aff, error)
import Effect.Aff as Aff
import Effect.Class (liftEffect)
import KSF.Footer.Component as Footer
import KSF.User as KSF.User
import KSF.Navbar.Component (Paper(..))
import KSF.Navbar.Component as Navbar
import Persona as Persona
import Prenumerera.Confirm as Confirm
import Prenumerera.PaymentSelect as PaymentSelect
import Prenumerera.ProductSelect as ProductSelect
import Prenumerera.User as User
import React.Basic (JSX, StateUpdate(..), element, make, runUpdate)
import React.Basic as React
import React.Basic.DOM as DOM
import React.Basic.Router as Router


type Self = React.Self Props State
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
component = React.createComponent "Prenumerera"

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
        [ classy DOM.div "prenumerera--main-container col-10 lg-col-7 mx-auto"
            [ element Router.switch { children: [ confirmPurchase, selectPayment, productRoute, buyRoute, noMatchRoute ] } ]
        ]
    , footerView
    ]
  where
    confirmPurchase =
      element
        Router.route
          { exact: true
          , path: toNullable $ Just "/confirm"
          , render: Confirm.confirm <<< Confirm.fromJSProps
          }
    selectPayment =
      element
        Router.route
          { exact: true
          , path: toNullable $ Just "/payment"
          , render: PaymentSelect.paymentSelect <<< PaymentSelect.fromJsProps
          }
    buyRoute =
      element
        Router.route
            { exact: true
            , path: toNullable $ Just "/buy/:product"
            , render: renderUser self
            }
    productRoute =
      element
        Router.route
            { exact: true
            , path: toNullable $ Just "/"
            , render: ProductSelect.productSelect <<< ProductSelect.fromJsProps
            }
    noMatchRoute =
      element
        Router.route
          { exact: false
          , path: toNullable Nothing
          , render: ProductSelect.productSelect <<< ProductSelect.fromJsProps
          }

update :: Self -> Action -> StateUpdate Props State
update self = case _ of
  SetUser u ->
    Update self.state { loggedInUser = u }

send :: Self -> Action -> Effect Unit
send = runUpdate update

renderUser :: Self -> User.JSProps -> JSX
renderUser self { location, match } =
  User.user
    $ User.fromJsProps
      { location
      , match
      , onUserLogin: \u -> send self $ SetUser u
      }

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
          KSF.User.logout (const $ pure unit)
          liftEffect $
            send self $ SetUser Nothing
    }

footerView :: React.JSX
footerView = Footer.footer {}

classy
  :: ({ className :: String, children :: Array JSX} -> JSX)
  -> String
  -> (Array JSX -> JSX)
classy element className children = element { className, children }
