module SubscribePaper.User where

import Prelude

import Data.Either (Either(..))
import Data.Foldable (fold, foldMap)
import Data.Function.Uncurried (runFn0)
import Data.Maybe (Maybe(..), fromMaybe, isNothing)
import Data.Nullable (Nullable, toMaybe, toNullable)
import Effect (Effect)
import Effect.Aff as Aff
import Effect.Class.Console as Console
import Foreign (unsafeFromForeign)
import KSF.InputField.Component as InputField
import KSF.Login.Component as Login
import Persona as Persona
import React.Basic (JSX, StateUpdate(..), capture_, element, make, send)
import React.Basic as React
import React.Basic.Compat as React.Compat
import React.Basic.DOM as DOM
import Router (Match, Location)
import Router as Router
import SubscribePaper.ConfirmPurchase as ConfirmPurchase
import SubscribePaper.SubscribePaper (Product)
import Unsafe.Coerce (unsafeCoerce)

type Self = React.Self Props State Void

type Props = { match :: Match, location :: Location (Maybe LocationState) }
type State =
  { product :: Maybe Product
  , loggedInUser :: Maybe Persona.User
  }
type PathParams =
  { product :: String }
type JSProps =
  { match :: Match
  , location :: Location (Nullable LocationJsState)
  }

type LocationState   = { product :: Maybe Product }
type LocationJsState = { product :: Nullable Product }

type AddressView =
  { streetAddress :: String
  , city :: String
  , zipCode :: String
  , countryCode :: String
  }

data Action =
  SetProduct Product
  | SetUser Persona.User

fromJsProps :: JSProps -> Props
fromJsProps { match, location: { key, pathname, search, hash, state } } =
    { match: match
    , location:
         { key
         , pathname
         , search
         , hash
         , state: convertState $ toMaybe state
         }
    }
  where
    convertState :: Maybe LocationJsState -> Maybe LocationState
    convertState (Just s) =
      Just { product: toMaybe s.product }
    convertState _ = Nothing

component :: React.Component Props
component = React.createComponent "User"

jsComponent :: React.ReactComponent JSProps
jsComponent =
  React.toReactComponent fromJsProps component { initialState, render, didMount, update }

user :: Props -> JSX
user = make component { initialState, didMount, render, update }

initialState :: State
initialState =
  { product: Nothing
  , loggedInUser: Nothing
  }

didMount :: Self -> Effect Unit
didMount self@{ props: { location: { state: Just state } } } = do
  setProduct self state.product

didMount _ = pure unit

setProduct :: Self -> Maybe Product -> Effect Unit
setProduct self (Just p) = send self (SetProduct p)
setProduct _ _ = pure unit

product :: Match -> Product
product { params } =
  p.product
  where
    p = unsafeFromForeign params

update :: Self -> Action -> StateUpdate Props State Action
update self = case _ of
  SetProduct newProduct ->
    Update self.state { product = Just newProduct }
  SetUser newUser ->
    Update self.state { loggedInUser = Just newUser }

render :: Self -> JSX
render self =
  DOM.div
    { className: "col-8 mx-auto"
    , children:
        [ foldMap productName self.state.product
        , loginContainer self
        ]
    }

productName :: Product -> JSX
productName p =
  DOM.h4_ [ DOM.text $ "Din beställning: " <> p.name ]

loginContainer :: Self -> JSX
loginContainer self =
  DOM.div
    { className: "subscribe-paper--login-container center"
    , children:
        [ if isNothing self.state.loggedInUser
          then loginComponent self
          else controlProfile self
        ]
    }

loginComponent :: Self -> JSX
loginComponent self =
  DOM.div_
    [ DOM.h2_ [ DOM.text "Logga in för att göra din beställning" ]
    , React.Compat.element
        Login.component
          { onMerge: pure unit
          , onMergeCancelled: pure unit
          , onUserFetch:
            case _ of
              Left err -> Console.log $ unsafeCoerce err
              Right u  ->
                send { props: self.props, state: self.state, instance_: self.instance_ } $ SetUser u
          , launchAff_: \a -> do
              _ <- Aff.launchAff a
              Console.log "fetched user"
          }
    ]

controlProfile :: Self -> JSX
controlProfile self@{ state: { loggedInUser: (Just personaUser) } } =
  DOM.div_
    [ DOM.h2_ [ DOM.text "Kontrollera konto" ]
    , userDataRow
        [ DOM.div
          { className: "col col-12"
          , children:
              [ DOM.div
                  { className: "left"
                  , children: [ DOM.text "Din information" ]
                  }
              ]
          }
        ]
    , userDataRow
        [ inputField "Förnamn*" $ fromMaybe "" $ toMaybe personaUser.firstName
        , inputField "Efternamn*" $ fromMaybe "" $ toMaybe personaUser.lastName
        ]
    , userDataRow
        [ inputField "Adress*" address.streetAddress
        , inputField "Stad*" address.city
        ]
    , userDataRow
        [ inputField "Postnummer*" address.zipCode
        , inputField "Land*" address.countryCode
        ]
    , userDataRow
        [ DOM.div
            { className: "col col-6 center"
            , children: [ DOM.text "* = obligatoriskt fält" ]
            }
        , continueButton self
        ]
    ]
  where
    address = foldMap formatAddress $ toMaybe personaUser.address

    formatAddress :: Persona.Address -> AddressView
    formatAddress addr =
      { streetAddress: addr.streetAddress
      , city:          valueOf addr.city
      , zipCode:       valueOf addr.zipCode
      , countryCode:   addr.countryCode
      }

    valueOf :: Nullable String -> String
    valueOf s = fromMaybe "" $ toMaybe s
controlProfile _ = mempty

inputField description inputValue =
  DOM.div
   { className: "col col-6 subscribe-paper--input"
   , children:
       [ DOM.label
           { children: [ DOM.text description ]
           }
       , DOM.input
           { type: "text", defaultValue: inputValue }
       ]
   }

userDataRow children =
  DOM.div
    { className: "clearfix subscribe-paper--input-row"
    , children
    }

continueButton :: Self -> JSX
continueButton self  =
  DOM.div
    { className: "flex"
    , children: [ link ]
    }
  where
    link =
      element
        Router.link
          { to: { pathname: "/confirm", state: confirmPurchaseState }
          , children: [ button ]
          , className: "subscribe-paper--button-link"
          }
    button = DOM.span_ [ DOM.text "Fortsätt" ]
    confirmPurchaseState :: ConfirmPurchase.LocationJsState
    confirmPurchaseState = { user: toNullable self.state.loggedInUser, product: toNullable self.state.product }
