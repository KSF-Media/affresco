module Prenumerera.User where

import Prelude

import Data.Either (Either(..))
import Data.Foldable (foldMap)
import Data.Maybe (Maybe(..), fromMaybe, isNothing)
import Data.Nullable (Nullable, toMaybe, toNullable)
import Data.Set as Set
import Effect (Effect)
import Effect.Aff as Aff
import Effect.Class.Console as Console
import Foreign (unsafeFromForeign)
import KSF.User.Login as Login
import Persona as Persona
import Prenumerera.PaymentSelect as PaymentSelect
import Prenumerera.Prenumerera (Product)
import React.Basic (JSX, StateUpdate(..), element, make, runUpdate)
import React.Basic as React
import React.Basic.DOM as DOM
import React.Basic.Router (Match, Location)
import React.Basic.Router as Router
import Unsafe.Coerce (unsafeCoerce)

type Self = React.Self Props State

type Props = { match :: Match, location :: Location (Maybe LocationState), onUserLogin :: (Maybe Persona.User -> Effect Unit) }
type State =
  { product :: Maybe Product
  , loggedInUser :: Maybe Persona.User
  }
type PathParams =
  { product :: String }
type JSProps =
  { match :: Match
  , location :: Location (Nullable LocationJsState)
  , onUserLogin :: (Maybe Persona.User -> Effect Unit)
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
fromJsProps { match, location: { key, pathname, search, hash, state }, onUserLogin } =
    { match: match
    , location:
         { key
         , pathname
         , search
         , hash
         , state: convertState $ toMaybe state
         }
    , onUserLogin
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
  React.toReactComponent fromJsProps component { initialState, render, didMount }

user :: Props -> JSX
user = make component { initialState, didMount, render }

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

update :: Self -> Action -> StateUpdate Props State
update self = case _ of
  SetProduct newProduct ->
    Update self.state { product = Just newProduct }
  SetUser newUser ->
    Update self.state { loggedInUser = Just newUser }

send :: Self -> Action -> Effect Unit
send = runUpdate update

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
    { className: "prenumerera--login-container center"
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
    , Login.login
        { onMerge: pure unit
        , onMergeCancelled: pure unit
        , onRegister: pure unit
        , onRegisterCancelled: pure unit
        , onUserFetch:
            case _ of
              Left err -> Console.log $ unsafeCoerce err
              Right u  -> do
                send self $ SetUser u
                self.props.onUserLogin (Just u)
        , launchAff_: \a -> do
            _ <- Aff.launchAff a
            Console.log "fetched user"
        , disableSocialLogins: Set.empty
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

inputField :: String -> String -> JSX
inputField description inputValue =
  DOM.div
   { className: "col col-6 prenumerera--input"
   , children:
       [ DOM.label
           { children: [ DOM.text description ]
           }
       , DOM.input
           { type: "text", defaultValue: inputValue }
       ]
   }

userDataRow :: Array JSX -> JSX
userDataRow children =
  DOM.div
    { className: "clearfix prenumerera--input-row"
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
          { to: { pathname: "/payment", state: paymentSelectState }
          , children: [ button ]
          , className: "prenumerera--button-link"
          }
    button = DOM.span_ [ DOM.text "Fortsätt" ]
    paymentSelectState :: PaymentSelect.LocationJsState
    paymentSelectState = { user: toNullable self.state.loggedInUser, product: toNullable self.state.product }
