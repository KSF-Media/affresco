module SubscribePaper.User where

import Prelude

import Data.Either (Either(..))
import Data.Foldable (fold, foldMap)
import Data.Function.Uncurried (runFn0)
import Data.Maybe (Maybe(..), fromMaybe, isNothing)
import Data.Nullable (Nullable, toMaybe)
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
import Router (Match)
import Router as Router
import SubscribePaper.SubscribePaper (Product)
import Unsafe.Coerce (unsafeCoerce)

type Self = React.Self Props State Void

type Props = { match :: Match }
type State =
  { product :: Maybe Product
  , loggedInUser :: Maybe Persona.User
  }
type PathParams =
  { product :: String }
type JSProps =
  { match :: Match }

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
fromJsProps jsProps =
    { match: jsProps.match
    }

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
didMount self = do
  send { props: self.props, state: self.state, instance_: self.instance_ } productUpdateAction
  where
    productUpdateAction = SetProduct $ product self.props.match

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
          else foldMap controlProfile self.state.loggedInUser
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

controlProfile :: Persona.User -> JSX
controlProfile personaUser =
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
        , continueButton
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

inputField description inputValue =
  DOM.div
   { className: "col col-6 subscribe-paper--input"
   , children:
       [ DOM.label
           { children: [ DOM.text description ]
           }
       , DOM.input
           { type: "text", value: inputValue }
       ]
   }

-- inputField description defaultValue =
--   DOM.div
--     { className: "col col-6 subscribe-paper--input"
--     , children:
--         [ DOM.label { children: [ DOM.text description ] }
--         , React.element
--             InputField.component
--               { type_: "text"
--               , placeholder: ""
--               , name: ""
--               , required: false
--               , children: []
--               , defaultValue: Just defaultValue
--               , onChange: \_ -> pure unit
--               }
--         ]
--     }

userDataRow children =
  DOM.div
    { className: "clearfix subscribe-paper--input-row"
    , children
    }

continueButton :: JSX
continueButton =
  DOM.div
    { className: "subscribe-paper--buy-now flex justify-center"
    , children:
        [ DOM.span_ [ link ] ]
    }
  where
    link = element Router.link { to: "/confirm", children: [ "Fortsätt" ] }
