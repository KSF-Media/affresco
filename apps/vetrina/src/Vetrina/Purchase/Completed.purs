module Vetrina.Purchase.Completed where

import Prelude

import Data.Maybe (Maybe(..), fromMaybe)
import Effect (Effect)
import KSF.User as User
import React.Basic (JSX, make)
import React.Basic as React
import React.Basic.DOM as DOM
import React.Basic.Events (handler_)
import Vetrina.Types (AccountStatus(..))

type Props =
  { onClose       :: Effect Unit
  , user          :: Maybe User.User
  , accountStatus :: AccountStatus
  }

type State = { user :: Maybe User.User  }

type Self = React.Self Props State

component :: React.Component Props
component = React.createComponent "PurchaseCompleted"

completed :: Props -> JSX
completed = make component
  { initialState: { user: Nothing }
  , render
  }

render :: Self -> JSX
render self =
  DOM.h1_ [ case self.props.accountStatus of
               ExistingAccount -> DOM.text "Tack för din beställning!"
               NewAccount      -> DOM.text "Ditt KSF Media konto är klart!"
          ]
  <> DOM.p_ [ DOM.text "Du kan nu läsa Premiumartiklar på HBL.fi" ] -- TODO: Should this come from props?
  <> DOM.p_ [ DOM.text $ "Vi har skickat ett bekräftelses-epost till " <> (fromMaybe "" $ map _.email self.props.user) ]
  <> completeButton self

completeButton :: Self -> JSX
completeButton self =
  DOM.button
    { className: "vetrina--button vetrina--completed-close"
    , children: [ DOM.text "Tillbaka till artikeln" ] -- TODO: This text may vary depending on use case
    , onClick: handler_ $ self.props.onClose
    }
