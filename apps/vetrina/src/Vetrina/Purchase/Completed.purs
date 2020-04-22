module Vetrina.Purchase.Completed where

import Prelude

import Data.Maybe (Maybe, fromMaybe)
import Effect (Effect)
import KSF.User as User
import React.Basic (JSX)
import React.Basic.DOM as DOM
import React.Basic.Events (handler_)
import Vetrina.Types (AccountStatus(..))

type Props =
  { onClose       :: Effect Unit
  , user          :: Maybe User.User
  , accountStatus :: AccountStatus
  }

completed :: Props -> JSX
completed props =
  DOM.h1_ [ case props.accountStatus of
               NewAccount      -> DOM.text "Ditt KSF Media konto är klart!"
               _               -> DOM.text "Tack för din beställning!"
          ]
  <> DOM.p
       { className: "vetrina--description-text"
       , children: [ DOM.text "Du kan nu läsa Premiumartiklar på HBL.fi." ] -- TODO: Should this come from props?
       }
  <> DOM.p
       { className: "vetrina--description-text"
       , children: [ DOM.text $ "Vi har skickat en bekräftelse till " <> (fromMaybe "" $ map _.email props.user) ]
       }
  <> completeButton props

completeButton :: Props -> JSX
completeButton props =
  DOM.button
    { className: "vetrina--button vetrina--completed-close"
    , children: [ DOM.text "Fortsätt läsa artikeln" ] -- TODO: This text may vary depending on use case
    , onClick: handler_ $ props.onClose
    }
