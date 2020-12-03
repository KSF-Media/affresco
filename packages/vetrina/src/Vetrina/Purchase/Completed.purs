module Vetrina.Purchase.Completed where

import Prelude

import Data.Foldable (foldMap)
import Data.Maybe (Maybe, fromMaybe)
import Effect (Effect)
import KSF.User as User
import React.Basic (JSX)
import React.Basic.DOM as DOM
import React.Basic.Events (handler_)
import Vetrina.Types (AccountStatus(..), Product)

type Props =
  { onClose          :: Effect Unit
  , user             :: Maybe User.User
  , accountStatus    :: AccountStatus
  , purchasedProduct :: Maybe Product
  }

completed :: Props -> JSX
completed props =
  DOM.h1
    { className: "vetrina--headline"
    , children:
        [ case props.accountStatus of
             NewAccount      -> DOM.text "Ditt KSF Media-konto är klart!"
             _               -> DOM.text "Tack för din beställning!"
        ]
    }
  <> DOM.p
       { className: "vetrina--description-text"
       , children: [ foldMap _.descriptionPurchaseCompleted props.purchasedProduct ]
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
