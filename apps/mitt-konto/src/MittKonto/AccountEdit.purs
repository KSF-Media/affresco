module MittKonto.AccountEdit where

import Prelude

import Bottega (bottegaErrorMessage)
import Bottega.Models (CreditCard)
import Data.Array (null)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Aff as Aff
import Effect.Class (liftEffect)
import KSF.AsyncWrapper as AsyncWrapper
import KSF.CreditCard.Update as CreditCard
import KSF.Sentry as Sentry
import KSF.User as User
import MittKonto.IconAction as IconAction
import React.Basic (make, JSX)
import React.Basic as React
import React.Basic.DOM as DOM
import React.Basic.Events (handler_)

accountEdit :: JSX
accountEdit = DOM.div
  { className: "account-edit--actions-container"
  , children: accountEditActions
  }
  where
    accountEditActions :: Array JSX
    accountEditActions =
      [ IconAction.iconAction
          { iconClassName: passwordChangeClass
          , description: "Byt lösenord"
          , onClick: IconAction.Href "https://www.hbl.fi/losenord"
          }
      , IconAction.iconAction
          { iconClassName: paymentHistoryClass
          , description: "Fakturor"
          , onClick: IconAction.Router "/fakturor"
          }
      , IconAction.iconAction
          { iconClassName: creditCardUpdateClass
          , description: "Uppdatera ditt kredit- eller bankkort"
          , onClick:
              IconAction.Router "/kortt/uppdatera"
          }
      ]
      where
        passwordChangeClass = "account-edit--password-change"
        paymentHistoryClass = "account-edit--payment-history"
        creditCardUpdateClass = "account-edit--credit-card-update"


