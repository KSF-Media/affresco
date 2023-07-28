module MittKonto.Main.UserView.AccountEdit where

import MittKonto.Main.UserView.IconAction as IconAction
import React.Basic (JSX)
import React.Basic.DOM as DOM
import Routing.PushState (PushStateInterface)

accountEdit :: PushStateInterface -> JSX
accountEdit router = DOM.div
  { className: "account-edit--actions-container"
  , children: accountEditActions
  }
  where
    accountEditActions :: Array JSX
    accountEditActions =
      [ IconAction.render
          { iconClassName: passwordChangeClass
          , description: "Byt lösenord"
          , onClick: IconAction.Href "/#lösenord"
          , router
          }
      , IconAction.render
          { iconClassName: paymentHistoryClass
          , description: "Fakturor"
          , onClick: IconAction.Router "/fakturor"
          , router
          }
      ]
      where
        passwordChangeClass = "account-edit--password-change"
        paymentHistoryClass = "account-edit--payment-history"
