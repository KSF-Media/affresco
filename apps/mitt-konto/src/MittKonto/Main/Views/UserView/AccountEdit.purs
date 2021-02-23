module MittKonto.Main.UserView.AccountEdit where

import MittKonto.Main.UserView.IconAction as IconAction
import React.Basic.Classic (JSX)
import React.Basic.DOM as DOM

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
      ]
      where
        passwordChangeClass = "account-edit--password-change"
        paymentHistoryClass = "account-edit--payment-history"
        creditCardUpdateClass = "account-edit--credit-card-update"


