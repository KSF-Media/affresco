module HBL365.NewPurchase where

import Prelude

import Data.Maybe (fromMaybe)
import Data.Nullable (toMaybe)
import HBL365.AnotherAccount (anotherAccount)
import React.Basic (JSX, fragment)
import React.Basic.DOM as DOM
import Vetrina.Purchase.NewPurchase as NewPurchase
import Vetrina.Types (AccountStatus(..))

render :: JSX -> NewPurchase.State -> JSX
render form state =
  case state.accountStatus of
    ExistingAccount _ ->
      fragment
        [ DOM.text "Du har redan ett KSF Media-konto"
        , form
        ]
    _ -> descriptionBox $ fragment
           [ case state.accountStatus of
                LoggedInAccount user ->
                  DOM.div
                    { children:
                        [ DOM.div_
                            [ DOM.text $ "Hej " <> fromMaybe user.email (toMaybe user.firstName)
                            ]
                        , anotherAccount
                        ]
                    }
                _ -> mempty
           , form
           ]

descriptionBox :: JSX -> JSX
descriptionBox content =
  DOM.div
    { className: "cta-box"
    , children:
        [ DOM.div
            { className: "cta-text"
            , children:
                [ DOM.text "PROVA "
                , DOM.strong_ [ DOM.text "HBL 365" ]
                , DOM.br {}
                , DOM.text "FÖRSTA MÅNADEN"
                , DOM.br {}
                , DOM.strong_ [ DOM.text "FÖR EN EURO!" ]
                ]
            }
        , content
        ]
    }
