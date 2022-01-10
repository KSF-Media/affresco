module Prenumerera.Page.Error where

import React.Basic (JSX)
import React.Basic.DOM as DOM

renderSubscriptionExists :: JSX
renderSubscriptionExists =
  DOM.div
    { className: "container ksf-block"
    , children:
        [ DOM.div
            { className: "row"
            , children: [ DOM.text "Du har redan en prenumeration för denna produkt." ]
            }
        , DOM.div
            { className: "row"
            , children:
                [ DOM.a
                    { href: "/"
                    , children: [ DOM.text "Tillbaka till startsidan" ]
                    }
                ]
            }
        ]
    }

renderPackageLoadFailed :: JSX
renderPackageLoadFailed =
  DOM.div
    { className: "container ksf-block"
    , children:
        [ DOM.div
            { className: "row"
            , children: [ DOM.text "Någonting gick fel. Försok igen." ]
            }
        ]
    }
