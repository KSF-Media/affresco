module Mosaico.Footer where

import Prelude

import KSF.Paper (Paper(..), cssName, homepage, paperName)
import Mosaico.Paper (mosaicoPaper)
import React.Basic (JSX)
import React.Basic.DOM as DOM
import React.Basic.Events (EventHandler)

footer :: (String -> EventHandler) -> JSX
footer onStaticPageClick =
  DOM.footer
    { className: "mosaico--footer"
    , children:
        [ DOM.div
            { className: "mosaico-footer__links"
            , children:
              [ footerLink "Dataskyddsbeskrivning" "dataskyddsbeskrivning"
              , footerLink "Bruksvillkor" "bruksvillkor"
              , footerLink "Kundservice" "kundservice"
              , footerLink "Kontakta oss" "kontakt"
              ]
            }
        , DOM.hr { className: "mosaico-footer__separator" }
        , DOM.div
            { className: "mosaico-footer__caption"
            , children: [ DOM.text "ALLA KSF-TIDNINGAR" ]
            }
        , DOM.div
            { className: "mosaico-footer__other-papers"
            , children: map logo [VN, HBL, ON]
            }
        ]
    }
  where
    footerLink caption link =
      DOM.a
        { href: "/sida/" <> link
        , children: [ DOM.text caption ]
        , onClick: onStaticPageClick link
        }
    logo :: Paper -> JSX
    logo paper =
      let className = "mosaico-footer__logo mosaico-footer__logo--" <> cssName paper
          children =
            [ DOM.div { className: "mosaico-footer__logo-image mosaico-footer__logo-image--" <> cssName paper }
            , DOM.div
                { className: "mosaico-footer__logo-caption"
                , children: [ DOM.text $ paperName paper ]
                }
            ]
      in if paper == mosaicoPaper
         then DOM.div { className, children }
         else DOM.a { className, children, href: homepage paper }
