module Mosaico.Footer where

import Prelude
import KSF.Paper (Paper(..), cssName, homepage, paperName)
import React.Basic (JSX)
import React.Basic.DOM as DOM
import React.Basic.Events (EventHandler)

footer :: Paper -> (String -> EventHandler) -> JSX
footer mosaicoPaper onStaticPageClick =
  DOM.footer
    { className: "mosaico--footer"
    , children:
        [ contactInfo mosaicoPaper onStaticPageClick
        , DOM.hr { className: "mosaico-footer__separator" }
        , DOM.div
            { className: "mosaico-footer__caption"
            , children: [ DOM.text "ALLA KSF-TIDNINGAR" ]
            }
        , DOM.div
            { className: "mosaico-footer__other-papers"
            , children: map logo [ VN, HBL, ON ]
            }
        ]
    }
  where
  logo :: Paper -> JSX
  logo paper =
    let
      className = "mosaico-footer__logo mosaico-footer__logo--" <> cssName paper

      children =
        [ DOM.div { className: "mosaico-footer__logo-image mosaico-footer__logo-image--" <> cssName paper }
        , DOM.div
            { className: "mosaico-footer__logo-caption"
            , children: [ DOM.text $ paperName paper ]
            }
        ]
    in
      if paper == mosaicoPaper then
        DOM.div { className, children }
      else
        DOM.a { className, children, href: homepage paper }

contactInfo :: Paper -> (String -> EventHandler) -> JSX
contactInfo ON = ostnylandContactInfo

contactInfo _ = footerLinks

ostnylandContactInfo :: (String -> EventHandler) -> JSX
ostnylandContactInfo onStaticPageClick =
  DOM.div
    { className: "mosaico-footer__contact-info"
    , children:
        [ DOM.div
            { className: "mosaico-footer__contact-info__columns"
            , children:
                [ column firstColumn
                , column secondColumn
                , column thirdColumn
                ]
            }
        , footerLinks onStaticPageClick
        ]
    }
  where
  firstColumn =
    [ section "Kontakta Östnyland: "
        [ DOM.text "Lundagatan 8"
        , DOM.br {}
        , DOM.text "06100 Borgå"
        ]
    , section "Växel: " [ DOM.a { href: "tel:0912531", children: [ DOM.text "09 12531" ] } ]
    , section "Prenumerationer: "
        [ DOM.a { href: "tel:091253500", children: [ DOM.text "09 1253 500" ] }
        , DOM.br {}
        , DOM.text "(mån-fre kl. 8.00-12.00 och 13.00-16.00."
        , DOM.text "På onsdagar stänger kundservice kl. 15.00)"
        ]
    , section "Redaktionen: "
        [ DOM.a { href: "tel:0447776000", children: [ DOM.text "044 777 6000" ] }
        , DOM.text ", "
        , DOM.a { href: "mailto:redaktion@ostnyland.fi", children: [ DOM.text "redaktion@ostnyland.fi" ] }
        ]
    , section "Annonser: "
        [ DOM.a { href: "tel:091253558", children: [ DOM.text "09 1253 558" ] }
        , DOM.text ", "
        , DOM.a { href: "mailto:annons@ostnyland.fi", children: [ DOM.text "annons@ostnyland.fi" ] }
        ]
    ]

  secondColumn =
    [ section "Ansvarig utgivare: " [ DOM.text "Erja Yläjärvi" ]
    , section "Chefredaktör för tidningen och nyhetschef: "
        [ DOM.text "Helén Kurri "
        , DOM.a { href: "tel:0405063977", children: [ DOM.text "040 506 3977" ] }
        ]
    , section "Insändare: " [ DOM.a {href: "#", children: [DOM.text "Skriv din insändare här" ]} ]
    , section "" [ DOM.text "Det lokala kommer först." ]
    ]

  thirdColumn =
    [ section "" [ DOM.text "KSF Media ger ut Hufvudstadsbladet, Västra Nyland, Östnyland och HBL Junior. KSF Media ägs av Konstsamfundet." ]
    , section "" [ DOM.a { href: "#", children: [ DOM.text "Jobba hos oss" ] } ]
    , section "Dataskydd: " [DOM.a {href: "#", children: [ DOM.text "Hantera dataskydd" ]}] 
    ]

  column children = DOM.div { className: "mosaico-footer__contact-info__column", children: children }

  section title children =
    DOM.p
      { className: "mosaico-footer__contact-info__section"
      , children:
          [ DOM.b { children: [ DOM.text title ] }
          , DOM.div { children: children }
          ]
      }

footerLinks :: (String -> EventHandler) -> JSX
footerLinks onStaticPageClick =
  DOM.div
    { className: "mosaico-footer__links"
    , children:
        [ externalLink "Dataskyddsbeskrivning" "https://www.ksfmedia.fi/dataskydd"
        , footerLink "Bruksvillkor" "bruksvillkor"
        , footerLink "Kundservice" "kundservice"
        , footerLink "Kontakta oss" "kontakt"
        , footerLink "Tipsa oss" "tipsa-oss"
        ]
    }
  where
  externalLink caption url =
    DOM.a
      { href: url
      , children: [ DOM.text caption ]
      }

  footerLink caption link =
    DOM.a
      { href: "/sida/" <> link
      , children: [ DOM.text caption ]
      , onClick: onStaticPageClick link
      }
