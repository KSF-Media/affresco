module Mosaico.Footer
  ( footer
  ) where

import Prelude
import Data.String (Pattern(..), Replacement(..), replaceAll)
import KSF.Paper (Paper(..), cssName, homepage, paperName)
import Mosaico.Ad (openConsentRevocationMessage)
import React.Basic (JSX)
import React.Basic.DOM as DOM
import React.Basic.Events (handler, EventHandler)
import React.Basic.DOM.Events (preventDefault)

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

contactInfo VN = vastranylandContactInfo

contactInfo HBL = hblContactInfo

contactInfo _ = footerLinks

hblContactInfo :: (String -> EventHandler) -> JSX
hblContactInfo onStaticPageClick =
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
    [ section "Besöksadress"
        [ DOM.text "Mannerheimvägen 18"
        , DOM.br {}
        , DOM.text "00100 Helsingfors"
        ]
    , section "Växel: " [ tel "09 125 31" ]
    , section "E-post: " [ DOM.text "fornamn.efternamn@hbl.fi" ]
    , section "Prenumerationer och Kundservice: "
        [ tel "09 125 35 00"
        , DOM.text ", "
        , email "pren@ksfmedia.fi"
        , DOM.br {}
        , DOM.text "(mån-fre kl. 8.00-12.00 och 13.00-16.00."
        , DOM.text "På onsdagar stänger kundservice kl. 15.00)"
        ]
    , section "Annonser: "
        [ tel "09 125 35 58"
        , DOM.text ", "
        , email "annons@hbl.fi"
        ]
    ]

  secondColumn =
    [ section "Erja Yläjärvi" [ DOM.text "Chefredaktör och ansvarig utgivare" ]
    , section "Lena Skogberg" [ DOM.text "Biträdande chefredaktör och redaktionschef (Kultur och samhälle)" ]
    , section "Gunilla Celvin" [ DOM.text "Redaktionschef (nyheter och sport)" ]
    , section "Steffen Ørsted" [ DOM.text "Redaktionschef (visuellt och utveckling)" ]
    , section "Nyhetstips"
        [ tel "09 125 32 22"
        , DOM.text ", "
        , email "nyheter@hbl.fi"
        ]
    , section "Insändare: "
        [ DOM.a
            { href: "/sida/insandare"
            , children: [ DOM.text "Skriv din insändare här" ]
            }
        ]
    , section "Mejla din insändäre: " [ email "debatt@hbl.fi" ]
    ]

vastranylandContactInfo :: (String -> EventHandler) -> JSX
vastranylandContactInfo onStaticPageClick =
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
    [ section "Kontakta Västra Nyland: "
        [ DOM.text "Genvägen 8"
        , DOM.br {}
        , DOM.text "10650 Ekenäs"
        ]
    , section "Prenumerationer: "
        [ tel "09 1253 500"
        , DOM.br {}
        , DOM.text "(mån-fre kl. 8.00-12.00 och 13.00-16.00."
        , DOM.text "På onsdagar stänger kundservice kl. 15.00)"
        , DOM.text ", "
        , email "pren@ostnyland.fi"
        ]
    , section "Redaktionen: "
        [ tel "019 222 822"
        , DOM.text ", "
        , email "vnred@vastranyland.fi"
        ]
    , section "Annonser: "
        [ tel "09 1253 558"
        , DOM.text ", "
        , email "annons@vastranyland.fi"
        ]
    ]

  secondColumn =
    [ section "Ansvarig utgivare: " [ DOM.text "Erja Yläjärvi" ]
    , section "Chefredaktör för tidningen och nyhetschef: " [ DOM.text "Marina Holmberg" ]
    , section "Insändare: "
        [ DOM.a
            { href: "/sida/insandare"
            , children: [ DOM.text "Skriv din insändare här" ]
            }
        ]
    , section "Anslagstavlan: "
        [ DOM.a
            { href: "/sida/anslagstavlan"
            , children: [ DOM.text "Skicka in din händelse här" ]
            }
        ]
    , section "" [ DOM.text "Det lokala kommer först." ]
    ]

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
    , section "Växel: " [ tel "09 12531" ]
    , section "Prenumerationer och kundservice:"
        [ tel "09 1253 500"
        , DOM.br {}
        , DOM.a { href: "mailto:pren@ksfmedia.fi", children: [ DOM.text "pren@ksfmedia.fi" ] }
        , DOM.br {}
        , DOM.text "(mån-fre kl. 8.00-12.00 och 13.00-16.00."
        , DOM.br {}
        , DOM.text "På onsdagar stänger kundservice kl. 15.00)"
        ]
    , section "Redaktionen: "
        [ tel "044 777 6000"
        , DOM.text ", "
        , email "redaktion@ostnyland.fi"
        ]
    , section "Annonser: "
        [ tel "09 1253 558"
        , DOM.text ", "
        , email "annons@ostnyland.fi"
        ]
    ]

  secondColumn =
    [ section "Ansvarig utgivare: " [ DOM.text "Erja Yläjärvi" ]
    , section "Chefredaktör för tidningen och nyhetschef: "
        [ DOM.text "Helén Kurri "
        , tel "040 506 3977"
        ]
    , section "Insändare: "
        [ DOM.a
            { href: "/sida/insandare"
            , children: [ DOM.text "Skriv din insändare här" ]
            }
        ]
    , section "" [ DOM.text "Det lokala kommer först." ]
    ]

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

column :: Array JSX -> JSX
column children = DOM.div { className: "mosaico-footer__contact-info__column", children: children }

section :: String -> Array JSX -> JSX
section title children =
  DOM.div
    { className: "mosaico-footer__contact-info__section"
    , children:
        [ DOM.b { children: [ DOM.text title, DOM.text " " ] }
        , DOM.div { children: children }
        ]
    }

email :: String -> JSX
email address = DOM.a { href: "mailto:" <> address, children: [ DOM.text address ] }

tel :: String -> JSX
tel number =
  DOM.a
    { href: "tel:" <> replaceAll (Pattern " ") (Replacement "") number
    , children: [ DOM.text number ]
    }

thirdColumn :: Array JSX
thirdColumn =
  [ section "" [ DOM.text "KSF Media ger ut Hufvudstadsbladet, Västra Nyland, Östnyland och HBL Junior. KSF Media ägs av Konstsamfundet." ]
  , section ""
      [ DOM.a
          { href: "https://www.ksfmedia.fi/jobba-hos-oss"
          , children: [ DOM.text "Jobba hos oss" ]
          }
      ]
  , section "Dataskydd: "
      [ DOM.a
          { href: "#"
          , onClick: handler preventDefault openConsentRevocationMessage
          , children: [ DOM.text "Hantera dataskydd" ]
          }
      ]
  ]
