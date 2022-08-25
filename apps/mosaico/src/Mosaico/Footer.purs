module Mosaico.Footer
  ( footer
  ) where

import Prelude
import Data.Foldable (foldMap)
import Data.Maybe  (Maybe(..))
import Data.String (Pattern(..), Replacement(..), replaceAll)
import KSF.Paper (Paper(..), homepage, paperName)
import Mosaico.Ad (openConsentAndSetCookie)
import React.Basic (JSX)
import React.Basic.DOM as DOM
import React.Basic.DOM.Events (preventDefault)
import React.Basic.Events (handler, EventHandler)

footer :: Paper -> (String -> EventHandler) -> JSX
footer mosaicoPaper onStaticPageClick =
  DOM.footer
    { className: "flex flex-col items-center py-12 px-0 m-0 bg-gray-50 lg:px-2 font-roboto [grid-area:foot]"
    , children:
        [ contactInfo mosaicoPaper onStaticPageClick
        , DOM.hr { className: "w-4/5 md:w-56 lg:w-96 mt-0 mx-auto mb-5 bg-gray-300 border-0 h-[1px]" }
        , DOM.div
            { className: "mt-6 mb-4 text-sm font-black text-gray-400"
            , children: [ DOM.text "ALLA KSF-TIDNINGAR" ]
            }
        , DOM.div
            { className: "flex flex-col w-full md:w-80 lg:w-96 sm:flex-row"
            , children: map logo [ VN, HBL, ON ]
            }
        ]
    }

  where
  logo :: Paper -> JSX
  logo paper =
    let
      className = "flex flex-col items-center justify-center flex-1 no-underline bg-cover"

      children = foldMap (\cls ->
        [ DOM.div { className: "w-14 h-14 bg-gray-400 mask-size-14" <> cls}
        , DOM.div
            { className: "text-xs text-gray-400 whitespace-nowrap"
            , children: [ DOM.text $ paperName paper ]
            }
        ]) $ logoClass paper

    in
      if paper == mosaicoPaper then
        DOM.div { className, children }
      else
        DOM.a { className, children, href: homepage paper }

  logoClass HBL = Just " maskimage-hbl"
  logoClass VN = Just " maskimage-vn"
  logoClass ON = Just " maskimage-on"
  logoClass _ = Nothing

contactInfo :: Paper -> (String -> EventHandler) -> JSX
contactInfo ON = ostnylandContactInfo

contactInfo VN = vastranylandContactInfo

contactInfo HBL = hblContactInfo

contactInfo _ = footerLinks

hblContactInfo :: (String -> EventHandler) -> JSX
hblContactInfo onStaticPageClick =
  DOM.div
    { children:
        [ DOM.div
            { className: "inline text-sm text-gray-600 lg:flex"
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
        , DOM.br {}
        , DOM.text "Annonskiosken: "
        , columnLink "https://annonskiosken.ksfmedia.fi/ilmoita/hufvudstadsbladet" [ DOM.text "hbl.fi/annonskiosken"]
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
        [ columnLink "/sida/insandare" [ DOM.text "Skriv din insändare här" ] ]
    , section "Mejla din insändäre: " [ email "debatt@hbl.fi" ]
    ]

vastranylandContactInfo :: (String -> EventHandler) -> JSX
vastranylandContactInfo onStaticPageClick =
  DOM.div
    { children:
        [ DOM.div
            { className: "inline text-sm text-gray-600 lg:flex"
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
        , email "pren@ksfmedia.fi"
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
        , DOM.br {}
        , DOM.text "Annonskiosken: "
        , columnLink "https://annonskiosken.ksfmedia.fi/ilmoita/vastranyland" [ DOM.text "vastranyland.fi/annonskiosken"]
        ]
    ]

  secondColumn =
    [ section "Ansvarig utgivare: " [ DOM.text "Erja Yläjärvi" ]
    , section "Chefredaktör för tidningen och nyhetschef: " [ DOM.text "Marina Holmberg" ]
    , section "Insändare: "
        [ columnLink "/sida/insandare" [ DOM.text "Skriv din insändare här" ]
        ]
    , section "Anslagstavlan: "
        [ columnLink "/sida/anslagstavlan" [ DOM.text "Skicka in din händelse här" ]
        ]
    , section "" [ DOM.text "Det lokala kommer först." ]
    ]

ostnylandContactInfo :: (String -> EventHandler) -> JSX
ostnylandContactInfo onStaticPageClick =
  DOM.div
    { children:
        [ DOM.div
            { className: "inline text-sm text-gray-600 lg:flex"
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
        , columnLink "mailto:pren@ksfmedia.fi" [ DOM.text "pren@ksfmedia.fi" ]
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
        , DOM.br {}
        , DOM.text "Annonskiosken: "
        , columnLink "https://annonskiosken.ksfmedia.fi/ilmoita/ostnyland" [ DOM.text "ostnyland.fi/annonskiosken"]
        ]
    ]

  secondColumn =
    [ section "Ansvarig utgivare: " [ DOM.text "Erja Yläjärvi" ]
    , section "Chefredaktör för tidningen och nyhetschef: "
        [ DOM.text "Helén Kurri "
        , tel "040 506 3977"
        ]
    , section "Insändare: "
        [ columnLink "/sida/insandare" [ DOM.text "Skriv din insändare här" ]
        ]
    , section "" [ DOM.text "Det lokala kommer först." ]
    ]

footerLinks :: (String -> EventHandler) -> JSX
footerLinks onStaticPageClick =
  DOM.div
    { className: "flex flex-col justify-center items-center mx-auto mt-9 mb-8 lg:flex-row"
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
      , className: "my-1 mx-auto text-sm text-gray-900 no-underline md:mx-5"
      , children: [ DOM.text caption ]
      }

  footerLink caption link =
    DOM.a
      { href: "/sida/" <> link
      , className: "my-1 mx-auto text-sm text-gray-900 no-underline md:mx-5"
      , children: [ DOM.text caption ]
      , onClick: onStaticPageClick link
      }

column :: Array JSX -> JSX
column children = DOM.div { className: "max-w-xs", children }

columnLink :: String -> Array JSX -> JSX
columnLink href children = DOM.a { href, children, className: "text-blue-link" }

section :: String -> Array JSX -> JSX
section title children =
  DOM.div
    { className: "block pr-1 pl-1 mt-4 mb-2"
    , children:
        [ DOM.b
        { className: "font-black"
        , children: [ DOM.text title, DOM.text " " ] }
        , DOM.div
        { className:  "no-underline"
        , children: children }
        ]
    }

email :: String -> JSX
email address = columnLink ("mailto:" <> address) [ DOM.text address ]

tel :: String -> JSX
tel number =
  columnLink
    ("tel:" <> replaceAll (Pattern " ") (Replacement "") number)
    [ DOM.text number ]

thirdColumn :: Array JSX
thirdColumn =
  [ section "" [ DOM.text "KSF Media ger ut Hufvudstadsbladet, Västra Nyland, Östnyland och HBL Junior. KSF Media ägs av Konstsamfundet." ]
  , section ""
      [ columnLink "https://www.ksfmedia.fi/jobba-hos-oss" [ DOM.text "Jobba hos oss" ]
      ]
  , section "Dataskydd: "
      [ DOM.a
          { href: "#"
          , onClick: handler preventDefault openConsentAndSetCookie
          , children: [ DOM.text "Hantera dataskydd" ]
          , className: "text-blue-link"
          }
      ]
  ]
