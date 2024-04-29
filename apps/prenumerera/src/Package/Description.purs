module Prenumerera.Package.Description where

import Prelude

import Data.Tuple (Tuple(..))
import Data.Tuple.Nested ((/\))
import Data.Maybe (Maybe(..))
import Data.Map (Map, fromFoldable)
import KSF.Paper (Paper(..))
import React.Basic (JSX)
import React.Basic.DOM as DOM
import React.Basic.Hooks as React

type Description =
  { brand :: Paper
  , brandLong :: String
  , descriptionShort :: String
  , descriptionLong :: JSX
  , priceDisclaimer :: Maybe String
  , ribbon :: JSX
  , url :: Maybe String
  , weekdays :: String
  , ordering :: Int
  , image :: String
  , packageGroup :: String
  }

packageDescriptions :: Map String Description
packageDescriptions = fromFoldable
  -- HBL PACKAGES
  [ "HBL_P+D" /\
      { brand: HBL
      , brandLong: "HBL"
      , descriptionShort: " mån-sön"
      , descriptionLong: description
          [ Tuple "Premium" "Alla artiklar på HBL.fi, Vastranyland.fi, Ostnyland.fi"
          , Tuple "Allt innehåll i våra nyhetsappar" "HBL Nyheter, VN Nyheter, BBL Nyheter (iOS & Android)"
          , Tuple "Nyhetsbrev" "Exklusiva förmåner och nyheter"
          , Tuple "Digitala korsord" ""
          , Tuple "E-tidningar" "Papperstidningar digitalt i appen HBL 365 och på webben (HBL, VN & BBL)"
          , Tuple "Papperstidningen" "Utgivningsfrekvens: mån-sön (7 dagar)"
          ]
      , ribbon: ribbon "7" "dagar med papper"
      , url: Nothing
      , weekdays: "mån – sön"
      , ordering: 3
      , image: "HBLTotal.png"
      , packageGroup: "Papperstidningen"
      , priceDisclaimer: Nothing
      }
  , "HBL_P+D FR" /\
      { brand: HBL
      , brandLong: "HBL"
      , descriptionShort: " fre-sön"
      , descriptionLong: description
          [ Tuple "Premium" "Alla artiklar på HBL.fi, Vastranyland.fi, Ostnyland.fi"
          , Tuple "Allt innehåll i våra nyhetsappar" "HBL Nyheter, VN Nyheter, BBL Nyheter (iOS & Android)"
          , Tuple "Nyhetsbrev" "Exklusiva förmåner och nyheter"
          , Tuple "Digitala korsord" ""
          , Tuple "E-tidningar" "Papperstidningar digitalt i appen HBL 365 och på webben (HBL, VN & BBL)"
          , Tuple "Papperstidningen" "Utgivningsfrekvens: fre-sön (3 dagar)"
          ]
      , ribbon: ribbon "3" "dagar med papper"
      , url: Nothing
      , weekdays: "fre – sön"
      , ordering: 4
      , image: "HBLHelg.png"
      , packageGroup: "Papperstidningen"
      , priceDisclaimer: Nothing
      }
  , "HBL P+D SÖ" /\
      { brand: HBL
      , brandLong: "HBL"
      , descriptionShort: " Söndag"
      , descriptionLong: description
          [ Tuple "Premium" "Alla artiklar på HBL.fi, Vastranyland.fi, Ostnyland.fi"
          , Tuple "Allt innehåll i våra nyhetsappar" "HBL Nyheter, VN Nyheter, BBL Nyheter (iOS & Android)"
          , Tuple "Nyhetsbrev" "Exklusiva förmåner och nyheter"
          , Tuple "Digitala korsord" ""
          , Tuple "E-tidningar" "Papperstidningar digitalt i appen HBL 365 och på webben (HBL, VN & BBL)"
          , Tuple "Papperstidningen" "Utgivningsfrekvens: Söndag (1 dag)"
          ]
      , ribbon: ribbon "1" "dag med papper"
      , url: Nothing
      , weekdays: "söndag"
      , ordering: 5
      , image: "HBLSöndag.png"
      , packageGroup: "Papperstidningen"
      , priceDisclaimer: Nothing
      }
  , "HBL WEBB" /\
      { brand: HBL
      , brandLong: "HBL"
      , descriptionShort: " Digital"
      , descriptionLong: description
          [ Tuple "Premium" "Alla artiklar på HBL.fi"
          , Tuple "Allt innehåll i vår nyhetsapp" "HBL Nyheter (iOS & Android)"
          , Tuple "Nyhetsbrev" "Exklusiva förmåner och nyheter"
          , Tuple "Digitala korsord" ""
          ]
      , ribbon: DOM.text "unika artiklar varje dag"
      , url: Nothing
      , weekdays: "Allt digitalt innehåll exkl. e-tidningen"
      , ordering: 1
      , image: "HBLPremium.png"
      , packageGroup: "Webb Premium"
      , priceDisclaimer: Nothing
      }
  -- BORGÅBLADET PACKAGES
  , "BBL_P+D" /\
      { brand: ON
      , brandLong: "Borgåbladet"
      , descriptionShort: ""
      , descriptionLong: description
          [ Tuple "Premium" "Alla artiklar på Ostnyland.fi"
          , Tuple "Allt innehåll i vår nyhetsapp" "BBL Nyheter (iOS & Android)"
          , Tuple "Nyhetsbrev" "Exklusiva förmåner och nyheter"
          , Tuple "Digitala korsord" ""
          , Tuple "E-tidningen" "Papperstidningen digitalt i appen HBL 365 och på webben (BBL)"
          , Tuple "Papperstidningen" "Tisdag och fredag"
          ]
      , ribbon: ribbon "2" "dagar med papper"
      , url: Nothing
      , weekdays: "Papperstidningen tis & fre"
      , ordering: 2
      , image: "ON.png"
      , packageGroup: "Papperstidningen"
      , priceDisclaimer: Nothing
      }
  , "BBL+HBL" /\
      { brand: ON
      , brandLong: "Borgåbladet"
      , descriptionShort: " + HBL"
      , descriptionLong: description
          [ Tuple "Premium" "Alla artiklar på HBL.fi, Ostnyland.fi, Vastranyland.fi"
          , Tuple "Allt innehåll i våra nyhetsappar" "BBL Nyheter, HBL Nyheter, VN Nyheter (iOS & Android)"
          , Tuple "Nyhetsbrev" "Exklusiva förmåner och nyheter"
          , Tuple "Digitala korsord" ""
          , Tuple "E-tidningar" "Papperstidningar digitalt i appen HBL 365 och på webben (BBL, HBL & VN)"
          , Tuple "Papperstidningen" "Östnyland 2 dagar & HBL 5 dagar"
          ]
      , ribbon: ribbon "2+5" "dagar med papper"
      , url: Nothing
      , weekdays: "Papperstidningen mån-sön"
      , ordering: 3
      , image: "ON.png"
      , packageGroup: "2+5"
      , priceDisclaimer: Nothing
      }
  , "BBL_DIGI" /\
      { brand: ON
      , brandLong: "BBL Digital"
      , descriptionShort: ""
      , descriptionLong: description
          [ Tuple "Premium" "Alla artiklar Ostnyland.fi"
          , Tuple "Allt innehåll i vår nyhetsapp" "BBL Nyheter (iOS & Android)"
          , Tuple "Nyhetsbrev" "Exklusiva förmåner och nyheter"
          , Tuple "Digitala korsord" ""
          , Tuple "E-tidningen" "Papperstidningen digitalt i appen HBL 365 och på webben (BBL)"
          ]
      , ribbon: DOM.text "Alla dagar digitalt"
      , url: Nothing
      , weekdays: "Allt digitalt innehåll"
      , ordering: 1
      , image: "ONPremium.png"
      , packageGroup: "Webb Premium"
      , priceDisclaimer: Nothing
      }
  -- VÄSTRA NYLAND PACKAGES
  , "VN_P+D" /\
      { brand: VN
      , brandLong: "Västra Nyland"
      , descriptionShort: ""
      , descriptionLong: description
          [ Tuple "Premium" "Alla artiklar på Vastranyland.fi"
          , Tuple "Allt innehåll i vår nyhetsapp" "VN Nyheter (iOS & Android)"
          , Tuple "Nyhetsbrev" "Exklusiva förmåner och nyheter"
          , Tuple "Digitala korsord" ""
          , Tuple "E-tidningen" "Papperstidningen digitalt i appen HBL 365 och på webben (VN)"
          , Tuple "Papperstidningen" "Tisdag och fredag"
          ]
      , ribbon: ribbon "2" "dagar med papper"
      , url: Nothing
      , weekdays: "Papperstidningen tis & fre"
      , ordering: 2
      , image: "VN.png"
      , packageGroup: "Papperstidningen"
      , priceDisclaimer: Nothing
      }
  , "VN+HBL" /\
      { brand: VN
      , brandLong: "Västra Nyland"
      , descriptionShort: " + HBL"
      , descriptionLong: description
          [ Tuple "Premium" "Alla artiklar på Vastranyland.fi, HBL.fi, Ostnyland.fi"
          , Tuple "Allt innehåll i våra nyhetsappar" "VN Nyheter, HBL Nyheter, BBL Nyheter (iOS & Android)"
          , Tuple "Nyhetsbrev" "Exklusiva förmåner och nyheter"
          , Tuple "Digitala korsord" ""
          , Tuple "E-tidningar" "Papperstidningar digitalt i appen HBL 365 och på webben (VN, HBL & BBL)"
          , Tuple "Papperstidningen" "Västra Nyland 2 dagar & HBL 5 dagar"
          ]
      , ribbon: ribbon "2+5" "dagar med papper"
      , url: Nothing
      , weekdays: "Papperstidningen mån-sön"
      , ordering: 3
      , image: "VN.png"
      , packageGroup: "2+5"
      , priceDisclaimer: Nothing
      }
  , "VN_DIGI" /\
      { brand: VN
      , brandLong: "VN Digital"
      , descriptionShort: ""
      , descriptionLong: description
          [ Tuple "Premium" "Alla artiklar på Vastranyland.fi"
          , Tuple "Allt innehåll i vår nyhetsapp" "VN Nyheter (iOS & Android)"
          , Tuple "Nyhetsbrev" "Exklusiva förmåner och nyheter"
          , Tuple "Digitala korsord" ""
          , Tuple "E-tidningen" "Papperstidningen digitalt i appen HBL 365 och på webben (VN)"
          ]
      , ribbon: DOM.text "Alla dagar digitalt"
      , url: Nothing
      , weekdays: "Allt digitalt innehåll"
      , ordering: 1
      , image: "vn_digi_premium.png"
      , packageGroup: "Webb Premium"
      , priceDisclaimer: Nothing
      }
  -- HBL JUNIOR PACKAGES
  , "JUNIOR PD" /\
      { brand: JUNIOR
      , brandLong: "HBL Junior"
      , descriptionShort: ""
      , descriptionLong: description
          [ Tuple "Papperstidningen" "Onsdag varannan vecka"
          ]
      , ribbon: DOM.text "För nyfikna barn"
      , url: Nothing
      , weekdays: "Onsdag varannan vecka"
      , ordering: 1
      , image: "juniorBoll_1.png"
      , packageGroup: "JUNIOR"
      , priceDisclaimer: Nothing
      }
  -- OTHER PACKAGES
  , "HBL 365" /\
      { brand: HBL
      , brandLong: "HBL"
      , descriptionShort: " Digital Plus"
      , descriptionLong: description
          [ Tuple "Premium" "Alla artiklar på HBL.fi, Vastranyland.fi, Ostnyland.fi"
          , Tuple "Allt innehåll i våra nyhetsappar" "HBL Nyheter, VN Nyheter, BBL Nyheter (iOS & Android)"
          , Tuple "Nyhetsbrev" "Exklusiva förmåner och nyheter"
          , Tuple "Digitala korsord" ""
          , Tuple "E-tidningar" "Papperstidningar digitalt i appen HBL 365 och på webben (HBL, VN & BBL)"
          ]
      , ribbon: React.fragment
          [ DOM.span_ [ DOM.text "första månaden" ], DOM.span_ [ DOM.strong_ [ DOM.text "1€" ] ] ]
      , url: Just "https://www.ksfmedia.fi/prenumerera-hbl-digital-plus"
      , weekdays: "Allt digitalt innehåll inkl. e-tidningar"
      , ordering: 2
      , image: "HBL365365.png"
      , packageGroup: "HBL 365"
      , priceDisclaimer: Nothing
      }
  ]
  where
    description =
      DOM.ul_ <<< map (\(Tuple a b) ->
                          DOM.li_ [ DOM.p_ [ DOM.strong_ [ DOM.text a ]
                                           , DOM.br {}
                                           , DOM.text b
                                           ]
                                  ]
                      )
    ribbon a b = React.fragment [ DOM.strong_ [ DOM.text a ], DOM.span_ [ DOM.text b ] ]
