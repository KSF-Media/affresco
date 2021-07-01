module Prenumerera.PackageDescription where

import Prelude

import Data.Tuple (Tuple(..))
import Data.Tuple.Nested ((/\))
import Data.Maybe (Maybe(..))
import Data.Map (Map, fromFoldable)
import React.Basic (JSX)
import React.Basic.DOM as DOM
import React.Basic.Hooks as React

type Description =
  { brand :: String
  , brandLong :: String
  , descriptionShort :: String
  , descriptionLong :: JSX
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
      { brand: "HBL"
      , brandLong: "HBL"
      , descriptionShort: "Total"
      , descriptionLong: description
          [ Tuple "Papperstidningen" "Tryckta tidningen HBL måndag-söndag"
          , Tuple "E-tidningar" "HBL, Västra Nyland, Östnyland och digitala korsord på datorn"
          , Tuple "E-tidningsappen HBL 365" "E-tidningarna HBL, Västra Nyland och Östnyland och digitala korsord på mobilen eller surfplattan"
          , Tuple "Nyhetsappar" "Nyheter på mobilen och surfplattan, pushnotiser med HBL Nyheter, VN Nyheter och ÖN Nyheter"
          , Tuple "Premium" "Alla artiklar på hbl.fi, vastranyland.fi och ostnyland.fi"
          , Tuple "Digitalt månadsbrev" "Nyheter & förmåner"
          ]
      , ribbon: ribbon "7" "dagar med papper"
      , url: Nothing
      , weekdays: "mån – sön"
      , ordering: 3
      , image: "HBLTotal.png"
      , packageGroup: "Papperstidningen"
      }
  , "HBL_P+D FR" /\
      { brand: "HBL"
      , brandLong: "HBL"
      , descriptionShort: "Helg"
      , descriptionLong: description
          [ Tuple "Papperstidningen" "Tryckta tidningen HBL fredag-söndag"
          , Tuple "E-tidningar" "HBL, Västra Nyland, Östnyland och digitala korsord på datorn"
          , Tuple "E-tidningsappen HBL 365" "E-tidningarna HBL, Västra Nyland och Östnyland och digitala korsord på mobilen eller surfplattan"
          , Tuple "Nyhetsappar" "Nyheter på mobilen och surfplattan, pushnotiser med HBL Nyheter, VN Nyheter och ÖN Nyheter"
          , Tuple "Premium" "Alla artiklar på hbl.fi, vastranyland.fi och ostnyland.fi"
          , Tuple "Digitalt månadsbrev" "Nyheter & förmåner"
          ]
      , ribbon: ribbon "3" "dagar med papper"
      , url: Nothing
      , weekdays: "fre – sön"
      , ordering: 4
      , image: "HBLHelg.png"
      , packageGroup: "Papperstidningen"
      }
  , "HBL P+D SÖ" /\
      { brand: "HBL"
      , brandLong: "HBL"
      , descriptionShort: "Söndag"
      , descriptionLong: description
          [ Tuple "Papperstidningen" "Tryckta tidningen HBL söndagar"
          , Tuple "E-tidningar" "HBL, Västra Nyland, Östnyland och digitala korsord på datorn"
          , Tuple "E-tidningsappen HBL 365" "E-tidningarna HBL, Västra Nyland och Östnyland och digitala korsord på mobilen eller surfplattan"
          , Tuple "Nyhetsappar" "Nyheter på mobilen och surfplattan, pushnotiser med HBL Nyheter, VN Nyheter och ÖN Nyheter"
          , Tuple "Premium" "Alla artiklar på hbl.fi, vastranyland.fi och ostnyland.fi"
          , Tuple "Digitalt månadsbrev" "Nyheter & förmåner"
          ]
      , ribbon: ribbon "1" "dag med papper"
      , url: Nothing
      , weekdays: "söndag"
      , ordering: 5
      , image: "HBLSöndag.png"
      , packageGroup: "Papperstidningen"
      }
  , "HBL WEBB" /\
      { brand: "HBL"
      , brandLong: "HBL"
      , descriptionShort: "Premium"
      , descriptionLong: description
          [ Tuple "Premium" "Alla artiklar på hbl.fi"
          , Tuple "Nyhetsappen HBL Nyheter" "Nyheter på mobilen och surfplattan, pushnotiser"
          , Tuple "Digitalt månadsbrev" "Nyheter & förmåner"
          ]
      , ribbon: DOM.text "unika artiklar varje dag"
      , url: Nothing
      , weekdays: "Alla artiklar på hbl.fi"
      , ordering: 1
      , image: "HBLPremium.png"
      , packageGroup: "Webb Premium"
      }
  -- ÖSTNYLAND PACKAGES
  , "ÖNY_P+D" /\
      { brand: "ON"
      , brandLong: "Östnyland"
      , descriptionShort: ""
      , descriptionLong: description
          [ Tuple "Papperstidningen" "Tryckta tidningen tisdag & fredag"
          , Tuple "E-tidningen" "Läs e-tidningen på dator"
          , Tuple "ÖN i HBL 365" "Östnyland som e-tidning i mobilen och på surfplattan"
          , Tuple "ÖN Nyheter" "Alla artiklar i nyhetsappen ÖN Nyheter"
          , Tuple "Premium" "Alla artiklar på ostnyland.fi"
          , Tuple "Digitalt månadsbrev" "Nyheter & förmåner"
          ]
      , ribbon: ribbon "2" "dagar med papper"
      , url: Nothing
      , weekdays: "tisdag och fredag"
      , ordering: 2
      , image: "ON.png"
      , packageGroup: "Papperstidningen"
      }
  , "ÖNY+HBL" /\
      { brand: "ON"
      , brandLong: "Östnyland"
      , descriptionShort: " + HBL"
      , descriptionLong: description
          [ Tuple "Papperstidningen" "Östnyland 2 dagar & HBL 5 dagar"
          , Tuple "E-tidningar" "Läs ÖN, HBL och VN på datorn"
          , Tuple "E-tidningsappen HBL 365" "E-tidningar och digitala korsord på mobilen eller surfplattan"
          , Tuple "Nyhetsappar" "Alla artiklar i nyhetsapparna ÖN Nyheter, HBL Nyheter och VN Nyheter"
          , Tuple "Premium" "Alla artiklar på ostnyland.fi, hbl.fi och vastranyland.fi"
          , Tuple "Digitalt månadsbrev" "Nyheter & förmåner"
          ]
      , ribbon: ribbon "2+5" "dagar med papper"
      , url: Nothing
      , weekdays: "mån – sön"
      , ordering: 3
      , image: "ON.png"
      , packageGroup: "2+5"
      }
  , "ÖNY_DIGI" /\
      { brand: "ON"
      , brandLong: "Östnyland"
      , descriptionShort: ""
      , descriptionLong: description
          [ Tuple "Premium" "Alla artiklar på ostnyland.fi"
          , Tuple "ÖN Nyheter" "Alla artiklar i nyhetsappen ÖN Nyheter"
          , Tuple "E-tidningen" "Läs e-tidningen på dator"
          , Tuple "ÖN i HBL 365" "Östnyland som e-tidning i mobilen och på surfplattan"
          , Tuple "Digitalt månadsbrev" "Nyheter & förmåner"
          ]
      , ribbon: DOM.text "Alla dagar digitalt"
      , url: Nothing
      , weekdays: "Allt digitalt innehåll"
      , ordering: 1
      , image: "ONPremium.png"
      , packageGroup: "Webb Premium"
      }
  -- VÄSTRA NYLAND PACKAGES
  , "VN_P+D" /\
      { brand: "VN"
      , brandLong: "Västra Nyland"
      , descriptionShort: ""
      , descriptionLong: description
          [ Tuple "Papperstidningen" "Tryckta tidningen tisdag & fredag"
          , Tuple "E-tidningen" "Läs e-tidningen på dator"
          , Tuple "VN i HBL 365" "Västra Nyland som e-tidning i mobilen och på surfplattan"
          , Tuple "VN Nyheter" "Alla artiklar i nyhetsappen VN Nyheter"
          , Tuple "Premium" "Alla artiklar på vastranyland.fi"
          , Tuple "Digitalt månadsbrev" "Nyheter & förmåner"
          ]
      , ribbon: ribbon "2" "dagar med papper"
      , url: Nothing
      , weekdays: "tisdag och fredag"
      , ordering: 2
      , image: "VN.png"
      , packageGroup: "Papperstidningen"
      }
  , "VN+HBL" /\
      { brand: "VN"
      , brandLong: "Västra Nyland"
      , descriptionShort: " + HBL"
      , descriptionLong: description
          [ Tuple "Papperstidningen" "Västra Nyland 2 dagar & HBL 5 dagar"
          , Tuple "E-tidningen" "Läs VN, HBL och ÖN på datorn"
          , Tuple "E-tidningsappen HBL 365" "E-tidningar och digitala korsord på mobilen eller surfplattan"
          , Tuple "Nyhetsappar" "Alla artiklar i nyhetsapparna VN Nyheter, HBL Nyheter och ÖN Nyheter"
          , Tuple "Premium" "Alla artiklar på vastranyland.fi, hbl.fi och ostnyland.fi"
          , Tuple "Digitalt månadsbrev" "Nyheter & förmåner"
          ]
      , ribbon: ribbon "2+5" "dagar med papper"
      , url: Nothing
      , weekdays: "mån – sön"
      , ordering: 3
      , image: "VPN.png"
      , packageGroup: "2+5"
      }
  , "VN_DIGI" /\
      { brand: "VN"
      , brandLong: "VN Premium"
      , descriptionShort: ""
      , descriptionLong: description
          [ Tuple "Premium" "Alla artiklar på vastranyland.fi"
          , Tuple "VN Nyheter" "Alla artiklar i nyhetsappen VN Nyheter"
          , Tuple "VN i HBL 365" "Västra Nyland som e-tidning i mobilen och på surfplattan"
          , Tuple "E-tidningen" "Läs e-tidningen på dator"
          , Tuple "Digitalt månadsbrev" "Nyheter & förmåner"
          ]
      , ribbon: DOM.text "Alla dagar digitalt"
      , url: Nothing
      , weekdays: "allt digitalt innehåll"
      , ordering: 1
      , image: "vn_digi_premium.png"
      , packageGroup: "Webb Premium"
      }
  -- HBL JUNIOR PACKAGES
  , "JUNIOR PD" /\
      { brand: "JUNIOR"
      , brandLong: ""
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
      }
  -- OTHER PACKAGES
  , "HBL 365" /\
      { brand: "HBL"
      , brandLong: "HBL"
      , descriptionShort: "365"
      , descriptionLong: description
          [ Tuple "E-tidningsappen HBL 365" "E-tidningarna HBL, Västra Nyland, Östnyland och digitala korsord"
          , Tuple "E-tidningar på dator" "HBL, Västra Nyland, Östnyland"
          , Tuple "Nyhetsappar" "HBL Nyheter, VN Nyheter, ÖN Nyheter, pushnotiser"
          , Tuple "Alla artiklar inkl. premium" "HBL.fi, Vastranyland.fi, Ostnyland.fi"
          , Tuple "Digitalt månadsbrev" "Nyheter & förmåner"
          ]
      , ribbon: React.fragment
          [ DOM.span_ [ DOM.text "första månaden" ], DOM.span_ [ DOM.strong_ [ DOM.text "1€" ] ] ]
      , url: Just "https://customer.hbl.fi/hbl365"
      , weekdays: "digitalt endast"
      , ordering: 2
      , image: "HBL365365.png"
      , packageGroup: "HBL 365"
      }
  ]
  where
    description =
      DOM.ul_ <<< map (\(Tuple a b) ->
                          DOM.li_ [ DOM.p_ [ DOM.strong_ [ DOM.text a ]
                                           , DOM.br {}
                                           , DOM.strong_ [ DOM.text b ]
                                           ]
                                  ]
                      )
    ribbon a b = React.fragment [ DOM.strong_ [ DOM.text a ], DOM.span_ [ DOM.text b ] ]
