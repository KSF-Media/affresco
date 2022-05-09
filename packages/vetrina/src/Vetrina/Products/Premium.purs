module KSF.Vetrina.Products.Premium where

import Prelude

import Data.Maybe (Maybe(..))
import KSF.Api.Package (CampaignLengthUnit(..))
import React.Basic.DOM as DOM
import Vetrina.Types (Product)

hblPremium :: Product
hblPremium =
  { id: "HBL WEBB"
  , name: "Hufvudstadsbladet Premium"
  , priceCents: 999
  , description:
      DOM.div_
        [ DOM.text "Kvalitetsjournalistik när, var och hur du vill."
        , DOM.br {}
        , DOM.text "Läs Hufvudstadsbladet för 1€ i en månad, därefter 9,99€ / månad tills vidare. Avsluta när du vill."
        ]
  , descriptionPurchaseCompleted: DOM.text "Du kan nu läsa Premiumartiklar på HBL.fi."
  , campaign: Just
      { no: 4071
      , id: "1MÅN1 EURO"
      , name: "FÖRSTA MÅNADEN FÖR 1 EURO"
      , length: 1
      , lengthUnit: Month
      , priceEur: 1.0
      }
  , contents:
      [ { title: "Premium"
        , description: "Alla artiklar på hbl.fi"
        }
      , { title: "Nyhetsappen HBL Nyheter"
        , description: "Nyheter på mobilen och surfplattan, pushnotiser"
        }
      , { title: "Digitalt månadsbrev"
        , description: "Nyheter & förmåner"
        }
      ]
  }

vnPremium :: Product
vnPremium =
  { id: "VN_DIGI"
  , name: "Västra Nyland Premium"
  , priceCents: 690
  , description:
      DOM.div_
        [ DOM.text "Kvalitetsjournalistik när, var och hur du vill."
        , DOM.br {}
        , DOM.text "Läs Västra Nyland för 1€ i en månad, därefter 6,90€ / månad tills vidare. Avsluta när du vill."
        ]
  , descriptionPurchaseCompleted: DOM.text "Du kan nu läsa Premiumartiklar på vastranyland.fi."
  , campaign: Just
      { no: 4072
      , id: "1MÅN1EURO"
      , name: "FÖRSTA MÅNADEN FÖR 1 EURO"
      , length: 1
      , lengthUnit: Month
      , priceEur: 1.0
      }
  , contents: commonDescription "vastranyland.fi" "VN" "Västra Nyland"
  }

onPremium :: Product
onPremium =
  { id: "ÖNY_DIGI"
  , name: "Östnyland Premium"
  , priceCents: 690
  , description:
      DOM.div_
        [ DOM.text "Kvalitetsjournalistik när, var och hur du vill."
        , DOM.br {}
        , DOM.text "Läs Östnyland för 1€ i en månad, därefter 6,90€ / månad tills vidare. Avsluta när du vill."
        ]
  , descriptionPurchaseCompleted: DOM.text "Du kan nu läsa Premiumartiklar på ostnyland.fi."
  , campaign: Just
      { length: 1
      , priceEur: 1.0
      , lengthUnit: Month
      , no: 4073
      , name: "FÖRSTA MÅNADEN 1 EURO"
      , id: "1MÅN1EURO"
      }
  , contents: commonDescription "ostnyland.fi" "ÖN" "Östnyland"
  }

commonDescription :: String -> String -> String -> Array { title :: String, description :: String }
commonDescription url id name =
  [ { title: "Premium"
    , description: "Alla artiklar på " <> url
    }
  , { title: id <> " Nyheter"
    , description: "Alla artiklar i nyhetsappen " <> id <> " Nyheter"
    }
  , { title: id <> " i HBL 365"
    , description: name <> " som e-tidning i mobilen och på surfplattan"
    }
  , { title: "E-tidningen"
    , description: "Läs e-tidningen på dator"
    }
  , { title: "Digitalt månadsbrev"
    , description: "Nyheter & förmåner"
    }
  ]
