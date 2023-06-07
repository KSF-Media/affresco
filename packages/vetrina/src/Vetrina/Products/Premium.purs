module KSF.Vetrina.Products.Premium where

import Data.Maybe (Maybe(..))
import KSF.Api.Package (CampaignLengthUnit(..))
import React.Basic.DOM as DOM
import Vetrina.Types (Product)

hblPremium :: Product
hblPremium =
  { id: "HBL WEBB"
  , name: "HBL Digital"
  , priceCents: 999
  , description:
      DOM.div
        { id: "tb-paywall--product-text-hbl"
        , className: "vetrina--form-product-text"
        , children:
            [ DOM.text "Kvalitetsjournalistik när, var och hur du vill."
            , DOM.br {}
            , DOM.text "Läs Hufvudstadsbladet gratis i en månad, därefter 9,99€ / månad tills vidare. Avsluta när du vill."
            ]
        }
  , descriptionPurchaseCompleted: DOM.text "Du kan nu läsa Premiumartiklar på HBL.fi."
  , campaign: Just
      { no: 4349
      , id: "HBL_0E1KK"
      , name: "0 E FÖRSTA MÅNADEN"
      , length: 1
      , lengthUnit: Month
      , priceEur: 0.0
      }
  , contents:
      [ { title: "Premium"
        , description: "Alla artiklar på HBL.fi"
        }
      , { title: "Allt innehåll i vårt nyhetsapp"
        , description: "HBL Nyheter (iOS & Android)"
        }
      , { title: "Nyhetsbrev"
        , description: "Exklusiva förmåner och nyheter"
        }
      , { title: "Digitala korsord"
        , description: ""
        }
      ]
  }

vnPremium :: Product
vnPremium =
  { id: "VN_DIGI"
  , name: "VN Digital"
  , priceCents: 690
  , description:
      DOM.div
        { id: "tb-paywall--product-text-vn"
        , className: "vetrina--form-product-text"
        , children:
            [ DOM.text "Kvalitetsjournalistik när, var och hur du vill."
            , DOM.br {}
            , DOM.text "Läs Västra Nyland för 1€ i en månad, därefter 6,90€ / månad tills vidare. Avsluta när du vill."
            ]
        }
  , descriptionPurchaseCompleted: DOM.text "Du kan nu läsa Premiumartiklar på vastranyland.fi."
  , campaign: Just
      { no: 4072
      , id: "1MÅN1EURO"
      , name: "FÖRSTA MÅNADEN FÖR 1 EURO"
      , length: 1
      , lengthUnit: Month
      , priceEur: 1.0
      }
  , contents:
      [ { title: "Premium"
        , description: "Alla artiklar på Vastranyland.fi"
        }
      , { title: "Allt innehåll i vårt nyhetsapp"
        , description: "VN Nyheter (iOS & Android)"
        }
      , { title: "Nyhetsbrev"
        , description: "Exklusiva förmåner och nyheter"
        }
      , { title: "Digitala korsord"
        , description: ""
        }
      , { title: "E-tidningar"
        , description: "Papperstidningen digitalt i appen HBL 365 och på webben (VN)"
        }
      ]
  }

onPremium :: Product
onPremium =
  { id: "ÖNY_DIGI"
  , name: "ÖN Digital"
  , priceCents: 690
  , description:
      DOM.div
        { id: "tb-paywall--product-text-on"
        , className: "vetrina--form-product-text"
        , children:
            [ DOM.text "Kvalitetsjournalistik när, var och hur du vill."
            , DOM.br {}
            , DOM.text "Läs Östnyland för 1€ i en månad, därefter 6,90€ / månad tills vidare. Avsluta när du vill."
            ]
        }
  , descriptionPurchaseCompleted: DOM.text "Du kan nu läsa Premiumartiklar på ostnyland.fi."
  , campaign: Just
      { length: 1
      , priceEur: 1.0
      , lengthUnit: Month
      , no: 4073
      , name: "FÖRSTA MÅNADEN 1 EURO"
      , id: "1MÅN1EURO"
      }
  , contents:
      [ { title: "Premium"
        , description: "Alla artiklar på Ostnyland.fi"
        }
      , { title: "Allt innehåll i vårt nyhetsapp"
        , description: "ÖN Nyheter (iOS & Android)"
        }
      , { title: "Nyhetsbrev"
        , description: "Exklusiva förmåner och nyheter"
        }
      , { title: "Digitala korsord"
        , description: ""
        }
      , { title: "E-tidningar"
        , description: "Papperstidningen digitalt i appen HBL 365 och på webben (ÖN)"
        }
      ]
  }
