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
        , className: "vetrina--product-text font-duplexsans font-light text-center text-base leading-tight px-5 pb-5"
        , children:
            [ DOM.text "Läs HBL digitalt gratis i en månad, därefter 9,99€ per månad. Avsluta enkelt när du vill." ]
        }
  , descriptionLoggedInAccount:
      DOM.div
          { className: "vetrina--product-text font-duplexsans font-light text-base leading-tight px-5 pb-5 border-neutral border-r-2 border-b-2 border-l-2"
          , children:
              [ DOM.text "Du har ett gratiskonto. För att läsa fler artiklar behöver du en digital prenumeration. Den kostar 9,99€ per månad. Du kan avsluta när du vill." ]
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
      , { title: "Allt innehåll i vår nyhetsapp"
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
        , className: "vetrina--product-text font-duplexsans font-light text-center text-base leading-tight px-5 pb-5"
        , children:
            [ DOM.text "Läs Västra Nyland digitalt för 1€ i en månad, därefter 6,90€ per månad. Avsluta enkelt när du vill." ]
        }
  , descriptionLoggedInAccount:
      DOM.div
          { className: "vetrina--product-text font-duplexsans font-light text-base leading-tight px-5 pb-5 border-neutral border-r-2 border-b-2 border-l-2"
          , children:
              [ DOM.text "Du har ett gratiskonto. För att läsa fler artiklar behöver du en digital prenumeration. Den kostar 6,90€ per månad. Du kan avsluta när du vill." ]
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
      , { title: "Allt innehåll i vår nyhetsapp"
        , description: "VN Nyheter (iOS & Android)"
        }
      , { title: "Nyhetsbrev"
        , description: "Exklusiva förmåner och nyheter"
        }
      , { title: "Digitala korsord"
        , description: ""
        }
      , { title: "E-tidningen"
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
        , className: "vetrina--product-text font-duplexsans font-light text-center text-base leading-tight px-5 pb-5"
        , children:
            [ DOM.text "Läs Östnyland digitalt för 1€ i en månad, därefter 6,90€ per månad. Avsluta enkelt när du vill." ]
        }
  , descriptionLoggedInAccount:
      DOM.div
          { className: "vetrina--product-text font-duplexsans font-light text-base leading-tight px-5 pb-5 border-neutral border-r-2 border-b-2 border-l-2"
          , children:
              [ DOM.text "Du har ett gratiskonto. För att läsa fler artiklar behöver du en digital prenumeration. Den kostar 6,90€ per månad. Du kan avsluta när du vill." ]
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
      , { title: "Allt innehåll i vår nyhetsapp"
        , description: "ÖN Nyheter (iOS & Android)"
        }
      , { title: "Nyhetsbrev"
        , description: "Exklusiva förmåner och nyheter"
        }
      , { title: "Digitala korsord"
        , description: ""
        }
      , { title: "E-tidningen"
        , description: "Papperstidningen digitalt i appen HBL 365 och på webben (ÖN)"
        }
      ]
  }
