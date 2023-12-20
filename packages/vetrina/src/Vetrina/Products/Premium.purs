module KSF.Vetrina.Products.Premium where

import Prelude

import Data.Maybe (Maybe(..), fromMaybe)
import Data.Nullable (Nullable, toMaybe)
import KSF.Api.Package (CampaignLengthUnit(..))
import KSF.Paper as Paper
import KSF.Paper (Paper(..))
import React.Basic.DOM as DOM
import Vetrina.Types (Product)

foreign import getCurrentCampaignNo_ :: String -> Nullable Int

-- Default to the value production uses with optional override for
-- staging/dev use
getCurrentCampaignNo :: Int -> Paper -> Int
getCurrentCampaignNo def = fromMaybe def <<< toMaybe <<< getCurrentCampaignNo_ <<< Paper.toString

hblPremium :: Product
hblPremium =
  { id: "HBL WEBB"
  , name: "HBL Digital"
  , priceCents: 999
  , description:
      DOM.div
        { id: "tb-paywall--product-text-hbl"
        , className: "vetrina--product-text font-duplexsans font-light text-center text-lg leading-tight px-5 pb-5"
        , children:
            [ DOM.text "Håll dig uppdaterad med HBL! Läs nu två månader gratis, sedan 9,99 €/mån. Avslutas enkelt online." ]
        }
  , descriptionLoggedInAccount:
      DOM.div
          { className: "vetrina--product-text font-duplexsans font-light text-lg leading-tight px-5 pb-5 border-neutral border-r-2 border-b-2 border-l-2"
          , children:
              [ DOM.text "Du har ett konto utan läsrätt. För att läsa fler artiklar behöver du en digital prenumeration. Den kostar 9,99€ per månad. Du kan avsluta när du vill." ]
          }
  , descriptionPurchaseCompleted: DOM.text "Du kan nu läsa Premiumartiklar på HBL.fi."
  , campaign: Just
      { no: getCurrentCampaignNo 4716 HBL
      , id: "2M_MUREN23"
      , name: "BETALMUREN GRATIS I TVÅ MÅNADER"
      , length: 2
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
        , className: "vetrina--product-text font-duplexsans font-light text-center text-lg leading-tight px-5 pb-5"
        , children:
            [ DOM.text "Nu kan du läsa VN Digital för 1 € i månaden i hela 3 månader, därefter 6,90 € per månad. Avslutas enkelt online." ]
        }
  , descriptionLoggedInAccount:
      DOM.div
          { className: "vetrina--product-text font-duplexsans font-light text-lg leading-tight px-5 pb-5 border-neutral border-r-2 border-b-2 border-l-2"
          , children:
              [ DOM.text "Du har ett konto utan läsrätt. För att läsa fler artiklar behöver du en digital prenumeration. Den kostar 6,90€ per månad. Du kan avsluta när du vill." ]
          }
  , descriptionPurchaseCompleted: DOM.text "Du kan nu läsa Premiumartiklar på vastranyland.fi."
  , campaign: Just
      { no: getCurrentCampaignNo 4735 VN
      , id: "VND_POP24"
      , name: "VN DIGITAL 3 EURO FÖR 3 MÅNADER (ERBJUDANDE I BETALMUREN)"
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
        , className: "vetrina--product-text font-duplexsans font-light text-center text-lg leading-tight px-5 pb-5"
        , children:
            [ DOM.text "Nu kan du läsa ÖN Digital för 1 € i månaden i hela 3 månader, därefter 6,90 € per månad. Avslutas enkelt online." ]
        }
  , descriptionLoggedInAccount:
      DOM.div
          { className: "vetrina--product-text font-duplexsans font-light text-lg leading-tight px-5 pb-5 border-neutral border-r-2 border-b-2 border-l-2"
          , children:
              [ DOM.text "Du har ett konto utan läsrätt. För att läsa fler artiklar behöver du en digital prenumeration. Den kostar 6,90€ per månad. Du kan avsluta när du vill." ]
          }
  , descriptionPurchaseCompleted: DOM.text "Du kan nu läsa Premiumartiklar på ostnyland.fi."
  , campaign: Just
      { length: 1
      , priceEur: 1.0
      , lengthUnit: Month
      , no: getCurrentCampaignNo 4737 ON
      , name: "ÖN DIGITAL 3 EURO FÖR 3 MÅNADER (ERBJUDANDE I BETALMUREN)"
      , id: "OND_POP24"
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
