module HBL365.Main where

import Prelude

import Data.Array as Array
import Data.Either (Either)
import Data.Maybe (Maybe(..))
import Data.Set as Set
import Effect (Effect)
import Effect.Aff as Aff
import Effect.Class (liftEffect)
import Effect.Exception (Error)
import Effect.Unsafe (unsafePerformEffect)
import HBL365.AnotherAccount (anotherAccount)
import HBL365.Product (getProduct)
import HBL365.NewPurchase as NewPurchase
import KSF.Spinner as Spinner
import KSF.User as User
import KSF.Vetrina as Vetrina
import React.Basic (JSX)
import React.Basic.DOM as DOM
import React.Basic.Hooks (Component, component, useEffectOnce, useState', (/\))
import React.Basic.Hooks as React
import Bottega.Models.Order (OrderSource(..))
import Vetrina.Types (Product)

foreign import appStore ::
  { apple   :: String
  , android :: String
  }

foreign import logo :: String

foreign import addOnScroll :: Effect Unit

jsApp :: {} -> JSX
jsApp = unsafePerformEffect app

app :: Component {}
app = do
  component "HBL365" \_ -> React.do
    product /\ setProduct <- useState' Nothing
    useEffectOnce do
      addOnScroll
      Aff.launchAff_ do
        liftEffect <<< setProduct <<< Just <<< map Array.singleton =<< getProduct
      pure $ pure unit
    pure $ render product

render :: Maybe (Either Error (Array Product)) -> JSX
render product =
  React.fragment
    [ DOM.header_
        [ DOM.div
            { className: "container"
            , children:
                [ DOM.a
                    { id: "logo-container"
                    , href: "/"
                    , children:
                        [ DOM.img { src: logo } ]
                    }
                ]
            }
        ]
    , DOM.div
        { id: "front-page"
        , children:
            [ DOM.div
                { className: "full-height-container"
                , children:
                    [ DOM.div
                        { className: "valigned-content"
                        , children:
                            [ DOM.div
                                { className: "container"
                                , children:
                                    [ DOM.h1_ [ DOM.text "Alla KSF Medias tidningar i en app!" ]
                                    , DOM.p_ [ DOM.text "Med HBL 365 kan du läsa den digitala versionen av papperstidningarna Hufvudstadsbladet, Västra Nyland och Östnyland i en och samma app." ]
                                    ]
                                }
                            ]
                        }
                    , DOM.div
                        { id: "sign-up-box-wrapper"
                        , className: "container"
                        , children: [ renderProduct product ]
                        }
                    ]
                }
            , DOM.div
                { id: "what-is-hbl-365"
                , className: "full-width-container text-center"
                , children:
                    [ DOM.div
                        { className: "container tight"
                        , children:
                            [ DOM.h2_ [ DOM.text "Vad är HBL 365?" ]
                            , DOM.p
                                { className: "large-text"
                                , children: [ DOM.text "Appen HBL 365 är designad speciellt för e-tidningarna. I appen hittar man lätt de senaste numren av tidningarna Hufvudstadsbladet, Västra Nyland och Östnyland. Man kan läsa tidningsartiklarna i textvy, söka artiklar och tidningar, och till och med lyssna på artiklar." ]
                                }
                            , DOM.div
                                { className: "app-store-icons"
                                , children:
                                    [ imgLink "https://itunes.apple.com/fi/app/ehbl/id436106377?mt=8" "Apple" appStore.apple
                                    , imgLink "https://play.google.com/store/apps/details?id=fi.hbl.areader" "Android" appStore.android
                                    ]
                                }
                            ]
                        }
                    ]
                }
            ]
        }
    , DOM.footer_
        [ DOM.div
            { className: "container"
            , children:
                [ DOM.ul
                    { id: "footer-links"
                    , children:
                        [ liLink "https://prenumerera.ksfmedia.fi/" "Prenumerera"
                        , liLink "https://www.hbl.fi/kundservice" "Kundservice"
                        , liLink "https://www.hbl.fi/kontakt" "Ta kontakt"
                        , liLink "https://prenumerera.ksfmedia.fi/" "Se alla våra produkter"
                        ]
                    }
                , DOM.div
                    { id: "brands"
                    , children:
                        [  imgLink "https://www.hbl.fi" "Hufvudstadsbladet" "https://cdn.ksfmedia.fi/prenumerera.ksfmedia.fi/images/hbl.png"
                        , imgLink "https://www.vastranyland.fi" "Västra Nyland" "https://cdn.ksfmedia.fi/prenumerera.ksfmedia.fi/images/vn.png"
                        , imgLink "https://www.ostnyland.fi" "Östra Nyland" "https://cdn.ksfmedia.fi/prenumerera.ksfmedia.fi/images/on.png"
                        ]
                    }
                ]
            }
        ]
    ]
  where
    renderProduct Nothing =
      DOM.div
        { className: "vetrina--container"
        , children:
            [ NewPurchase.descriptionBox Spinner.loadingSpinner ]
        }
    renderProduct (Just prod) =
      Vetrina.vetrina
        { onClose: Nothing
        , onLogin: mempty
        , products: prod
        , unexpectedError: DOM.text "Något gick fel"
        , accessEntitlements: Set.singleton "hbl-365"
        , headline: Nothing
        , paper: Nothing
        , paymentMethods: [ User.CreditCard ]
        , loadingContainer: Just NewPurchase.descriptionBox
        , customNewPurchase: Just NewPurchase.render
        , orderSource: PrenumereraSource -- TODO: find out if there is a more suitable value for this
        , subscriptionExists: anotherAccount
        , askAccountAlways: false
        , user: Nothing
        }

    imgLink href alt src =
      DOM.a
        { rel: "noopener"
        , href
        , target: "_blank"
        , children: [ DOM.img { src, alt } ]
        }

    liLink :: String -> String -> JSX
    liLink href text = DOM.li_ [ DOM.a { href, children: [ DOM.text text ] } ]
