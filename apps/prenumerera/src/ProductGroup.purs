module Prenumerera.ProductGroup where

import Prelude

import Data.Array.NonEmpty as NonEmpty
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Foldable (minimum)
import Data.Maybe (Maybe(..), maybe)
import Data.Tuple (Tuple(..), fst, snd)
import Effect (Effect)
import Effect.Aff as Aff
import Effect.Class (liftEffect)
import KSF.Api.Package (Package)
import KSF.Helpers as Helpers
import Prenumerera.PackageDescription (Description)
import React.Basic (JSX)
import React.Basic.DOM as DOM
import React.Basic.Events (handler_)
import React.Basic.Hooks (Component, useState', useMemo, (/\))
import React.Basic.Hooks as React

type WithDescription = Tuple Package Description

imgRoot :: String
imgRoot = "https://cdn.ksfmedia.fi/prenumerera.ksfmedia.fi/images/products/"

type Props =
  { packages :: NonEmptyArray WithDescription
  , startPurchase :: Package -> Effect Unit
  }

component :: Component Props
component = do
  React.component "ProductGroup" \{ packages, startPurchase } -> React.do
    activePackage /\ setActivePackage <- useState' $ NonEmpty.head packages
    transitionPackage /\ setTransitionPackage <- useState' $ NonEmpty.head packages
    fade /\ setFade <- useState' false
    let startActivePackageFade package = when (not fade) do
          setFade true
          setTransitionPackage package
          Aff.launchAff_ do
            Aff.delay $ Aff.Milliseconds 100.0
            liftEffect do
              setFade false
              setActivePackage package
    packageHeader <- useMemo (_.id $ fst $ transitionPackage) $ const $
                     renderHeader packages transitionPackage startActivePackageFade
    pure $ render packageHeader activePackage fade startPurchase

renderHeader :: NonEmptyArray WithDescription -> WithDescription -> (WithDescription -> Effect Unit) -> JSX
renderHeader packages activePackage setActivePackage =
  DOM.div
    { className: "package-header"
    , children:
        [ DOM.h2_ [ DOM.text $
                      if NonEmpty.length packages == 1
                        then activeDesc.brandLong <> " " <> activeDesc.descriptionShort
                        else activeDesc.packageGroup
                  ]
          -- Weekday selection or display
        , DOM.ul
            { className: "nav nav-pills nav-plain-text nav-package"
            , children: if NonEmpty.length packages == 1
                          then [ DOM.li
                                   { className: "single"
                                   , children: [ DOM.text activeDesc.weekdays ]
                                   }
                               ]
                          else NonEmpty.toArray $ map displaySelectPackage packages
            }
        ]
    }
  where
    (Tuple _ activeDesc) = activePackage
    displaySelectPackage pkg =
      DOM.li
        { className: if (_.id $ fst pkg) == (_.id $ fst activePackage) then "active" else ""
        , children:
            [ DOM.a
                { onClick: handler_ $ setActivePackage pkg
                , children: [ DOM.text $ _.weekdays $ snd pkg ]
                }
            ]
        }

render :: JSX -> WithDescription -> Boolean -> (Package -> Effect Unit) -> JSX
render packageHeader activePackage fade startPurchase =
  DOM.div
    { className: "ksf-package-group text-center"
    , children:
        [ packageHeader
        , DOM.div
            { className: "tab-content" <> if fade then " fade" else ""
            , children:
                [ DOM.div
                    { className: "package-price"
                    , children:
                        [ DOM.div
                            { className: "package-ribbon"
                            , children:
                                [ DOM.p_ [ _.ribbon $ snd $ activePackage ] ]
                            }
                        , DOM.img { src: imgRoot <> (_.image $ snd $ activePackage) }
                        , DOM.div
                            { className: "price-container"
                            , children:
                                [ DOM.div
                                    { className: "price-bubble"
                                    , children:
                                        [ DOM.text $
                                            (maybe "?" Helpers.formatEur $ minimum $ map (_.monthlyPrice) $
                                             (_.offers $ fst $ activePackage)) <> " "
                                            <> "euro/mån"
                                        ]
                                    }
                                ]
                            }
                        , case _.url $ snd activePackage of
                              Just url ->
                                DOM.a
                                  { className: "btn btn-cta"
                                  , children: [ DOM.text "Köp nu" ]
                                  , href: url
                                  }
                              Nothing ->
                                DOM.a
                                  { className: "btn btn-cta"
                                  , children: [ DOM.text "Köp nu" ]
                                  , onClick: handler_ $ startPurchase $ fst $ activePackage
                                  }
                        , DOM.div
                            { className: "details"
                            , children: [ _.descriptionLong $ snd $ activePackage ]
                            }
                        ]
                    }
                ]
            }
        ]
    }
