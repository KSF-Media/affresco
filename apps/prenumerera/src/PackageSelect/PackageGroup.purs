module Prenumerera.PackageSelect.PackageGroup where

import Prelude

import Data.Array.NonEmpty as NonEmpty
import Data.Array.NonEmpty (NonEmptyArray, foldr1)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..), fst)
import Effect (Effect)
import Effect.Aff as Aff
import Effect.Class (liftEffect)
import KSF.Helpers as Helpers
import Prenumerera.Package (Package, PackageId)
import Prenumerera.PackageDescription (Description)
import React.Basic (JSX)
import React.Basic.DOM as DOM
import React.Basic.DOM.Events (preventDefault)
import React.Basic.Events (handler)
import React.Basic.Hooks (Component, useState', (/\))
import React.Basic.Hooks as React

type WithDescription = Tuple Package Description

imgRoot :: String
imgRoot = "https://cdn.ksfmedia.fi/prenumerera.ksfmedia.fi/images/products/"

type Props =
  { packages :: NonEmptyArray WithDescription
  , startPurchase :: Package -> Description -> Effect Unit
  }

component :: Component Props
component = do
  React.component "PackageGroup" \{ packages, startPurchase } -> React.do
    activePackage /\ setActivePackage <- useState' $ _.id $ fst $ NonEmpty.head packages
    transitionPackage /\ setTransitionPackage <- useState' activePackage
    fade /\ setFade <- useState' false
    let startActivePackageFade id = when (not fade) do
          setFade true
          setTransitionPackage id
          Aff.launchAff_ do
            Aff.delay $ Aff.Milliseconds 150.0
            liftEffect do
              setFade false
              setActivePackage id
    let packageHeader = renderHeader packages transitionPackage startActivePackageFade
    pure $ render packageHeader activePackage (NonEmpty.toArray packages) fade startPurchase

renderHeader :: NonEmptyArray WithDescription -> PackageId -> (PackageId -> Effect Unit) -> JSX
renderHeader packages activePackage setActivePackage =
  DOM.div
    { className: "package-header"
    , children:
        [ DOM.h2_ [ DOM.text $
                      if NonEmpty.length packages == 1
                        then firstDesc.brandLong <> " " <> firstDesc.descriptionShort
                        else firstDesc.packageGroup
                  ]
          -- Weekday selection or display
        , DOM.ul
            { className: "nav nav-pills nav-plain-text nav-package"
            , children: if NonEmpty.length packages == 1
                          then [ DOM.li
                                   { className: "single"
                                   , children: [ DOM.text firstDesc.weekdays ]
                                   }
                               ]
                          else NonEmpty.toArray $ map displaySelectPackage packages
            }
        ]
    }
  where
    (Tuple _ firstDesc) = NonEmpty.head packages
    displaySelectPackage (Tuple package description) =
      DOM.li
        { className: if package.id == activePackage then "active" else ""
        , children:
            [ DOM.a
                { onClick: handler preventDefault $ const $ setActivePackage package.id
                , children: [ DOM.text $ description.weekdays ]
                , href: "#"
                }
            ]
        }

render :: JSX -> PackageId -> Array WithDescription -> Boolean -> (Package -> Description -> Effect Unit) -> JSX
render packageHeader activePackage packages fade startPurchase =
  DOM.div
    { className: "ksf-package-group"
    , children:
        [ DOM.div
            { className: "package-box text-center"
            , children:
                [ packageHeader
                , DOM.div
                    { className: "tab-content"
                    , children: map renderPackage packages
                    }
                ]
            }
        ]
    }
  where
    renderPackage (Tuple package description) =
      DOM.div
        { className: "tab-pane fade"
          <> (if package.id /= activePackage then "" else
                if fade then " active" else " active in")
        , children:
            [ DOM.div
                { className: "package-price"
                , children:
                    [ DOM.div
                        { className: "package-ribbon"
                        , children:
                            [ DOM.p_ [ description.ribbon ] ]
                        }
                    , DOM.img { src: imgRoot <> description.image }
                    , DOM.div
                        { className: "price-container"
                        , children:
                            [ DOM.div
                                { className: "price-bubble"
                                , children:
                                    [ DOM.span_ [ DOM.text $ Helpers.formatEur $
                                                    foldr1 min $ map _.monthlyPrice package.offers ]
                                    , DOM.br {}
                                    , DOM.text "euro/mån"
                                    ]
                                }
                            ]
                        }
                    ]
                }
            , case description.url of
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
                      , onClick: handler preventDefault $ \_ -> startPurchase package description
                      , href: "#"
                      }
            , DOM.div
                { className: "details"
                , children: [ description.descriptionLong ]
                }
            ]
        }


{-
render :: JSX -> WithDescription -> Boolean -> (Package -> Effect Unit) -> JSX
render packageHeader activePackage fade startPurchase =
  DOM.div
    { className: "ksf-package-group"
    , children:
        [ DOM.div
            { className: "package-box text-center"
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
                                                 [ DOM.span_ [ DOM.text $ maybe "?" Helpers.formatEur $ minimum $ map (_.monthlyPrice) $ (_.offers $ fst $ activePackage) ]
                                                 , DOM.br {}
                                                 , DOM.text "euro/mån"
                                                 ]
                                             }
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
                                  , onClick: handler preventDefault $ \_ -> startPurchase $ fst $ activePackage
                                  , href: "#"
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
-}
