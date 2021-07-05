module Prenumerera.ProductSelect where

import Prelude

import Bottega as Bottega
import Data.Array as Array
import Data.Array.NonEmpty as NonEmpty
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Map as Map
import Data.Foldable (find, foldMap)
import Data.Maybe
import Data.String.Common (toLower)
import Data.Tuple
import Effect (Effect)
import Effect.Aff as Aff
import Effect.Class (liftEffect)
import KSF.Api.Package (Package)
import KSF.Paper (Paper(..), paperName)
import KSF.Paper as Paper
import KSF.User (User)
import Prenumerera.PackageDescription as PackageDescription
import Prenumerera.PackageDescription (Description, packageDescriptions)
import Prenumerera.ProductGroup as ProductGroup
import React.Basic (JSX)
import React.Basic.DOM as DOM
import React.Basic.DOM.Events (preventDefault)
import React.Basic.Events (handler)
import React.Basic.Hooks (Component, useEffectOnce, useState', (/\))
import React.Basic.Hooks as React
import Routing.PushState (LocationState, PushStateInterface, locations, makeInterface)
import Debug
import Effect.Console (log)

type Props =
  { user :: Maybe User
  , nav :: PushStateInterface
  , packages :: Array Package
  , startPurchase :: Package -> Effect Unit
  , setBrand :: Paper -> Effect Unit
  }

packagesByPaper :: (NonEmptyArray (NonEmptyArray (Tuple Package Description)) -> JSX) -> Array Package -> Array (Tuple Paper JSX)
packagesByPaper buildGroupElements =
  -- Pair with a description
  map (\p -> Tuple p <$> Map.lookup p.id PackageDescription.packageDescriptions) >>> Array.catMaybes >>>
  -- Group by brand in description
  Array.sortWith (_.brand <<< snd) >>> Array.groupBy (\a b -> (_.brand $ snd a) == (_.brand $ snd b)) >>>
  -- Group by product type
  map (NonEmpty.sortWith (_.packageGroup <<< snd) >>>
       NonEmpty.groupBy (\a b -> (_.packageGroup $ snd a) == (_.packageGroup $ snd b)) >>>
       -- Sort each group by ordering number
       map (NonEmpty.sortWith (_.ordering <<< snd)) >>>
       NonEmpty.sortWith (_.ordering <<< snd <<< NonEmpty.head)) >>>
  -- Construct tuples
  map (\packages -> let groupElements = buildGroupElements packages
                        { brand } = snd $ NonEmpty.head $ NonEmpty.head packages
                    in Tuple brand groupElements)


component :: Component Props
component = do
  productGroupComponent <- ProductGroup.component
  let papers = [HBL, VN, ON, JUNIOR]
  React.component "ProductSelect" $ \ { user, nav, packages, startPurchase, setBrand } -> React.do
    let buildGroupElements =
          map (\packages -> productGroupComponent { startPurchase, packages }) >>>
          NonEmpty.toArray >>> React.fragment
        packagePapers :: Array (Tuple Paper JSX)
        packagePapers = packagesByPaper buildGroupElements packages
    activePaper /\ setActivePaper <- useState' HBL
    fade /\ setFade <- useState' false
    transitionPaper /\ setTransitionPaper <- useState' activePaper
    -- A bit smoother render
    firstRender /\ setFirstRender <- useState' true
    useEffectOnce do
      Aff.launchAff_ do
        Aff.delay $ Aff.Milliseconds 1.0
        liftEffect $ setFirstRender false
      pure $ pure unit
    let setPaper paper = do
          setBrand paper
          setFade true
          setTransitionPaper paper
          Aff.launchAff_ do
            Aff.delay $ Aff.Milliseconds 150.0
            liftEffect do
              setFade false
              setActivePaper paper
              log $ "activePaper " <> Paper.toString paper
        selectPaper = renderSelectPaper transitionPaper papers fade setPaper
    pure $ render firstRender selectPaper fade activePaper packagePapers

renderSelectPaper :: Paper -> Array Paper -> Boolean -> (Paper -> Effect Unit) -> JSX
renderSelectPaper selected papers fade setPaper =
  DOM.ul
    { className: "nav nav-pills nav-pills-brand nav-plain-text"
    , children:
        [ DOM.li_ [ DOM.div
                      { className: "choose"
                      , children: [ DOM.text "Välj tidning" ]
                      }
                  ]
        ] <> map renderPaper papers
    }
  where
    renderPaper paper =
      DOM.li
        { className: if paper == selected then "active" else ""
        , onClick: handler preventDefault $ \_ ->
           if not fade && paper /= selected then setPaper paper else pure unit
        , children:
            [ DOM.a
                { className: toLower (Paper.toString paper)
                , children: [ DOM.text $ paperName paper ]
                , href: "#"
                }
            ]
        }

render :: Boolean -> JSX -> Boolean -> Paper -> (Array (Tuple Paper JSX)) -> JSX
render firstRender selectPaper fade activePaper productGroups =
  DOM.div
    { className: "container-fluid ksf-block-productmatrix"
    , children:
        [ DOM.div
            { className: "container ksf-block"
            , children:
                [ DOM.h2
                    { className: "hero-text"
                    , children: [ DOM.span_ [ DOM.text "Kvalitetsjournalistik" ]
                                , DOM.text " när, var och hur du vill"
                                ]
                    }
                , DOM.h3_ [ DOM.text "Välj det paket som passar dig bäst!" ]
                , DOM.div
                    { className: "ksf-block-products"
                    , children:
                        [ selectPaper
                        , DOM.div
                            { className: "tab-content ksf-products"
                            , children: map renderProductGroup productGroups
                            }
                        ]
                    }
                ]
            }
        ]
    }
  where
    renderProductGroup (Tuple paper content) =
      DOM.div
        { className: "tab-pane fade"
          <> (if not firstRender && paper == activePaper then " active" else "")
          <> (if paper == activePaper && not fade then " in" else "")
        , children:
            [ DOM.div
                { className: "row is-flex"
                , children: [ content ]
                }
            ]
        }
