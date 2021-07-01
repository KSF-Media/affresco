module Prenumerera.ProductSelect where

import Prelude

import Bottega as Bottega
import Data.Array as Array
import Data.Array.NonEmpty as NonEmpty
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Map as Map
import Data.Foldable (find)
import Data.Maybe
import Data.Tuple
import Effect (Effect)
import Effect.Aff as Aff
import Effect.Class (liftEffect)
import KSF.Api.Package (Package)
import KSF.User (User)
import Prenumerera.PackageDescription as PackageDescription
import Prenumerera.PackageDescription (Description, packageDescriptions)
import Prenumerera.ProductGroup as ProductGroup
import React.Basic (JSX)
import React.Basic.DOM as DOM
import React.Basic.Events (handler_)
import React.Basic.Hooks (Component, useState', (/\))
import React.Basic.Hooks as React
import Routing.PushState (LocationState, PushStateInterface, locations, makeInterface)

type Props =
  { user :: Maybe User
  , nav :: PushStateInterface
  , packages :: Array Package
  , startPurchase :: Package -> Effect Unit
  }

type PackagePaper =
  { brand :: String
  , brandLong :: String
  , packages :: NonEmptyArray (NonEmptyArray (Tuple Package Description))
  }

packagesByPaper :: Array Package -> Array PackagePaper
packagesByPaper =
  -- Pair with a description
  map (\p -> Tuple p <$> Map.lookup p.id PackageDescription.packageDescriptions) >>> Array.catMaybes >>>
  -- Group by brand in description
  Array.sortWith (_.brand <<< snd) >>> Array.groupBy (\a b -> (_.brand $ snd a) == (_.brand $ snd b)) >>>
  -- Group by product type
  map (NonEmpty.sortWith (_.brand <<< snd) >>> NonEmpty.groupBy (\a b -> (_.brand $ snd a) == (_.brand $ snd b)) >>>
       -- Sort each group by ordering number
       map (NonEmpty.sortWith (_.ordering <<< snd)) >>>
       NonEmpty.sortWith (_.ordering <<< snd <<< NonEmpty.head)) >>>
  -- Construct PackagePapers
  map (\packages -> let { brand, brandLong } = snd $ NonEmpty.head $ NonEmpty.head packages
                    in { brand, brandLong, packages })

component :: Component Props
component = do
  -- Not using KSF.Paper since it's missing Junior
  let papers =
        [ Tuple "HBL" "Hufvudstadtsbladet"
        , Tuple "ON" "Östnyland"
        , Tuple "VN" "Västra Nyland"
        , Tuple "JUNIOR" "Hbl Junior"
        ]
  productGroupComponent <- ProductGroup.component
  React.component "ProductSelect" $ \ { user, nav, packages, startPurchase } -> React.do
    fade /\ setFade <- useState' false
    activePaper /\ setActivePaper <- useState' "HBL"
    transitionPaper /\ setTransitionPaper <- useState' "HBL"
    pure $ case NonEmpty.fromArray $ packagesByPaper packages of
      Nothing -> mempty
      Just packagePapers ->
        let setPaper paper = do
              setFade true
              setTransitionPaper paper
              Aff.launchAff_ do
                Aff.delay $ Aff.Milliseconds 100.0
                liftEffect do
                  setFade false
                  setActivePaper paper
            selectPaper = renderSelectPaper transitionPaper papers fade setPaper
            activeGroup = maybe (_.packages $ NonEmpty.head packagePapers) _.packages $
                          find (\p -> p.brand == activePaper) $ packagePapers
{-
            activeGroup = fromMaybe (NonEmpty.head $ packagePapers.packages) $
                          map _.packages $

-}
            productGroups =
              map
              (\packages -> productGroupComponent { startPurchase, packages }) $
              activeGroup
        in render selectPaper fade $ React.fragment $ NonEmpty.toArray productGroups

renderSelectPaper :: String -> Array (Tuple String String) -> Boolean -> (String -> Effect Unit) -> JSX
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
    renderPaper (Tuple paper name) =
      DOM.li
        { className: if paper == selected then "active" else ""
        , children: [ DOM.text name ]
        , onClick: handler_ $ if not fade && paper /= selected then setPaper paper else pure unit
        }

render :: JSX -> Boolean -> JSX -> JSX
render selectPaper fade productGroup =
  React.fragment
   [ selectPaper
   , DOM.div
       { className: "tab-content ksf-products"
       , children:
           [ DOM.div
               { className: "tab-pane" <> if fade then " fade" else ""
               , children:
                   [ DOM.div
                       { className: "row is-flex"
                       , children: [ productGroup ]
                       }
                   ]
               }
           ]
       }
   ]
