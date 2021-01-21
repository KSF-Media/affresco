module KSF.TemporaryAddressChange.DropDown where

import Prelude (Unit, map, show, ($), (<$>), (>>>), (<<<), (<>))

import Control.Bind
import Data.Enum (enumFromTo)
import Data.Function (flip)
import Data.Int as Int
import Data.Foldable (class Foldable)
import Data.List (zip, length, fromFoldable, filter)
import Data.Map as Map
import Data.Maybe (Maybe, isJust, fromMaybe)
import Data.Tuple (Tuple (..))
import Effect (Effect)
import KSF.TemporaryAddressChange.Types (AddressChange)
import KSF.InputField (inputLabel)
import React.Basic (JSX)
import React.Basic.DOM as DOM
import React.Basic.DOM.Events (targetValue)
import React.Basic.Events (handler)


pastTemporaryAddressDropDown :: forall f. Foldable f => f AddressChange -> (Maybe AddressChange -> Effect Unit) -> JSX
pastTemporaryAddressDropDown pastAddresses onChange =
  let addresses = filter (isJust <<< _.streetAddress) $ fromFoldable pastAddresses
      addrIndexed = Map.fromFoldable $ zip (enumFromTo 1 $ length addresses) addresses
      setAddress = ((flip Map.lookup addrIndexed <=< Int.fromString) =<< _) in
  DOM.div
    { className: "input-field--container"
    , style: DOM.css { "width": "100%" }
    , children:
        [ inputLabel { label: "Välj adress", nameFor: "pastTemp" }
        , DOM.select
            { id: "pastTemp"
            , children:
                [ DOM.option { value: "-", children: [ DOM.text "Ny adress" ] } ] <>
                (map createOption $ Map.toUnfoldable $ (fromMaybe "" <<< _.streetAddress) <$> addrIndexed)
            , onChange: handler targetValue (setAddress >>> onChange)
            }
        ]
    }
  where
    createOption (Tuple k v) =
      DOM.option
        { value: show k
        , children: [ DOM.text v ]
        }
