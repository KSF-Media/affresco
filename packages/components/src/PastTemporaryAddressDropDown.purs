module KSF.PastTemporaryAddressDropDown where

import Prelude (Unit, map, show, ($), (<$>), (>>>), (<<<), (>=>))

import Control.Bind
import Data.Enum (enumFromTo)
import Data.Function (flip)
import Data.Int (fromString)
import Data.Foldable (class Foldable)
import Data.List (zip, length, fromFoldable)
import Data.Map as Map
import Data.Maybe (Maybe)
import Data.Tuple (Tuple (..))
import Effect (Effect)
import KSF.AddressChange (AddressChange) -- TODO
import KSF.InputField (inputLabel)
import React.Basic (JSX)
import React.Basic.DOM as DOM
import React.Basic.DOM.Events (targetValue)
import React.Basic.Events (handler, unsafeEventFn)


pastTemporaryAddressDropDown :: forall f. Foldable f => f AddressChange -> (Maybe AddressChange -> Effect Unit) -> JSX
pastTemporaryAddressDropDown pastAddresses onChange =
  let addresses = fromFoldable pastAddresses
      addrIndexed = Map.fromFoldable $ zip (enumFromTo 1 $ length addresses) addresses
      toAddress x = x >>= (fromString >=> flip Map.lookup addrIndexed) in
  DOM.div
    { className: "input-field--container"
    , children:
        [ inputLabel { label: "past temp addr", nameFor: "pastTemp" }
        , DOM.select
            { id: "pastTemp"
            , children: map createOption $ Map.toUnfoldable $ (show <<< _.streetAddress) <$> addrIndexed
            , onChange: handler (targetValue >>> unsafeEventFn toAddress) onChange
            }
        ]
    }
  where
    createOption (Tuple k v) =
      DOM.option
        { value: show k
        , children: [ DOM.text v ]
        }
