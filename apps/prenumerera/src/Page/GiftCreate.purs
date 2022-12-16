module Prenumerera.Page.GiftCreate where

import Prelude

import Data.Maybe (Maybe(..), maybe)
import Effect (Effect)
import Effect.Aff as Aff
import Effect.Class (liftEffect)
import KSF.Spinner as Spinner
import KSF.User (redeemGift)
import Bottega.GiftApi as Api
import React.Basic (JSX)
import React.Basic.DOM as DOM
import React.Basic.Hooks (Component, useEffectOnce, useState', (/\))
import React.Basic.Hooks as React

-- No User, gift create will access the auth tokens on its own
type Props =
  { gift :: String
  , next :: Effect Unit
  }

component :: Component Props
component = do
  React.component "GiftCreate" $ \{ gift, next } -> React.do
    giftError /\ setGiftError <- useState' Nothing
    useEffectOnce do
      Aff.launchAff_ do
        liftEffect <<< maybe next (setGiftError <<< Just) =<< redeemGift gift
      pure $ pure unit
    pure $ case giftError of
      Nothing -> Spinner.loadingSpinner
      Just err -> renderError err

renderError :: Api.RedeemError -> JSX
renderError err =
  DOM.div
    { className: "container ksf-block"
    , children:
        [ DOM.text errMsg ]
    }
  where
    errMsg = case err of
      Api.AlreadyRedeemedByYou -> "Lunastit jo lahjakoodin"
      Api.UnknownCode -> "Tuntematon lahjakoodi"
      Api.Permission -> "Kirjautumisvirhe"
      Api.AlreadyRedeemed -> "Koodi on jo lunastettu"
      Api.NotPaid -> "Lahjakoodia ei ole maksettu"
      Api.UnknownError -> "Något gick fel"
      
