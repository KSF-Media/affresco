module Prenumerera.Analytics.Analytics where

import Prelude

import KSF.User.Cusno (toString)
import Effect (Effect)
import Effect.Uncurried (EffectFn4, runEffectFn4)
import Data.Int (toNumber)
import Bottega.Models (PaymentMethod)
import KSF.User (User)
import Prenumerera.Package (Package, PackageOffer)

foreign import _purchase :: EffectFn4 String String String Number Unit

analyticsSendPurchase :: User -> Package -> PaymentMethod -> PackageOffer -> Effect Unit
analyticsSendPurchase user package method offer = do
    runEffectFn4 _purchase (toString user.cusno) package.id (show method) (toNumber offer.totalPrice/100.0)
