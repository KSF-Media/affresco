module Bottega where

import Prelude

import Data.Function.Uncurried (Fn4, runFn4)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.String (toLower)
import Effect.Aff (Aff)
import Effect.Aff.Compat (EffectFnAff, fromEffectFnAff)
import Foreign (Foreign, unsafeToForeign)
import KSF.Api (UUID, UserAuth, oauthToken)

foreign import data Api :: Type
foreign import ordersApi :: Api

foreign import callApi_
  :: forall req res opts
   . Fn4
       Api
       String
       req
       { | opts }
       (EffectFnAff res)

callApi :: forall res opts. Api -> String -> Array Foreign -> { | opts } -> Aff res
callApi api methodName req opts =
  fromEffectFnAff (runFn4 callApi_ api methodName req opts)

createOrder :: UserAuth -> NewOrder -> Aff Order
createOrder { userId, authToken } newOrder =
  callApi ordersApi "orderPost" [ unsafeToForeign $ addPrefix newOrder ] { authorization, authUser }
  where
    authorization = oauthToken authToken
    authUser = unsafeToForeign userId

    addPrefix o =
      { buyingOptionPackage: o.packageId
      , buyingOptionPeriod: o.period
      , buyingOptionPayAmountCents: o.payAmountCents
      }

getOrder :: UserAuth -> OrderNumber -> Aff Order
getOrder { userId, authToken } orderNumber =
  callApi ordersApi "orderOrderNumberGet" [ unsafeToForeign orderNumber ] { authorization, authUser }
  where
    authorization = oauthToken authToken
    authUser = unsafeToForeign userId

payOrder :: UserAuth -> OrderNumber -> PaymentMethod -> Aff Order
payOrder { userId, authToken } orderNumber paymentMethod =
  callApi ordersApi "orderOrderNumberPayPost" [ unsafeToForeign orderNumber, unsafeToForeign { paymentOption: show paymentMethod } ] { authorization, authUser }
  where
    authorization = oauthToken authToken
    authUser = unsafeToForeign userId

newtype OrderNumber = OrderNumber String

type Order =
  { orderNumber :: OrderNumber
  , orderUser   :: UUID
  , orderStatus :: OrderStatus
  }

type OrderStatus =
  { status :: String -- TODO: Use enum
  , time   :: String
  }

type NewOrder =
  { packageId      :: String
  , period         :: Int
  , payAmountCents :: Int
  }

data PaymentMethod = CreditCard
derive instance genericProvider :: Generic PaymentMethod _
instance showProvider :: Show PaymentMethod where
  show = genericShow
