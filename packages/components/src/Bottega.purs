module Bottega where

import Prelude

import Data.Function.Uncurried (Fn4, runFn4)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Maybe (Maybe(..))
import Effect.Aff (Aff)
import Effect.Aff.Compat (EffectFnAff, fromEffectFnAff)
import Foreign (Foreign, F, unsafeToForeign)
import Foreign.Generic.EnumEncoding (genericDecodeEnum)
import KSF.Api (UUID, UserAuth, oauthToken)
import Simple.JSON (class ReadForeign, read', readImpl, read_)

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
  callApi ordersApi "orderPost" [ unsafeToForeign newOrder ] { authorization, authUser }
  where
    authorization = oauthToken authToken
    authUser = unsafeToForeign userId

getOrder :: UserAuth -> OrderNumber -> Aff Order
getOrder { userId, authToken } orderNumber =
  callApi ordersApi "orderOrderNumberGet" [ unsafeToForeign orderNumber ] { authorization, authUser }
  where
    authorization = oauthToken authToken
    authUser = unsafeToForeign userId

payOrder :: UserAuth -> OrderNumber -> PaymentMethod -> Aff PaymentTerminalUrl
payOrder { userId, authToken } orderNumber paymentMethod =
  callApi ordersApi "orderOrderNumberPayPost" [ unsafeToForeign orderNumber, unsafeToForeign { paymentOption: show paymentMethod } ] { authorization, authUser }
  where
    authorization = oauthToken authToken
    authUser = unsafeToForeign userId

newtype OrderNumber = OrderNumber String

type Order =
  { number :: OrderNumber
  , user   :: UUID
  , status :: OrderStatus
  }

type OrderStatus =
  { state  :: String -- TODO: Use enum
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

type PaymentTerminalUrl = { paymentTerminalUrl :: String }
