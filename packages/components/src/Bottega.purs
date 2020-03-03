module Bottega where

import OpenApiClient
import Prelude

import Data.Either (Either(..))
import Data.Function.Uncurried (Fn4, runFn4)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Effect.Aff (Aff)
import Foreign (Foreign, unsafeToForeign)
import KSF.Api (UUID, UserAuth, oauthToken)
import KSF.Api.Package (Package)
import Simple.JSON (class ReadForeign, read, readImpl)

foreign import ordersApi :: Api
foreign import packagesApi :: Api

createOrder :: UserAuth -> NewOrder -> Aff Order
createOrder { userId, authToken } newOrder =
  callApi ordersApi "orderPost" [ unsafeToForeign newOrder ] { authorization, authUser }
  where
    authorization = oauthToken authToken
    authUser = unsafeToForeign userId

getOrder :: UserAuth -> OrderNumber -> Aff Order
getOrder { userId, authToken } orderNumber = do
  orderObj <- callApi ordersApi "orderOrderNumberGet" [ unsafeToForeign orderNumber ] { authorization, authUser }
  readOrder orderObj
  where
    authorization = oauthToken authToken
    authUser = unsafeToForeign userId
    readOrder :: { number :: OrderNumber, user :: UUID, status :: { state :: Foreign, time :: String } } -> Aff Order
    readOrder orderObj = do
      orderStatus <- case read orderObj.status.state of
        Right status -> pure status
        Left err     -> pure UnknownState
      pure { number: orderObj.number
           , user: orderObj.user
           , status:
               { state: orderStatus
               , time: orderObj.status.time
               }
           }

payOrder :: UserAuth -> OrderNumber -> PaymentMethod -> Aff PaymentTerminalUrl
payOrder { userId, authToken } orderNumber paymentMethod =
  callApi ordersApi "orderOrderNumberPayPost" [ unsafeToForeign orderNumber, unsafeToForeign { paymentOption: show paymentMethod } ] { authorization, authUser }
  where
    authorization = oauthToken authToken
    authUser = unsafeToForeign userId

getPackages :: Aff (Array Package)
getPackages = callApi packagesApi "packageGet" [] {}

newtype OrderNumber = OrderNumber String

type Order =
  { number :: OrderNumber
  , user   :: UUID
  , status :: OrderStatus
  }

type OrderStatus =
  { state  :: OrderStatusState
  , time   :: String
  }

data OrderStatusState
  = OrderCreated
  | OrderStarted
  | OrderCompleted
  | OrderFailed
  | UnknownState

derive instance genericOrderStatusState :: Generic OrderStatusState _
instance readOrderStatusState :: ReadForeign OrderStatusState where
  readImpl foreignOrderStatusState = do
    orderStatusStateString :: String <- readImpl foreignOrderStatusState
    case orderStatusStateString of
      "created"   -> pure OrderCreated
      "started"   -> pure OrderStarted
      "completed" -> pure OrderCompleted
      "failed"    -> pure OrderFailed
      _           -> pure UnknownState

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
