module Bottega where

import Prelude

import Bottega.Models
import Bottega.Models.PaymentMethod as PaymentMethod
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Maybe (Maybe, maybe)
import Data.Nullable (Nullable, toMaybe, toNullable)
import Effect.Aff (Aff)
import Foreign (unsafeToForeign)
import KSF.Api (UUID, UserAuth, oauthToken)
import KSF.Api.Package (Package)
import OpenApiClient (Api, callApi)

foreign import ordersApi :: Api
foreign import packagesApi :: Api

createOrder :: UserAuth -> NewOrder -> Aff Order
createOrder { userId, authToken } newOrder@{ campaignNo } =
  readOrder =<< callApi ordersApi "orderPost" [ unsafeToForeign newOrder { campaignNo = nullableCampaignNo } ] { authorization, authUser }
  where
    -- NOTE/REMINDER: We don't want send Maybes to the server,
    -- as they will be sent as objects
    nullableCampaignNo = toNullable campaignNo
    authorization = oauthToken authToken
    authUser = unsafeToForeign userId

getOrder :: UserAuth -> OrderNumber -> Aff Order
getOrder { userId, authToken } orderNumber = do
  readOrder =<< callApi ordersApi "orderOrderNumberGet" [ unsafeToForeign orderNumber ] { authorization, authUser }
  where
    authorization = oauthToken authToken
    authUser = unsafeToForeign userId

readOrder :: { number :: OrderNumber, user :: UUID, status :: { state :: String, time :: String, failReason :: Nullable String } } -> Aff Order
readOrder orderObj = do
  let state = parseStatus orderObj.status.state (toMaybe orderObj.status.failReason)
  pure $ { number: orderObj.number, user: orderObj.user, status: { state, time: orderObj.status.time }}

payOrder :: UserAuth -> OrderNumber -> PaymentMethod.PaymentMethod -> Aff PaymentTerminalUrl
payOrder { userId, authToken } orderNumber paymentMethod =
  callApi ordersApi "orderOrderNumberPayPost" [ unsafeToForeign orderNumber, unsafeToForeign { paymentOption: show paymentMethod } ] { authorization, authUser }
  where
    authorization = oauthToken authToken
    authUser = unsafeToForeign userId

getPackages :: Aff (Array Package)
getPackages = callApi packagesApi "packageGet" [] {}