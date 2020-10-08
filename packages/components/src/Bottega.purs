module Bottega where

import Prelude

import Bottega.Models (CreditCard, CreditCardId, CreditCardRegister, CreditCardRegisterNumber, NewOrder, Order, OrderNumber, PaymentMethod, PaymentMethodId, PaymentTerminalUrl, parseCreditCardRegisterState, parseOrderState)
import Data.Nullable (Nullable, toMaybe, toNullable)
import Effect.Aff (Aff)
import Foreign (unsafeToForeign)
import KSF.Api (UUID, UserAuth, oauthToken)
import KSF.Api.Package (Package)
import OpenApiClient (Api, callApi)

foreign import ordersApi :: Api
foreign import packagesApi :: Api
foreign import paymentMethodsApi :: Api

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
  let state = parseOrderState orderObj.status.state (toMaybe orderObj.status.failReason)
  pure $ { number: orderObj.number, user: orderObj.user, status: { state, time: orderObj.status.time }}

payOrder :: UserAuth -> OrderNumber -> PaymentMethod -> Aff PaymentTerminalUrl
payOrder { userId, authToken } orderNumber paymentMethod =
  callApi ordersApi "orderOrderNumberPayPost" [ unsafeToForeign orderNumber, unsafeToForeign { paymentOption: show paymentMethod } ] { authorization, authUser }
  where
    authorization = oauthToken authToken
    authUser = unsafeToForeign userId

getPackages :: Aff (Array Package)
getPackages = callApi packagesApi "packageGet" [] {}

readCreditCard :: { id :: CreditCardId, user :: UUID, paymentMethodId :: PaymentMethodId, maskedPan :: String, expiryDate :: String } -> Aff CreditCard
readCreditCard creditCardObj = pure $ 
  { id: creditCardObj.id, user: creditCardObj.user, paymentMethodId: creditCardObj.paymentMethodId, maskedPan: creditCardObj.maskedPan, expiryDate: creditCardObj.expiryDate }

readCreditCardRegister :: { number :: CreditCardRegisterNumber, user :: UUID, terminalUrl :: Nullable PaymentTerminalUrl, status :: { state :: String, time :: String, failReason :: Nullable String }  } -> Aff CreditCardRegister
readCreditCardRegister creditCardRegisterObj = do
  let state = parseCreditCardRegisterState creditCardRegisterObj.status.state (toMaybe creditCardRegisterObj.status.failReason)
  pure $ { number: creditCardRegisterObj.number, user: creditCardRegisterObj.user, terminalUrl: toMaybe creditCardRegisterObj.terminalUrl, status: { state, time: creditCardRegisterObj.status.time }}

getCreditCard :: UserAuth -> CreditCardId -> Aff CreditCard
getCreditCard { userId, authToken } creditCardId = do
  readCreditCard =<< callApi paymentMethodsApi "paymentMethodCreditCardIdGet" [ unsafeToForeign creditCardId ] { authorization, authUser }
  where
    authorization = oauthToken authToken
    authUser = unsafeToForeign userId

deleteCreditCard :: UserAuth -> CreditCardId -> Aff Unit
deleteCreditCard { userId, authToken } creditCardId = do
  callApi paymentMethodsApi "paymentMethodCreditCardIdDelete" [ unsafeToForeign creditCardId ] { authorization, authUser }
  where
    authorization = oauthToken authToken
    authUser = unsafeToForeign userId

registerCreditCard :: UserAuth -> CreditCardId -> Aff CreditCardRegister
registerCreditCard { userId, authToken } creditCardId =
  readCreditCardRegister =<< callApi paymentMethodsApi "paymentMethodCreditCardIdRegisterPost" [ unsafeToForeign creditCardId ] { authorization, authUser }
  where
    authorization = oauthToken authToken
    authUser = unsafeToForeign userId

getCreditCardRegister :: UserAuth -> CreditCardId -> CreditCardRegisterNumber -> Aff CreditCardRegister
getCreditCardRegister { userId, authToken } creditCardId creditCardRegisterNumber = do
  readCreditCardRegister =<< callApi paymentMethodsApi "paymentMethodCreditCardIdRegisterNumberGet" [ unsafeToForeign creditCardId, unsafeToForeign creditCardRegisterNumber ] { authorization, authUser }
  where
    authorization = oauthToken authToken
    authUser = unsafeToForeign userId

updateCreditCardSubscriptions :: UserAuth -> CreditCardId -> CreditCardId -> Aff Unit
updateCreditCardSubscriptions { userId, authToken } oldCreditCardId newCreditCardId = 
  callApi paymentMethodsApi "paymentMethodCreditCardIdSubscriptionPut" [ unsafeToForeign oldCreditCardId, unsafeToForeign newCreditCardId ] { authorization, authUser }
  where
    authorization = oauthToken authToken
    authUser = unsafeToForeign userId