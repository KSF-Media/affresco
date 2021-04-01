module Bottega where

import Prelude

import Bottega.Models (CreditCard, CreditCardId, CreditCardRegister, CreditCardRegisterNumber, NewOrder, Order, OrderNumber, PaymentMethod, PaymentMethodId, PaymentTerminalUrl, parseCreditCardRegisterState, parseOrderState)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe)
import Data.Nullable (Nullable, toMaybe, toNullable)
import Data.Show.Generic (genericShow)
import Data.Traversable (traverse)
import Effect.Aff (Aff)
import Foreign (unsafeToForeign)
import KSF.Api (UUID, UserAuth, oauthToken)
import KSF.Api.Error (ServerError)
import KSF.Api.Package (Package)
import OpenApiClient (Api, callApi)

foreign import ordersApi :: Api
foreign import packagesApi :: Api
foreign import paymentMethodsApi :: Api

type ApiOrder =
  { number :: OrderNumber
  , user :: UUID
  , status :: ApiOrderStatus
  }

type ApiOrderStatus =
  { state :: String
  , time :: String
  , failReason :: Nullable String
  }

type ApiCreditCard =
  { id :: CreditCardId
  , user :: UUID
  , paymentMethodId :: PaymentMethodId
  , maskedPan :: String
  , expiryDate :: String
  }

type ApiCreditCardRegister =
   { number :: CreditCardRegisterNumber
   , user :: UUID
   , creditCardId :: CreditCardId
   , paymentTerminalUrl :: Nullable PaymentTerminalUrl
   , status :: ApiCreditCardRegisterStatus
   }

type ApiCreditCardRegisterStatus =
  { state :: String
  , time :: String
  , failReason :: Nullable String
  }

type InsufficientAccount = ServerError
  ( missing_address_details ::
    { message :: String }
  )

-- TODO: Add more errors!
data BottegaError
  = BottegaInsufficientAccount -- ^ Cannot create order due to missing account data
  | BottegaUnexpectedError String

derive instance genericBottegaError :: Generic BottegaError _
instance showBottegaError :: Show BottegaError where
  show = genericShow

bottegaErrorMessage :: BottegaError -> String
bottegaErrorMessage (BottegaUnexpectedError errMsg) = errMsg
bottegaErrorMessage e = show e

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

readOrder :: ApiOrder -> Aff Order
readOrder orderObj = do
  let state = parseOrderState orderObj.status.state (toMaybe orderObj.status.failReason)
  pure $ { number: orderObj.number, user: orderObj.user, status: { state, time: orderObj.status.time }}

type PaymentTerminalUrlApi = Nullable { paymentTerminalUrl :: Nullable String }

payOrder :: UserAuth -> OrderNumber -> PaymentMethod -> Aff (Maybe PaymentTerminalUrl)
payOrder { userId, authToken } orderNumber paymentMethod = do
  nullableTerminalUrl :: PaymentTerminalUrlApi <- callApi ordersApi "orderOrderNumberPayPost" [ unsafeToForeign orderNumber, unsafeToForeign { paymentOption: show paymentMethod } ] { authorization, authUser }
  pure do
    urlObject <- toMaybe nullableTerminalUrl
    url <- toMaybe urlObject.paymentTerminalUrl
    pure { paymentTerminalUrl: url }
  where
    authorization = oauthToken authToken
    authUser = unsafeToForeign userId

getPackages :: Aff (Array Package)
getPackages = callApi packagesApi "packageGet" [] {}

readCreditCard :: ApiCreditCard -> Aff CreditCard
readCreditCard creditCardObj = pure $
  { id: creditCardObj.id, user: creditCardObj.user, paymentMethodId: creditCardObj.paymentMethodId, maskedPan: creditCardObj.maskedPan, expiryDate: creditCardObj.expiryDate }

readCreditCardRegister :: ApiCreditCardRegister -> Aff CreditCardRegister
readCreditCardRegister creditCardRegisterObj = do
  let state = parseCreditCardRegisterState creditCardRegisterObj.status.state (toMaybe creditCardRegisterObj.status.failReason)
  pure $ { number: creditCardRegisterObj.number, user: creditCardRegisterObj.user, creditCardId: creditCardRegisterObj.creditCardId, terminalUrl: toMaybe creditCardRegisterObj.paymentTerminalUrl, status: { state, time: creditCardRegisterObj.status.time }}

getCreditCards :: UserAuth -> Aff (Array CreditCard)
getCreditCards { userId, authToken } = do
  traverse readCreditCard =<< callApi paymentMethodsApi "paymentMethodCreditCardGet" [ ] { authorization, authUser }
  where
    authorization = oauthToken authToken
    authUser = unsafeToForeign userId

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

registerCreditCard :: UserAuth -> Aff CreditCardRegister
registerCreditCard { userId, authToken } =
  readCreditCardRegister =<< callApi paymentMethodsApi "paymentMethodCreditCardRegisterPost" [ ] { authorization, authUser }
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
