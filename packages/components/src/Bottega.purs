module Bottega where

import Prelude

import Bottega.Models (CreditCard, CreditCardId, CreditCardRegister, CreditCardRegisterNumber, NewOrder, Order, OrderNumber, PaymentMethod, PaymentMethodId, PaymentTerminalUrl, parseCreditCardRegisterState, parseOrderState)
import Bottega.Models.Order (fromOrderSource)
import Data.Array (mapMaybe)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe)
import Data.Nullable (Nullable, toMaybe, toNullable)
import Data.Show.Generic (genericShow)
import Data.Traversable (traverse)
import Data.UUID (UUID)
import Effect.Aff (Aff)
import Foreign (unsafeToForeign)
import KSF.Api (UserAuth, oauthToken)
import KSF.Api.Error (ServerError)
import KSF.Api.Package (Package (..), fromJSCampaign)
import OpenApiClient (Api, callApi, decodeApiRes)

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
  | BottegaTimeout
  | BottegaUnexpectedError String

derive instance genericBottegaError :: Generic BottegaError _
instance showBottegaError :: Show BottegaError where
  show = genericShow

derive instance eqBottegaError :: Eq BottegaError

bottegaErrorMessage :: BottegaError -> String
bottegaErrorMessage (BottegaUnexpectedError errMsg) = errMsg
bottegaErrorMessage e = show e

createOrder :: UserAuth -> NewOrder -> Aff Order
createOrder { userId, authToken } newOrder@{ campaignNo, orderSource } =
  decodeApiRes "Order" =<< callApi ordersApi "orderPost" [ unsafeToForeign newOrder { campaignNo = nullableCampaignNo, orderSource = nullableOrderSource } ] { authorization, authUser }
  where
    -- NOTE/REMINDER: We don't want send Maybes to the server,
    -- as they will be sent as objects
    nullableCampaignNo = toNullable campaignNo
    nullableOrderSource = toNullable $ map fromOrderSource orderSource
    authorization = oauthToken authToken
    authUser = unsafeToForeign userId


getOrder :: UserAuth -> OrderNumber -> Aff Order
getOrder { userId, authToken } orderNumber = do
  decodeApiRes "Order" =<< callApi ordersApi "orderOrderNumberGet" [ unsafeToForeign orderNumber ] { authorization, authUser }
  where
    authorization = oauthToken authToken
    authUser = unsafeToForeign userId

type PaymentTerminalUrlApi = Nullable { paymentTerminalUrl :: Nullable String }

payOrder :: UserAuth -> OrderNumber -> PaymentMethod -> Aff (Maybe PaymentTerminalUrl)
payOrder { userId, authToken } orderNumber paymentMethod = do
  decodeApiRes "PaymentTermunalUrl" =<< callApi ordersApi "orderOrderNumberPayPost" [ unsafeToForeign orderNumber, unsafeToForeign { paymentOption: show paymentMethod } ] { authorization, authUser }
  -- pure do
  --   urlObject <- toMaybe nullableTerminalUrl
  --   url <- toMaybe urlObject.paymentTerminalUrl
  --   pure { paymentTerminalUrl: url }
  where
    authorization = oauthToken authToken
    authUser = unsafeToForeign userId

getPackages :: Aff (Array Package)
getPackages = decodeApiRes "Package" =<< callApi packagesApi "packageGet" [] {}

-- readCreditCard :: ApiCreditCard -> Aff CreditCard
-- readCreditCard creditCardObj = pure $
--   { id: creditCardObj.id, user: creditCardObj.user, paymentMethodId: creditCardObj.paymentMethodId, maskedPan: creditCardObj.maskedPan, expiryDate: creditCardObj.expiryDate }

-- readCreditCardRegister :: ApiCreditCardRegister -> Aff CreditCardRegister
-- readCreditCardRegister creditCardRegisterObj = do
--   let state = parseCreditCardRegisterState creditCardRegisterObj.status.state (toMaybe creditCardRegisterObj.status.failReason)
--   pure $ { number: creditCardRegisterObj.number, user: creditCardRegisterObj.user, creditCardId: creditCardRegisterObj.creditCardId, terminalUrl: toMaybe creditCardRegisterObj.paymentTerminalUrl, status: { state, time: creditCardRegisterObj.status.time }}

getCreditCards :: UserAuth -> Aff (Array CreditCard)
getCreditCards { userId, authToken } = do
  decodeApiRes "CreditCard" =<< callApi paymentMethodsApi "paymentMethodCreditCardGet" [ ] { authorization, authUser }
  where
    authorization = oauthToken authToken
    authUser = unsafeToForeign userId

getCreditCard :: UserAuth -> CreditCardId -> Aff CreditCard
getCreditCard { userId, authToken } creditCardId = do
  decodeApiRes "CreditCard" =<< callApi paymentMethodsApi "paymentMethodCreditCardIdGet" [ unsafeToForeign creditCardId ] { authorization, authUser }
  where
    authorization = oauthToken authToken
    authUser = unsafeToForeign userId

deleteCreditCard :: UserAuth -> CreditCardId -> Aff Unit
deleteCreditCard { userId, authToken } creditCardId = do
  pure unit <* callApi paymentMethodsApi "paymentMethodCreditCardIdDelete" [ unsafeToForeign creditCardId ] { authorization, authUser }
  where
    authorization = oauthToken authToken
    authUser = unsafeToForeign userId

registerCreditCard :: UserAuth -> Aff CreditCardRegister
registerCreditCard { userId, authToken } =
  decodeApiRes "CreditCardRegister" =<< callApi paymentMethodsApi "paymentMethodCreditCardRegisterPost" [ ] { authorization, authUser }
  where
    authorization = oauthToken authToken
    authUser = unsafeToForeign userId

registerCreditCardFromExisting :: UserAuth -> CreditCardId -> Aff CreditCardRegister
registerCreditCardFromExisting { userId, authToken } creditCardId =
  decodeApiRes "CreditCardRegister" =<< callApi paymentMethodsApi "paymentMethodCreditCardIdRegisterPost" [ unsafeToForeign creditCardId ] { authorization, authUser }
  where
    authorization = oauthToken authToken
    authUser = unsafeToForeign userId

getCreditCardRegister :: UserAuth -> CreditCardId -> CreditCardRegisterNumber -> Aff CreditCardRegister
getCreditCardRegister { userId, authToken } creditCardId creditCardRegisterNumber = do
  decodeApiRes "CreditCardRegister" =<< callApi paymentMethodsApi "paymentMethodCreditCardIdRegisterNumberGet" [ unsafeToForeign creditCardId, unsafeToForeign creditCardRegisterNumber ] { authorization, authUser }
  where
    authorization = oauthToken authToken
    authUser = unsafeToForeign userId

updateCreditCardSubscriptions :: UserAuth -> CreditCardId -> CreditCardId -> Aff Unit
updateCreditCardSubscriptions { userId, authToken } oldCreditCardId newCreditCardId =
  pure unit <* callApi paymentMethodsApi "paymentMethodCreditCardIdSubscriptionPut" [ unsafeToForeign oldCreditCardId, unsafeToForeign newCreditCardId ] { authorization, authUser }
  where
    authorization = oauthToken authToken
    authUser = unsafeToForeign userId
