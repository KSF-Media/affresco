module Bottega where

import Prelude

import Bottega.Models (CreditCard, CreditCardId, CreditCardRegister, CreditCardRegisterNumber, NewOrder, Order, OrderNumber, PaymentMethod(..), PaymentMethodId, PaymentTerminalUrl, RegisterCallback(..), parseCreditCardRegisterState, parseOrderState)
import Bottega.Models.Order (fromOrderSource)
import Data.Array (mapMaybe)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..))
import Data.Nullable (Nullable, toMaybe, toNullable)
import Data.Show.Generic (genericShow)
import Data.Traversable (traverse)
import Data.UUID (UUID)
import Effect.Aff (Aff)
import Foreign (unsafeToForeign)
import KSF.Api (UserAuth, oauthToken)
import KSF.Api.Error (ServerError)
import KSF.Api.Package (Package, fromJSCampaign)
import OpenApiClient (Api, callApi)
import Unsafe.Coerce (unsafeCoerce)

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

data IdentificationError
  = StrongIdentificationWindowOpenFailed
  | StrongIdentificationFailed String

derive instance genericIdentificationError :: Generic IdentificationError _
instance showIdentificationError :: Show IdentificationError where
  show = genericShow

derive instance eqIdentificationError :: Eq IdentificationError

-- TODO: Add more errors!
data BottegaError
  = BottegaInsufficientAccount -- ^ Cannot create order due to missing account data
  | BottegaTimeout
  | BottegaIdentificationError IdentificationError
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
  readOrder =<< callApi ordersApi "orderPost" [ unsafeToForeign newOrder { campaignNo = nullableCampaignNo, orderSource = nullableOrderSource } ] { authorization, authUser }
  where
    -- NOTE/REMINDER: We don't want send Maybes to the server,
    -- as they will be sent as objects
    nullableCampaignNo = toNullable campaignNo
    nullableOrderSource = toNullable $ map fromOrderSource orderSource
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
  nullableTerminalUrl :: PaymentTerminalUrlApi <- callApi ordersApi "orderOrderNumberPayPost" [ unsafeToForeign orderNumber, unsafeToForeign { paymentOption } ] { authorization, authUser }
  pure do
    urlObject <- toMaybe nullableTerminalUrl
    url <- toMaybe urlObject.paymentTerminalUrl
    pure { paymentTerminalUrl: url }
  where
    authorization = oauthToken authToken
    authUser = unsafeToForeign userId
    paymentOption = case paymentMethod of
      CreditCard -> "CreditCard"
      PaperInvoice -> "PaperInvoiceWithIdentification"

-- Tells Bottega to call Persona.  It's not going to trust data the
-- data a client sends on this.
userVerified :: UserAuth -> OrderNumber -> Aff Unit
userVerified { userId, authToken } orderNumber = do
  callApi ordersApi "orderOrderNumberIdentifiedGet"
    [ unsafeToForeign orderNumber ] { authorization, authUser }
  where
    authorization = oauthToken authToken
    authUser = unsafeToForeign userId

getPackages :: Aff (Array Package)
getPackages = do
  map (\package -> package { campaigns = mapMaybe fromJSCampaign package.campaigns })
    <$> callApi packagesApi "packageGet" [] {}

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

registerCreditCardProcess :: String -> Aff Unit
registerCreditCardProcess transactionId = do
--  callApi paymentMethodsApi "paymentMethodCreditCardRegisterProcessGet" [ unsafeToForeign transactionID, unsafeToForeign "OK" ] {}
  callApi paymentMethodsApi "paymentMethodCreditCardRegisterProcessGet" [] {transactionId, responseCode: "OK"}

registerCreditCardFromExisting :: UserAuth -> CreditCardId -> Aff CreditCardRegister
registerCreditCardFromExisting { userId, authToken } creditCardId =
  readCreditCardRegister =<< callApi paymentMethodsApi "paymentMethodCreditCardIdRegisterPost" [ unsafeToForeign creditCardId ] { authorization, authUser }
  where
    authorization = oauthToken authToken
    authUser = unsafeToForeign userId

registerCreditCardForSubscription :: UserAuth -> Maybe RegisterCallback -> Int -> Aff CreditCardRegister
registerCreditCardForSubscription { userId, authToken } registerCallback subsno =
  readCreditCardRegister =<< callApi paymentMethodsApi "paymentMethodCreditCardSubscriptionSubsnoRegisterPost" [ unsafeToForeign subsno ] opts
  where
    authorization = oauthToken authToken
    authUser = unsafeToForeign userId
    opts = case registerCallback of
      Nothing -> unsafeCoerce { authorization, authUser }
      Just x ->
        { authorization
        , authUser
        , callback: case x of
          MittKonto -> "MittKonto"
          Kort -> "Kort"
        }

getCreditCardRegister :: UserAuth -> CreditCardId -> CreditCardRegisterNumber -> Aff CreditCardRegister
getCreditCardRegister { userId, authToken } creditCardId creditCardRegisterNumber = do
  readCreditCardRegister =<< callApi paymentMethodsApi "paymentMethodCreditCardIdRegisterNumberGet" [ unsafeToForeign creditCardId, unsafeToForeign creditCardRegisterNumber ] { authorization, authUser }
  where
    authorization = oauthToken authToken
    authUser = unsafeToForeign userId
