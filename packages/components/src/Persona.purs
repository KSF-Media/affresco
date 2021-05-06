module Persona where

import Prelude

import Control.Alt ((<|>))
import Data.Array (catMaybes)
import Data.Date (Date)
import Data.Generic.Rep (class Generic)
import Data.JSDate (JSDate, toDate)
import Data.Maybe (Maybe(..))
import Data.Nullable (Nullable, toNullable, toMaybe)
import Data.Nullable as Nullable
import Data.Show.Generic (genericShow)
import Data.String as String
import Data.String (toLower)
import Data.String.Read (class Read, read)
import Data.Traversable (sequence)
import Data.UUID (UUID)
import Data.UUID as UUID
import Effect.Aff (Aff)
import Foreign (unsafeToForeign)
import Foreign.Generic.EnumEncoding (defaultGenericEnumOptions, genericDecodeEnum, genericEncodeEnum)
import Foreign.Object (Object)
import KSF.Api (InvalidateCache, Password, Token, UserAuth, invalidateCacheHeader, oauthToken)
import KSF.Api.Address (Address)
import KSF.Api.Consent (GdprConsent, LegalConsent)
import KSF.Api.Error (ServerError)
import KSF.Api.Search (SearchQuery, SearchResult, JanrainUser, FaroUser)
import KSF.Api.Subscription (Subscription, PendingAddressChange, Subsno(..))
import KSF.Api.Subscription as Subscription
import KSF.Helpers (formatDate)
import KSF.User.Cusno (Cusno)
import OpenApiClient (Api, callApi)
import Record as Record
import Simple.JSON (class ReadForeign, class WriteForeign)

foreign import adminApi :: Api
foreign import loginApi :: Api
foreign import usersApi :: Api

login :: LoginData -> Aff LoginResponse
login loginData = callApi loginApi "loginPost" [ unsafeToForeign loginData ] {}

loginSome :: LoginDataSome -> Aff LoginResponse
loginSome loginData = callApi loginApi "loginSomePost" [ unsafeToForeign loginData ] {}

loginSso :: LoginDataSso -> Aff LoginResponse
loginSso loginData = callApi loginApi "loginSsoPost" [ unsafeToForeign loginData ] {}

-- Send authUser field only when impersonating a user
authHeaders :: UUID -> UserAuth -> { authorization :: String, authUser :: Nullable String }
authHeaders uuid { userId, authToken } =
  { authorization: oauthToken authToken
  , authUser: if uuid == userId
                then Nullable.null
                else Nullable.notNull $ UUID.toString userId
  }

getUser :: Maybe InvalidateCache -> UUID -> UserAuth -> Aff User
getUser invalidateCache uuid auth = do
  user <- callApi usersApi "usersUuidGet" [ unsafeToForeign uuid ] headers
  let parsedSubs = map Subscription.parseSubscription user.subs
  pure $ user { subs = parsedSubs }
  where
    headers = Record.merge (authHeaders uuid auth)
      { cacheControl: toNullable maybeCacheControl
      }
    maybeCacheControl = invalidateCacheHeader <$> invalidateCache

getUserEntitlements :: UserAuth -> Aff (Array String)
getUserEntitlements auth =
  callApi usersApi "usersUuidEntitlementGet" [ unsafeToForeign auth.userId ] $ authHeaders auth.userId auth

updateUser :: UUID -> UserUpdate -> UserAuth -> Aff User
updateUser uuid update auth = do
  let body = case update of
        UpdateName names          -> unsafeToForeign names
        UpdateEmail email -> unsafeToForeign email
        UpdateAddress { countryCode, zipCode, streetAddress, startDate } ->
          unsafeToForeign
            { address:
                { countryCode
                , zipCode
                , streetAddress
                , validFrom: toNullable $ formatDate <$> startDate
                }
            }
        UpdateFull userInfo ->
          unsafeToForeign
            { firstName: userInfo.firstName
            , lastName: userInfo.lastName
            , address:
                { streetAddress: userInfo.streetAddress
                , zipCode: userInfo.zipCode
                , countryCode: userInfo.countryCode
                , city: userInfo.city
                }
            }
        DeletePendingAddressChanges -> unsafeToForeign { pendingAddressChanges: [] }

  user <- callApi usersApi "usersUuidPatch" [ unsafeToForeign uuid, body ] $ authHeaders uuid auth
  let parsedSubs = map Subscription.parseSubscription user.subs
  pure $ user { subs = parsedSubs }

updateGdprConsent :: UUID -> Token -> Array GdprConsent -> Aff Unit
updateGdprConsent uuid token consentValues = callApi usersApi "usersUuidGdprPut" [ unsafeToForeign uuid, unsafeToForeign consentValues ] { authorization }
  where
    authorization = oauthToken token

updatePassword :: UUID -> Password -> Password -> UserAuth -> Aff User
updatePassword uuid password confirmPassword auth = callApi usersApi "usersUuidPasswordPut" [ unsafeToForeign uuid, unsafeToForeign { password, confirmPassword } ] $ authHeaders uuid auth

logout :: UserAuth -> Aff Unit
logout auth =
  callApi loginApi "loginUuidDelete" [ unsafeToForeign auth.userId ] { authorization }
  where
    authorization = oauthToken auth.authToken

register :: NewUser -> Aff LoginResponse
register newUser =
  callApi usersApi "usersPost" [ unsafeToForeign newUser ] {}

type NewTemporaryUser =
  { emailAddress :: Email
  , legalConsents :: Array LegalConsent
  }

registerWithEmail :: NewTemporaryUser -> Aff LoginResponse
registerWithEmail newEmailUser =
  callApi usersApi "usersTemporaryPost" [ unsafeToForeign newEmailUser ] {}

type NewCusnoUser =
  { cusno     :: Cusno
  , email     :: String
  , firstName :: String
  , lastName  :: String
  , password  :: String
  , consents  :: Array LegalConsent
  }

registerCusno :: NewCusnoUser -> UserAuth -> Aff LoginResponse
registerCusno newUser@{ cusno } auth = do
  let user =
        { cusno
        , user: { firstName: if String.null $ String.trim newUser.firstName
                               then Nullable.null
                               else Nullable.notNull newUser.firstName
                , lastName : if String.null $ String.trim newUser.lastName
                               then Nullable.null
                               else Nullable.notNull newUser.lastName
                , emailAddress: newUser.email
                , password: newUser.password
                , confirmPassword: newUser.password
                , legalConsents: newUser.consents
                }
        }
  callApi adminApi "adminUserPost" [ unsafeToForeign user ]
    ( authHeaders UUID.emptyUUID auth )

pauseSubscription :: UUID -> Subsno -> Date -> Date -> UserAuth -> Aff Subscription
pauseSubscription uuid (Subsno subsno) startDate endDate auth = do
  let startDateISO = formatDate startDate
      endDateISO   = formatDate endDate
  callApi usersApi "usersUuidSubscriptionsSubsnoPausePost"
    [ unsafeToForeign uuid
    , unsafeToForeign subsno
    , unsafeToForeign { startDate: startDateISO, endDate: endDateISO }
    ]
    ( authHeaders uuid auth )

editSubscriptionPause :: UUID -> Subsno -> Date -> Date -> Date -> Date -> UserAuth -> Aff Subscription
editSubscriptionPause uuid (Subsno subsno) oldStartDate oldEndDate newStartDate newEndDate auth = do
  let oldStartDateISO = formatDate oldStartDate
      oldEndDateISO   = formatDate oldEndDate
      newStartDateISO = formatDate newStartDate
      newEndDateISO   = formatDate newEndDate
  callApi usersApi "usersUuidSubscriptionsSubsnoPausePatch"
    [ unsafeToForeign uuid
    , unsafeToForeign subsno
    , unsafeToForeign { oldStartDate: oldStartDateISO
                      , oldEndDate: oldEndDateISO
                      , newStartDate: newStartDateISO
                      , newEndDate: newEndDateISO
                      }
    ]
    ( authHeaders uuid auth )

unpauseSubscription :: UUID -> Subsno -> UserAuth -> Aff Subscription
unpauseSubscription uuid (Subsno subsno) auth = do
  callApi usersApi "usersUuidSubscriptionsSubsnoUnpausePost"
    ([ unsafeToForeign uuid
     , unsafeToForeign subsno
     ])
    ( authHeaders uuid auth )

temporaryAddressChange
  :: UUID
  -> Subsno
  -> Date
  -> Maybe Date
  -> String
  -> String
  -> String
  -> Maybe String
  -> UserAuth
  -> Aff Subscription
temporaryAddressChange uuid (Subsno subsno) startDate endDate streetAddress zipCode countryCode temporaryName auth = do
  let startDateISO = formatDate startDate
      endDateISO   = formatDate <$> endDate

  callApi usersApi "usersUuidSubscriptionsSubsnoAddressChangePost"
    [ unsafeToForeign uuid
    , unsafeToForeign subsno
    , unsafeToForeign { startDate: startDateISO, endDate: toNullable endDateISO, streetAddress, zipCode, countryCode, temporaryName: toNullable temporaryName }
    ]
    ( authHeaders uuid auth )

editTemporaryAddressChange
  :: UUID
  -> Subsno
  -> Date
  -> Date
  -> Maybe Date
  -> UserAuth
  -> Aff Subscription
editTemporaryAddressChange uuid (Subsno subsno) oldStartDate startDate endDate auth = do
  let oldStartDateISO = formatDate oldStartDate
      startDateISO = formatDate startDate
      endDateISO = formatDate <$> endDate

  callApi usersApi "usersUuidSubscriptionsSubsnoAddressChangePatch"
    [ unsafeToForeign uuid
    , unsafeToForeign subsno
    , unsafeToForeign { oldStartDate: oldStartDateISO, newStartDate: startDateISO, newEndDate: toNullable endDateISO }
    ]
    ( authHeaders uuid auth )

deleteTemporaryAddressChange
  :: UUID
  -> Subsno
  -> Date
  -> Maybe Date
  -> UserAuth
  -> Aff Subscription
deleteTemporaryAddressChange uuid (Subsno subsno) startDate endDate auth = do
  let startDateISO = formatDate startDate
      endDateISO   = formatDate <$> endDate
  callApi usersApi "usersUuidSubscriptionsSubsnoAddressChangeDelete"
    [ unsafeToForeign uuid
    , unsafeToForeign subsno
    , unsafeToForeign { startDate: startDateISO, endDate: toNullable endDateISO  }
    ]
    ( authHeaders uuid auth )

createDeliveryReclamation
  :: UUID
  -> Subsno
  -> Date
  -> DeliveryReclamationClaim
  -> UserAuth
  -> Aff DeliveryReclamation
createDeliveryReclamation uuid (Subsno subsno) date claim auth = do
  let dateISO = formatDate date
  let claim'  = show claim
  callApi usersApi "usersUuidSubscriptionsSubsnoReclamationPost"
    [ unsafeToForeign uuid
    , unsafeToForeign subsno
    , unsafeToForeign { publicationDate: dateISO, claim: claim' }
    ]
    ( authHeaders uuid auth )

newtype Email = Email String
derive newtype instance showEmail :: Show Email
derive newtype instance readforeignEmail :: ReadForeign Email
derive newtype instance writeforeignEmail :: WriteForeign Email
derive newtype instance eqEmail :: Eq Email

type LoginResponse =
  { token :: Token
  , ssoCode :: Nullable String
  , uuid :: UUID
  , isAdmin :: Boolean
  }

type LoginData =
  { username :: String
  , password :: String
  , mergeToken :: Nullable MergeToken
  }

type LoginDataSome =
  { provider :: String
  , someToken :: Token
  , mergeToken :: Nullable MergeToken
  }

type LoginDataSso =
  { uuid :: UUID
  , accessToken :: Token
  }

data UserUpdate
  = UpdateName { firstName :: String, lastName :: String }
  | UpdateEmail { email :: String }
  | UpdateAddress { countryCode :: String
                  , zipCode :: String
                  , streetAddress :: String
                  , startDate :: Maybe Date
                  }
  | UpdateFull { firstName :: String
               , lastName :: String
               , city :: String
               , countryCode :: String
               , zipCode :: String
               , streetAddress :: String
               , startDate :: Maybe Date
               }
  | DeletePendingAddressChanges

type EmailAddressInUse = ServerError
  ( email_address_in_use ::
    { existing_provider :: Provider
    , merge_token :: MergeToken
    , description :: String
    }
  )

type TokenInvalid = ServerError
  ( login_token_expired ::
    { description :: String }
  )

type InvalidCredentials = ServerError
  ( invalid_credentials :: { description :: String } )

type InvalidFormFields = ServerError
  ( invalid_form_fields ::
       { description :: String
       , errors :: Object (Array String)
       }
  )

data InvalidPauseDateError
  = PauseInvalidStartDate
  | PauseInvalidLength
  | PauseInvalidOverlapping
  | PauseInvalidTooRecent
  -- Persona never returns PauseInvalidUnexpected
  -- We use it here only to indicate an unexpected error message
  | PauseInvalidUnexpected
derive instance genericInvalidPauseDateError :: Generic InvalidPauseDateError _
instance readInvalidPauseDateError :: ReadForeign InvalidPauseDateError where
  readImpl a = genericDecodeEnum defaultGenericEnumOptions a <|> pure PauseInvalidUnexpected

type InvalidPauseDates = ServerError
  ( invalid_pause_dates ::
    { message :: InvalidPauseDateError }
  )

data InvalidDateInput
  = InvalidStartDate
  | InvalidLength
  | InvalidOverlapping
  | InvalidTooRecent
  -- Persona never returns InvalidUnexpected
  -- We use it here only to indicate an unexpected error message
  | InvalidUnexpected

derive instance genericInvaliDateInput :: Generic InvalidDateInput _
instance readInvalidDateInput :: ReadForeign InvalidDateInput where
  readImpl a = genericDecodeEnum defaultGenericEnumOptions a <|> pure InvalidUnexpected
instance showSubscriptionError :: Show InvalidDateInput where
  show = genericShow

type InvalidDates = ServerError
  ( invalid_param ::
    { message :: InvalidDateInput }
  )

-- | TODO: These should really be fixed on the server side (we don't need both types)
pauseDateErrorToInvalidDateError :: InvalidPauseDateError -> InvalidDateInput
pauseDateErrorToInvalidDateError = case _ of
  PauseInvalidStartDate   -> InvalidStartDate
  PauseInvalidLength      -> InvalidLength
  PauseInvalidOverlapping -> InvalidOverlapping
  PauseInvalidTooRecent   -> InvalidTooRecent
  PauseInvalidUnexpected  -> InvalidUnexpected

type EmailAddressInUseRegistration = ServerError
  ( email_address_in_use_registration :: { description :: String } )

type CusnoInUseRegistration = ServerError
  ( unique_cusno_violation :: { description :: String } )

data Provider
  = Facebook
  | GooglePlus
  | Capture

derive instance genericProvider :: Generic Provider _
instance readProvider :: ReadForeign Provider where
  readImpl = genericDecodeEnum {constructorTagTransform: toLower}
instance writeProvider :: WriteForeign Provider where
  writeImpl = genericEncodeEnum {constructorTagTransform: toLower}
instance showProvider :: Show Provider where
  show = toLower <<< genericShow

newtype MergeToken = MergeToken String
derive newtype instance showMergeToken :: Show MergeToken
derive newtype instance readMergeToken :: ReadForeign MergeToken
derive newtype instance writeMergeToken :: WriteForeign MergeToken

type User = Record BaseUser

type BaseUser =
  ( uuid :: UUID
  , email :: String
  , firstName :: Nullable String
  , lastName :: Nullable String
  , address :: Nullable Address
  , cusno :: Cusno
  , subs :: Array Subscription
  , consent :: Array GdprConsent
  , pendingAddressChanges :: Nullable (Array PendingAddressChange)
  , pastTemporaryAddresses :: Array TemporaryAddressChange
  , hasCompletedRegistration :: Boolean
  )

type NewUser =
  { firstName :: String
  , lastName :: String
  , emailAddress :: String
  , password :: String
  , confirmPassword :: String
  , streetAddress :: String
  , zipCode :: String
  , city :: String
  , country :: String
  , phone :: String
  , legalConsents :: Array LegalConsent
  }

type TemporaryAddressChange =
  { street        :: String
  , zipcode       :: String
  , cityName      :: Nullable String
  , countryCode   :: String
  , temporaryName :: Nullable String
  }

type DeliveryReclamation =
  { subscriptionNumber :: Int
  , customerNumber     :: Int
  , number             :: Int
  , date               :: JSDate
  , publicationDate    :: JSDate
  , claim              :: DeliveryReclamationClaim
  , status             :: DeliveryReclamationStatus
  }

data DeliveryReclamationClaim
  = Extension
  | NewDelivery

instance readDeliveryReclamationClaim :: Read DeliveryReclamationClaim where
  read c =
    case c of
      "Extension"   -> pure Extension
      "NewDelivery" -> pure NewDelivery
      _             -> Nothing
derive instance genericDeliveryReclamationClaim :: Generic DeliveryReclamationClaim _
instance readForeignDeliveryReclamationClaim :: ReadForeign DeliveryReclamationClaim where
  readImpl = genericDecodeEnum { constructorTagTransform: \x -> x }
instance writeForeignDeliveryReclamationClaim :: WriteForeign DeliveryReclamationClaim where
  writeImpl = genericEncodeEnum { constructorTagTransform: \x -> x }
instance showDeliveryReclamationClaim :: Show DeliveryReclamationClaim where
  show = genericShow

data DeliveryReclamationStatus
  = Created
  | Processing
  | Processed

instance readDeliveryReclamationStatus :: Read DeliveryReclamationStatus where
  read c =
    case c of
      "Created"    -> pure Created
      "Processing" -> pure Processing
      "Processed"  -> pure Processed
      _            -> Nothing
derive instance genericDeliveryReclamationStatus :: Generic DeliveryReclamationStatus _
instance readForeignDeliveryReclamationStatus :: ReadForeign DeliveryReclamationStatus where
  readImpl = genericDecodeEnum { constructorTagTransform: \x -> x }
instance writeForeignDeliveryReclamationStatus :: WriteForeign DeliveryReclamationStatus where
  writeImpl = genericEncodeEnum { constructorTagTransform: \x -> x }
instance showDeliveryReclamationStatus :: Show DeliveryReclamationStatus where
  show = genericShow

data PaymentState
  = PaymentOpen
  | PartiallyPaid
  | Paid
  | Reminded
  | Foreclosure
  | Reimbursed
  | CreditLoss

instance readPaymentState :: Read PaymentState where
  read s =
    case s of
      "PaymentOpen"   -> pure PaymentOpen
      "PartiallyPaid" -> pure PartiallyPaid
      "Paid"          -> pure Paid
      "Reminded"      -> pure Reminded
      "Foreclosure"   -> pure Foreclosure
      "Reimbursed"    -> pure Reimbursed
      "CreditLoss"    -> pure CreditLoss
      _               -> Nothing

data PaymentType
  = NormalState
  | DirectDebit
  | Reminder1
  | Reminder2
  | Nonpayment
  | Reimbursement

instance readPaymentType :: Read PaymentType where
  read t =
    case t of
      "NormalState"   -> pure NormalState
      "DirectDebit"   -> pure DirectDebit
      "Reminder1"     -> pure Reminder1
      "Reminder2"     -> pure Reminder2
      "Nonpayment"    -> pure Nonpayment
      "Reimbursement" -> pure Reimbursement
      _               -> Nothing

type SubscriptionPayments =
  { name :: String
  , startDate :: Date
  , lastDate :: Date
  , payments :: Array Payment
  }

type Payment =
  { date :: Date
  , dueDate :: Date
  , expenses :: Number
  , interest :: Number
  , vat :: Number
  , amount :: Number
  , openAmount :: Number
  , type :: PaymentType
  , state :: PaymentState
  , discount :: Number
  , invno :: Int
  , reference :: String
  }

type ApiSubscriptionPayments =
  { name :: String
  , startDate :: JSDate
  , lastDate :: JSDate
  , payments :: Array ApiPayment
  }

type ApiPayment =
  { date :: JSDate
  , dueDate :: JSDate
  , expenses :: Number
  , interest :: Number
  , vat :: Number
  , amount :: Number
  , openAmount :: Number
  , type :: String
  , state :: String
  , discount :: Number
  , invno :: Int
  , reference :: String
  }

getPayments :: UUID -> UserAuth -> Aff (Array SubscriptionPayments)
getPayments uuid auth =
  catMaybes <<< map nativeSubscriptionPayments
    <$> callApi usersApi "usersUuidPaymentsGet" [ unsafeToForeign uuid ] ( authHeaders uuid auth )
  where
    nativeSubscriptionPayments :: ApiSubscriptionPayments -> Maybe SubscriptionPayments
    nativeSubscriptionPayments x = do
      ps <- sequence $ map nativePayment x.payments
      sd <- toDate x.startDate
      ld <- toDate x.lastDate
      pure $ { name: x.name
             , startDate: sd
             , lastDate : ld
             , payments: ps
             }
    nativePayment :: ApiPayment -> Maybe Payment
    nativePayment x = do
      d <- toDate x.date
      dd <- toDate x.dueDate
      t <- read x.type
      s <- read x.state
      pure $ { date: d
             , dueDate: dd
             , expenses: x.expenses
             , interest: x.interest
             , vat: x.vat
             , amount: x.amount
             , openAmount: x.openAmount
             , type: t
             , state: s
             , discount: x.discount
             , invno: x.invno
             , reference: x.reference
             }

type Forbidden = ServerError
  ( forbidden :: { description :: String } )

type ApiJanrainUser =
  { uuid        :: UUID
  , email       :: Nullable String
  , firstName   :: Nullable String
  , lastName    :: Nullable String
  , consent     :: Array GdprConsent
  , legal       :: Array LegalConsent
  , cusno       :: Nullable String
  , otherCusnos :: Nullable (Array String)
  }

type ApiFaroUser =
  { cusno         :: Cusno
  , name          :: String
  , address       :: Nullable Address
  , email         :: Nullable String
  , subscriptions :: Nullable (Array Subscription)
  }

type ApiSearchResult =
  { janrain     :: Nullable ApiJanrainUser
  , faro        :: Array ApiFaroUser
  }

-- Pass dummy uuid to force authUser field generation.
searchUsers :: SearchQuery -> UserAuth -> Aff (Array SearchResult)
searchUsers query auth = do
  catMaybes <<< map nativeSearchResults <$>
    callApi adminApi "adminSearchPost" [ unsafeToForeign query ]
    ( authHeaders UUID.emptyUUID auth )
  where
    nativeSearchResults :: ApiSearchResult -> Maybe SearchResult
    nativeSearchResults x = do
      janrain <- nativeJanrain x.janrain
      faro <- sequence $ map nativeFaro x.faro
      pure { janrain, faro }
    nativeJanrain :: Nullable ApiJanrainUser -> Maybe (Maybe JanrainUser)
    nativeJanrain x = do
      pure $ case toMaybe x of
        Nothing -> Nothing
        Just j -> Just
          { uuid:        j.uuid
          , email:       toMaybe j.email
          , firstName:   toMaybe j.firstName
          , lastName:    toMaybe j.lastName
          , consent:     j.consent
          , legal:       j.legal
          , cusno:       toMaybe j.cusno
          , otherCusnos: toMaybe j.otherCusnos
          }
    nativeFaro :: ApiFaroUser -> Maybe FaroUser
    nativeFaro x = do
      pure $ { cusno:   x.cusno
             , name:    x.name
             , address: toMaybe x.address
             , email:   toMaybe x.email
             , subs:    toMaybe x.subscriptions
             }
