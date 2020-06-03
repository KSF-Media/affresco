module Persona where

import Prelude

import Control.Alt ((<|>))
import Control.Monad.Except (runExcept)
import Data.DateTime (DateTime)
import Data.Either (hush)
import Data.Formatter.DateTime (FormatterCommand(..), format)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.JSDate (JSDate)
import Data.List (fromFoldable)
import Data.Maybe (Maybe(..))
import Data.Nullable (Nullable, toNullable)
import Data.String (toLower)
import Data.String.Read (class Read)
import Effect.Aff (Aff)
import Effect.Exception (Error)
import Foreign (unsafeToForeign)
import Foreign.Generic.EnumEncoding (defaultGenericEnumOptions, genericDecodeEnum, genericEncodeEnum)
import Foreign.Index (readProp) as Foreign
import Foreign.Object (Object)
import KSF.Api (InvalidateCache, Password, Token(..), UUID, invalidateCacheHeader)
import KSF.Api.Subscription (Subscription, PendingAddressChange)
import OpenApiClient (Api, callApi)
import Simple.JSON (class ReadForeign, class WriteForeign)
import Simple.JSON as JSON

foreign import loginApi :: Api
foreign import usersApi :: Api

login :: LoginData -> Aff LoginResponse
login loginData = callApi loginApi "loginPost" [ unsafeToForeign loginData ] {}

loginSome :: LoginDataSome -> Aff LoginResponse
loginSome loginData = callApi loginApi "loginSomePost" [ unsafeToForeign loginData ] {}

loginSso :: LoginDataSso -> Aff LoginResponse
loginSso loginData = callApi loginApi "loginSsoPost" [ unsafeToForeign loginData ] {}

getUser :: Maybe InvalidateCache -> UUID -> Token -> Aff User
getUser invalidateCache uuid token =
  callApi usersApi "usersUuidGet" [ unsafeToForeign uuid ] headers
  where
    headers =
      { authorization
      , cacheControl: toNullable maybeCacheControl
      }
    authorization = oauthToken token
    maybeCacheControl = invalidateCacheHeader <$> invalidateCache

updateUser :: UUID -> UserUpdate -> Token -> Aff User
updateUser uuid update token =
  let
    authorization = oauthToken token
    body = case update of
      UpdateName names      -> unsafeToForeign names
      UpdateAddress address -> unsafeToForeign { address }
  in
   callApi usersApi "usersUuidPatch" [ unsafeToForeign uuid, body ] { authorization }

updateGdprConsent :: UUID -> Token -> Array GdprConsent -> Aff Unit
updateGdprConsent uuid token consentValues = callApi usersApi "usersUuidGdprPut" [ unsafeToForeign uuid, unsafeToForeign consentValues ] { authorization }
  where
    authorization = oauthToken token

updatePassword :: UUID -> Password -> Password -> Token -> Aff User
updatePassword uuid password confirmPassword token = callApi usersApi "usersUuidPasswordPut" [ unsafeToForeign uuid, unsafeToForeign { password, confirmPassword } ] { authorization }
  where
    authorization = oauthToken token

logout :: UUID -> Token -> Aff Unit
logout uuid token =
  callApi loginApi "loginUuidDelete" [ unsafeToForeign uuid ] { authorization }
  where
    authorization = oauthToken token

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

pauseSubscription :: UUID -> Int -> DateTime -> DateTime -> Token -> Aff Subscription
pauseSubscription uuid subsno startDate endDate token = do
  let startDateISO = formatDate startDate
      endDateISO   = formatDate endDate
  callApi usersApi "usersUuidSubscriptionsSubsnoPausePost"
    [ unsafeToForeign uuid
    , unsafeToForeign subsno
    , unsafeToForeign { startDate: startDateISO, endDate: endDateISO }
    ]
    { authorization }
  where
    authorization = oauthToken token

unpauseSubscription :: UUID -> Int -> Token -> Aff Subscription
unpauseSubscription uuid subsno token = do
  callApi usersApi "usersUuidSubscriptionsSubsnoUnpausePost"
    ([ unsafeToForeign uuid
     , unsafeToForeign subsno
     ])
    { authorization }
  where
    authorization = oauthToken token

temporaryAddressChange
  :: UUID
  -> Int
  -> DateTime
  -> DateTime
  -> String
  -> String
  -> String
  -> Maybe String
  -> Token
  -> Aff Subscription
temporaryAddressChange uuid subsno startDate endDate streetAddress zipCode countryCode temporaryName token = do
  let startDateISO = formatDate startDate
      endDateISO   = formatDate endDate
  callApi usersApi "usersUuidSubscriptionsSubsnoAddressChangePost"
    [ unsafeToForeign uuid
    , unsafeToForeign subsno
    , unsafeToForeign { startDate: startDateISO, endDate: endDateISO, streetAddress, zipCode, countryCode, temporaryName: toNullable temporaryName }
    ]
    { authorization }
  where
    authorization = oauthToken token

createDeliveryReclamation
  :: UUID
  -> Int
  -> DateTime
  -> DeliveryReclamationClaim
  -> Token
  -> Aff DeliveryReclamation
createDeliveryReclamation uuid subsno date claim token = do
  let dateISO = formatDate date
  let claim'  = show claim
  callApi usersApi "usersUuidSubscriptionsSubsnoReclamationPost"
    [ unsafeToForeign uuid
    , unsafeToForeign subsno
    , unsafeToForeign { publicationDate: dateISO, claim: claim' }
    ]
    { authorization }
  where
    authorization = oauthToken token

formatDate :: DateTime -> String
formatDate = format formatter
  where
    dash = Placeholder "-"
    formatter = fromFoldable
      [ YearFull
      , dash
      , MonthTwoDigits
      , dash
      , DayOfMonthTwoDigits
      ]

newtype Email = Email String
derive newtype instance showEmail :: Show Email
derive newtype instance readforeignEmail :: ReadForeign Email
derive newtype instance writeforeignEmail :: WriteForeign Email
derive newtype instance eqEmail :: Eq Email

oauthToken :: Token -> String
oauthToken (Token token) = "OAuth " <> token

type LoginResponse =
  { token :: Token
  , ssoCode :: Nullable String
  , uuid :: UUID
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
  | UpdateAddress { countryCode :: String, zipCode :: String, streetAddress :: String }

type PersonaError extraFields =
  { http_status :: String
  , http_code :: Int
  | extraFields
  }

type EmailAddressInUse = PersonaError
  ( email_address_in_use ::
    { existing_provider :: Provider
    , merge_token :: MergeToken
    , description :: String
    }
  )

type TokenInvalid = PersonaError
  ( login_token_expired ::
    { description :: String }
  )

type InvalidCredentials = PersonaError
  ( invalid_credentials :: { description :: String } )

type InvalidFormFields = PersonaError
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

type InvalidPauseDates = PersonaError
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

type InvalidDates = PersonaError
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

type EmailAddressInUseRegistration = PersonaError
  ( email_address_in_use_registration :: { description :: String } )

errorData
  :: forall fields
   . ReadForeign (PersonaError fields)
  => Error
  -> Maybe (PersonaError fields)
errorData =
  hush
    <<< (JSON.read =<< _)
    <<< runExcept
          <<< Foreign.readProp "data"
          <<< unsafeToForeign



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

type User =
  { uuid :: UUID
  , email :: String
  , firstName :: Nullable String
  , lastName :: Nullable String
  , address :: Nullable Address
  , cusno :: String
  , subs :: Array Subscription
  , consent :: Array GdprConsent
  , pendingAddressChanges :: Nullable (Array PendingAddressChange)
  , hasCompletedRegistration :: Boolean
  }

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

type Address =
  { countryCode   :: String
  , zipCode       :: Nullable String
  , city          :: Nullable String
  , streetAddress :: String
  , streetName    :: Nullable String
  , houseNo       :: Nullable String
  , staircase     :: Nullable String
  , apartment     :: Nullable String
  }

type GdprConsent =
  { brand      :: String
  , consentKey :: String
  , value      :: Boolean
  }

type LegalConsent =
  { consentId :: String
  , screenName :: String
  , dateAccepted :: String
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
