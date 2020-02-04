module Persona where

import Prelude

import Control.Alt ((<|>))
import Control.Monad.Except (runExcept)
import Control.MonadPlus (guard)
import Data.DateTime (DateTime)
import Data.Either (Either(..), hush)
import Data.Formatter.DateTime (FormatterCommand(..), format)
import Data.Function.Uncurried (Fn4, runFn4)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.JSDate (JSDate)
import Data.List (fromFoldable)
import Data.Maybe (Maybe(..), isNothing)
import Data.Nullable (Nullable, toNullable)
import Data.String (toLower)
import Data.String.Read
import Data.Traversable (traverse)
import Effect.Aff (Aff)
import Effect.Aff.Compat (EffectFnAff, fromEffectFnAff)
import Effect.Exception (Error)
import Foreign (Foreign, readNullOrUndefined, unsafeToForeign)
import Foreign.Generic.EnumEncoding (defaultGenericEnumOptions, genericDecodeEnum, genericEncodeEnum)
import Foreign.Index (readProp) as Foreign
import Foreign.Object (Object)
import Simple.JSON (class ReadForeign, class WriteForeign, readImpl)
import Simple.JSON as JSON

foreign import data Api :: Type
foreign import loginApi :: Api
foreign import usersApi :: Api

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

login :: LoginData -> Aff LoginResponse
login loginData = callApi loginApi "loginPost" [ unsafeToForeign loginData ] {}

loginSome :: LoginDataSome -> Aff LoginResponse
loginSome loginData = callApi loginApi "loginSomePost" [ unsafeToForeign loginData ] {}

loginSso :: LoginDataSso -> Aff LoginResponse
loginSso loginData = callApi loginApi "loginSsoPost" [ unsafeToForeign loginData ] {}

getUser :: UUID -> Token -> Aff User
getUser uuid token = callApi usersApi "usersUuidGet" [ unsafeToForeign uuid ] { authorization }
  where
    authorization = oauthToken token

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

logout :: UUID -> Token -> Aff Unit
logout uuid token =
  callApi loginApi "loginUuidDelete" [ unsafeToForeign uuid ] { authorization }
  where
    authorization = oauthToken token

register :: NewUser -> Aff LoginResponse
register newUser =
  callApi usersApi "usersPost" [ unsafeToForeign newUser ] {}

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
  callApi usersApi "usersUuidSubscriptionsSubsnoReclamationPost"
    [ unsafeToForeign uuid
    , unsafeToForeign subsno
    , unsafeToForeign { publicationDate: dateISO, claim: claim }
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

newtype Token = Token String
derive newtype instance showToken :: Show Token
derive newtype instance readforeignToken :: ReadForeign Token
derive newtype instance writeforeignToken :: WriteForeign Token

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

-- | Matches internal server error produced by superagent.
--   Checks that it has `status` field that's 5XX.
internalServerError :: Error -> Maybe { status :: Int }
internalServerError err = do
  status <- err # errorField "status"
  guard $ status >= 500 && status <= 599
  pure { status }

-- | Matches network error produced by superagent.
--   Checks that it has `method` and `url` fields, but no `status`.
networkError :: Error -> Maybe { method :: String, url :: String }
networkError err = do
  method <- errorField "method" err
  url <- errorField "url" err
  guard $ isNothing (errorField "status" err :: Maybe Foreign)
  pure { method, url }

-- | Check if an error has some field and it's not null or undefined.
errorField :: forall a. ReadForeign a => String -> Error -> Maybe a
errorField field =
  join <<< hush
    <<< (traverse JSON.read =<< _)
    <<< runExcept
    <<< do readNullOrUndefined <=< Foreign.readProp field
    <<< unsafeToForeign

isSubscriptionCanceled :: Subscription -> Boolean
isSubscriptionCanceled s = isSubscriptionStateCanceled s.state

isSubscriptionStateCanceled :: SubscriptionState -> Boolean
isSubscriptionStateCanceled (SubscriptionState "Canceled") = true
isSubscriptionStateCanceled _ = false

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

newtype UUID = UUID String
derive newtype instance showUUID :: Show UUID
derive newtype instance readforeignUUID :: ReadForeign UUID
derive newtype instance writeforeignUUID :: WriteForeign UUID

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

type DeliveryAddress =
  { streetAddress :: Nullable String
  , zipcode       :: String
  , city          :: Nullable String
  , temporaryName :: Nullable String
  }

type PendingAddressChange =
  { address   :: DeliveryAddress
  , startDate :: JSDate
  , endDate   :: JSDate
  }

type Subscription =
  { subsno                :: Int
  , extno                 :: Int
  , cusno                 :: Int
  , paycusno              :: Int
  , kind                  :: String
  , state                 :: SubscriptionState
  , pricegroup            :: String
  , package               :: ModelPackage
  , dates                 :: SubscriptionDates
  , campaign              :: Campaign
  , paused                :: Nullable (Array PausedSubscription)
  , deliveryAddress       :: Nullable DeliveryAddress
  , pendingAddressChanges :: Nullable (Array PendingAddressChange)
  }

type PausedSubscription =
  { startDate :: JSDate
  , endDate   :: Nullable JSDate
  }

newtype SubscriptionState = SubscriptionState String

derive instance genericSubscriptionState :: Generic SubscriptionState _
instance readForeignSubscriptionState :: ReadForeign SubscriptionState where
  readImpl f = map SubscriptionState (readImpl f)
derive instance eqSubscriptionState :: Eq SubscriptionState
instance ordSubscriptionState :: Ord SubscriptionState where
  compare =
    comparing
      \s@(SubscriptionState st) ->
        if isSubscriptionStateCanceled s
        then Right st
        else Left st

type ModelPackage =
  { id          :: String
  , name        :: String
  , paper       :: { code :: String, name :: String }
  , products    :: Array Product
  , offers      :: Array PackageOffer
  , campaigns   :: Array Campaign
  , nextDelivery :: Nullable JSDate
  , description  :: Nullable PackageDescription
  , digitalOnly :: Boolean
  }

type PackageDescription =
  { brand     :: String
  , brandLong :: String
  , descShort :: String
  , descLong  :: String
  , url       :: String
  , days      :: String
  , weekdays  :: String
  , frequency :: { amount :: Int, unit :: String }
  , includes  :: Array String
  }

type PackageOffer =
  { months       :: Int
  , totalPrice   :: Int
  , monthlyPrice :: Int
  }

type Product =
  { id :: String
  , name :: String
  , active :: ActiveDays
  , nextDelivery :: Nullable JSDate
  }

type ActiveDays =
  { mon :: Boolean
  , tue :: Boolean
  , wed :: Boolean
  , thu :: Boolean
  , fri :: Boolean
  , sat :: Boolean
  , sun :: Boolean
  }

type Campaign =
  { no   :: Int
  , id   :: String
  , name :: String
  }

type SubscriptionDates =
  { lenMonths           :: Nullable Int
  , lenDays             :: Nullable Int
  , start               :: JSDate
  , end                 :: Nullable JSDate
  , unpaidBreak         :: Nullable JSDate
  , invoicingStart      :: Nullable JSDate
  , paidUntil           :: Nullable JSDate
  , suspend             :: Nullable JSDate
  }

type GdprConsent =
  { brand      :: String
  , consentKey :: String
  , value      :: Boolean
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
