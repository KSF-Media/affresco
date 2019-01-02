module Persona where

import Prelude

import Control.Monad.Except (runExcept)
import Control.MonadPlus (guard)
import Data.Either (hush)
import Data.Function.Uncurried (Fn4, runFn4)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.JSDate (JSDate)
import Data.Maybe (Maybe, isNothing)
import Data.Nullable (Nullable)
import Data.String (toLower)
import Data.Traversable (traverse)
import Effect.Aff (Aff)
import Effect.Aff.Compat (EffectFnAff, fromEffectFnAff)
import Effect.Exception (Error)
import Foreign (Foreign, readNullOrUndefined, unsafeToForeign)
import Foreign.Generic.EnumEncoding (genericDecodeEnum, genericEncodeEnum)
import Foreign.Index (readProp) as Foreign
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

updateGdprConsent :: UUID -> Token -> Array GdprConsent -> Aff Unit
updateGdprConsent uuid token consentValues = callApi usersApi "usersUuidGdprPut" [ unsafeToForeign uuid, unsafeToForeign consentValues ] { authorization }
  where
    authorization = oauthToken token

logout :: UUID -> Token -> Aff Unit
logout uuid token =
  callApi loginApi "loginUuidDelete" [ unsafeToForeign uuid ] { authorization }
  where
    authorization = oauthToken token

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
  guard $ isNothing $ errorField "status" err :: Maybe Foreign
  pure { method, url }

-- | Check if an error has some field and it's not null or undefined.
errorField :: forall a. ReadForeign a => String -> Error -> Maybe a
errorField field =
  join <<< hush
    <<< (traverse JSON.read =<< _)
    <<< runExcept
    <<< do readNullOrUndefined <=< Foreign.readProp field
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

type Subscription =
  { subsno     :: Int
  , extno      :: Int
  , cusno      :: Int
  , paycusno   :: Int
  , kind       :: String
  , state      :: SubscriptionState
  , pricegroup :: String
  , package    :: ModelPackage
  , dates      :: SubscriptionDates
  , campaign   :: Campaign
  }

newtype SubscriptionState = SubscriptionState String

derive instance genericSubscriptionState :: Generic SubscriptionState _
instance readForeignSubscriptionState :: ReadForeign SubscriptionState where
  readImpl f = map SubscriptionState (readImpl f)

type ModelPackage =
  { id          :: String
  , name        :: String
  , paper       :: { code :: String, name :: String }
  , products    :: Array Product
  , offers      :: Array PackageOffer
  , campaigns   :: Array Campaign
  , nextDelivery :: Nullable JSDate
  , description  :: Nullable PackageDescription
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
  { key :: String
  , val :: Boolean
  }
