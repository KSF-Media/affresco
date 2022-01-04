module OpenApiClient where

import Prelude

import Data.Function.Uncurried (Fn4, runFn4, Fn1, runFn1)
import Data.Nullable (Nullable, toMaybe)
import Data.Argonaut.Decode.Error (JsonDecodeError (..), printJsonDecodeError)
import Data.Argonaut.Decode (class DecodeJson, decodeJson)
import Data.Maybe (Maybe (..))
import Control.Monad.Error.Class (throwError)
import Data.Newtype (class Newtype)
import Data.Either (Either (..))
import Effect.Aff (Aff)
import Effect.Aff.Compat (EffectFnAff, fromEffectFnAff)
import Effect.Exception (error)
import Foreign (Foreign)
import Data.Argonaut.Core (Json)
import Data.DateTime (DateTime)
import KSF.Helpers (parseDateTime)

foreign import data Api :: Type
foreign import readOpenApiDateImpl :: Fn1 Json (Nullable String)

newtype OpenApiDate = OpenApiDate DateTime

derive instance newtypeOpenApiDate :: Newtype OpenApiDate _

instance decodeJsonOpenApiDate :: DecodeJson OpenApiDate where
  decodeJson json = do
    let dateTimeString = runFn1 readOpenApiDateImpl json
    case parseDateTime =<< toMaybe dateTimeString of
      Just dt -> Right $ OpenApiDate dt
      Nothing -> Left $ UnexpectedValue json

type MethodName = String

foreign import callApi_
  :: forall req res opts
   . Fn4
       Api
       MethodName
       req
       { | opts }
       (EffectFnAff res)

callApi :: forall res opts. Api -> MethodName -> Array Foreign -> { | opts } -> Aff res
callApi api methodName req opts =
  fromEffectFnAff (runFn4 callApi_ api methodName req opts)

callApi' :: forall res opts. Api -> MethodName -> Array Foreign -> { | opts } -> Aff Json
callApi' api methodName req opts =
  fromEffectFnAff (runFn4 callApi_ api methodName req opts)

decodeApiRes :: forall a. (DecodeJson a) => String -> Json -> Aff a
decodeApiRes typeName json =
  case decodeJson json of
    Right x -> pure x
    Left e  -> throwError $ error $ "Could not parse json! " <> "Type: '" <> typeName <> "', Error: " <> printJsonDecodeError e
