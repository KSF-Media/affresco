module Lettera.Request where

import Prelude

import Affjax (Error, Response, Request, printError) as AX
import Affjax.RequestHeader (RequestHeader(..)) as AX
import Affjax.ResponseHeader (ResponseHeader(..)) as AX
import Affjax.StatusCode (StatusCode(..)) as AX
import Data.Argonaut.Core (Json, jsonEmptyObject)
import Data.Argonaut.Encode (class EncodeJson, extend, (:=), (:=?), (~>), (~>?))
import Data.Argonaut.Encode.Class (encodeJson)
import Data.Argonaut.Encode.Encoders (encodeInt, encodeString)
import Data.Either (Either(..), hush)
import Data.Foldable (class Foldable, foldMap, foldl, foldr)
import Data.Maybe (Maybe(..))
import Data.MediaType (MediaType(..))

encodeRequestHeaders :: Array AX.RequestHeader -> Maybe Json
encodeRequestHeaders [] = Nothing
encodeRequestHeaders hs =
  let encodeHeader h obj = flip extend obj $ case h of
        AX.Accept (MediaType v)              -> "Accept" := v
        AX.ContentType (MediaType v)         -> "ContentType" := v
        AX.RequestHeader k@"Authorization" _ -> k := "[censored]"
        AX.RequestHeader k v                 -> k := v
  in Just $ foldr encodeHeader jsonEmptyObject hs

encodeRequest :: forall a . AX.Request a -> Json
encodeRequest x =
  let method = case x.method of
        Left m  -> encodeJson (show m)
        Right s -> encodeJson (show s)
  in "method"  := method ~>
     "url"     := x.url  ~>
     "headers" :=? encodeRequestHeaders x.headers ~>?
     -- Currently we're only using GET requests so we don't need content:
     -- "content" := x.content ~>
     jsonEmptyObject

encodeResponseHeaders :: Array AX.ResponseHeader -> Json
encodeResponseHeaders hs =
  let encodeHeader (AX.ResponseHeader k v) obj = k := v ~> obj
  in foldr encodeHeader jsonEmptyObject hs

encodeResponse :: forall a . AX.Response a -> Json
encodeResponse x =
  let statusCode = case x.status of
        AX.StatusCode n -> n
  in "statusCode" := statusCode ~>
--     "statusText" := x.statusText ~>
     "headers" := encodeResponseHeaders x.headers ~>
     jsonEmptyObject

encodeError :: String -> Maybe Json -> Maybe Json -> Maybe Json -> Json
encodeError name req err code =
  "err" := name ~>
  (("req" := _) <$> req) ~>?
  (("msg" := _) <$> err) ~>?
  (("code" := _) <$> code) ~>?
  jsonEmptyObject

instance encodeJsonLetteraError :: EncodeJson LetteraError where
  encodeJson e = case e of
    ResponseError { letteraErrorRequest, letteraErrorError } ->
      encodeError "ResponseError"
                  (Just letteraErrorRequest)
                  (Just (encodeString (AX.printError letteraErrorError)))
                  Nothing
    HttpError { letteraErrorRequest, letteraErrorResponse, letteraErrorCode } ->
      encodeError "HttpError"
                  (Just letteraErrorRequest)
                  (Just letteraErrorResponse)
                  (Just (encodeInt letteraErrorCode))
    ParseError ->
      encodeError "ParseErrror" Nothing Nothing Nothing

data LetteraError
  = ResponseError
    { letteraErrorRequest  :: Json
    , letteraErrorError    :: AX.Error
    }
  | HttpError
    { letteraErrorRequest  :: Json
    , letteraErrorResponse :: Json
    , letteraErrorCode     :: Int
    }
  | ParseError -- TODO: Include some info for better error messages

mkResponseError :: forall a. AX.Request a -> AX.Error -> LetteraError
mkResponseError req err = ResponseError
  { letteraErrorRequest: encodeRequest req
  , letteraErrorError: err
  }

mkHttpError :: forall a b. AX.Request a -> AX.Response b -> Int -> LetteraError
mkHttpError req resp code = HttpError
  { letteraErrorRequest: encodeRequest req
  , letteraErrorResponse: encodeResponse resp
  , letteraErrorCode: code
  }

mkParseError :: LetteraError
mkParseError = ParseError

data LetteraResponse a = LetteraResponse
  { maxAge :: Maybe Int
  , body :: Either LetteraError a
  }

responseBody :: forall a. LetteraResponse a -> Maybe a
responseBody (LetteraResponse a) = hush a.body

instance functorLetteraResponse :: Functor LetteraResponse where
  map f (LetteraResponse r@{ body }) =
    LetteraResponse $ r { body = f <$> body }

instance foldableLetteraResponse :: Foldable LetteraResponse where
  foldl f z = foldl f z <<< responseBody
  foldr f z = foldr f z <<< responseBody
  foldMap f = foldMap f <<< responseBody
