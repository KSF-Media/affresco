module Bottega.GiftApi where

import Prelude

import Affjax (defaultRequest, request) as AX
import Affjax.RequestHeader (RequestHeader(..)) as AX
import Affjax.ResponseFormat (json) as AX
import Affjax.StatusCode (StatusCode(..))
import Affjax.Web (driver)
import Bottega.Models (Gift)
import Data.Argonaut as Json
import Data.Either (Either(..))
import Data.HTTP.Method (Method(..))
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Data.UUID as UUID
import Effect.Aff (Aff)
import Foreign.Object as Object
import KSF.Api (UserAuth, oauthToken)

data RedeemError = Permission | UnknownCode | AlreadyRedeemed | AlreadyRedeemedByYou | NotPaid | UnknownError

redeemError :: StatusCode -> Maybe RedeemError
redeemError status = case unwrap status of
  204 -> Just AlreadyRedeemedByYou
  402 -> Just NotPaid
  403 -> Just Permission
  404 -> Just UnknownCode
  409 -> Just AlreadyRedeemed
  _ -> Nothing

getGift :: String -> Aff (Either RedeemError Gift)
getGift giftCode = do
  let request = AX.defaultRequest
        { url = "http://localhost:8081/v1/order/gift/" <> giftCode
        , method = Left GET
        , responseFormat = AX.json
        }
  giftResponse <- AX.request driver request
  pure $ case giftResponse of
    Left _ -> Left UnknownError
    Right response
      | Just { owner, package } <- fromJson response.body -> Right { owner, package }
      | Just err <- redeemError response.status -> Left err
      | otherwise -> Left UnknownError
  where
    fromJson = (\o -> {owner:_, package:_}
                      <$> (UUID.parseUUID =<< Json.toString =<< Object.lookup "owner" o)
                      <*> (Json.toString =<< Object.lookup "package" o)
               ) <=< Json.toObject
{-
  pure $ case UUID.parseUUID "00000000-0000-0000-0000-000000000000" of
    Just uuid ->  Just { package: "HBL_P+D", owner: uuid }
    _ -> Nothing
-}

redeemGift :: String -> UserAuth -> Aff (Maybe RedeemError)
redeemGift giftCode { userId, authToken } = do
  let request = AX.defaultRequest
        { url = "http://localhost:8081/v1/order/gift/" <> giftCode
        , method = Left PUT
        , headers =
            [ AX.RequestHeader "AuthUser" $ UUID.toString userId
            , AX.RequestHeader "Authorization" $ oauthToken authToken
            ]
        }
  giftResponse <- AX.request driver request
  pure $ case giftResponse of
    Left _ -> Just UnknownError
    Right response
      | Just err <- redeemError response.status -> Just err
      | (StatusCode 204) <- response.status -> Nothing
      | otherwise -> Just UnknownError
