module Ospedale.TokenResponse where

import Prelude

import Affjax.RequestBody (formURLEncoded)
import Affjax.Web (defaultRequest, request)
import Affjax.ResponseFormat as ResponseFormat
import Control.Monad.Rec.Class (Step(..), tailRecM, untilJust)
import Data.Argonaut as JSON
import Data.Either (Either(..), hush)
import Data.FormURLEncoded as FormURLEncoded
import Data.HTTP.Method (Method(POST))
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Newtype (modify)
import Data.Time.Duration (Seconds(..), fromDuration)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.AVar as AVar
import Effect.Aff (Aff, delay, launchAff_, forkAff, killFiber)
import Effect.Aff.AVar as Aff.AVar
import Effect.Class (liftEffect)
import Effect.Exception as Exception
import Foreign.Object (lookup)
import Debug

type TokenResponse =
  { accessToken :: String
  , expiresIn :: Maybe Seconds
  , refreshToken :: String
  }

fromJSON :: JSON.Json -> Maybe TokenResponse
fromJSON = buildTokenResponse <=< JSON.toObject
  where
    buildTokenResponse obj =
      { expiresIn: map Seconds <<< JSON.toNumber =<< lookup "expires_in" obj
      , accessToken: _
      , refreshToken: _
      }
      <$> (JSON.toString =<< lookup "access_token" obj)
      <*> (JSON.toString =<< lookup "refresh_token" obj)

-- Nothing in case refresh failed.  The caller should force logging in
-- again if it happens.
type AccessToken =
  { accessToken :: Aff (Maybe String)
  }

maintainAccess :: String -> TokenResponse -> Effect AccessToken
maintainAccess _ {accessToken, expiresIn: Nothing} =
  pure { accessToken: pure $ Just accessToken }
maintainAccess tokenURL initialToken = do
  var <- AVar.new $ Just initialToken.accessToken
  launchAff_ $ flip tailRecM initialToken $ \{refreshToken, expiresIn} -> do
    delay $ fromDuration $ modify (_ - 5.0) $ fromMaybe (Seconds 60.0) expiresIn
    let req = defaultRequest
              { url = tokenURL
              , method = Left POST
              , responseFormat = ResponseFormat.json
              , content = Just $ formURLEncoded $ FormURLEncoded.fromArray
                          [ Tuple "grant_type" $ Just "refresh_token"
                          , Tuple "refresh_token" $ Just refreshToken
                          , Tuple "client_id" $ Just "ospedale-public"
                          ]
              }
    -- The old token is fine for a while longer
    unsetFiber <- forkAff do
      delay $ fromDuration $ Seconds 4.0
      _ <- Aff.AVar.take var
      pure unit
    response <- request req
    killFiber (Exception.error "stop emptying thread") unsetFiber
    _ <- Aff.AVar.tryTake var
    let newVal = fromJSON <<< _.body =<< hush response
    Aff.AVar.put (_.accessToken <$> newVal) var
    traceM {newVal, response, refreshToken, expiresIn}
    pure $ maybe (Done unit) Loop newVal
  pure { accessToken: Aff.AVar.take var }

listenAccessToken :: (Maybe String -> Effect Unit) -> AccessToken -> Effect Unit
listenAccessToken onChange { accessToken } = do
  launchAff_ $ untilJust do
    token <- accessToken
    traceM {listenAccessToken:unit, token}
    liftEffect $ onChange token
    pure $ maybe (Just unit) (const Nothing) token
