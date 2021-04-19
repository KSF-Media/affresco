module KSF.Auth where

import Prelude

import Control.Monad.Maybe.Trans (MaybeT(..), runMaybeT)
import Data.Maybe (Maybe(..))
import Data.Nullable as Nullable
import Data.Traversable (for_, traverse_)
import Data.UUID as UUID
import Effect (Effect)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Exception (throw)
import KSF.Api (UserAuth)
import KSF.Api as Api
import KSF.JanrainSSO as JanrainSSO
import KSF.LocalStorage as LocalStorage
import Persona as Persona

loadToken :: forall m. MonadEffect m => m (Maybe UserAuth)
loadToken = liftEffect $ runMaybeT do
  authToken <- map Api.Token $ MaybeT $ LocalStorage.getItem "token"
  userId <- MaybeT $ (UUID.parseUUID =<< _) <$> LocalStorage.getItem "uuid"
  pure { userId, authToken }

saveToken :: forall m. MonadEffect m => Persona.LoginResponse -> m UserAuth
saveToken { token, ssoCode, uuid, isAdmin } = liftEffect do
  for_ (Nullable.toMaybe ssoCode) $ \code -> do
    config <- JanrainSSO.loadConfig
    for_ (Nullable.toMaybe config) \conf -> JanrainSSO.setSession conf code
  LocalStorage.setItem "token" case token of Api.Token a -> a
  LocalStorage.setItem "uuid" $ UUID.toString uuid
  -- This isn't returned by loadToken.
  if isAdmin
    then LocalStorage.setItem "isAdmin" "1"
    else LocalStorage.removeItem "isAdmin"
  pure { userId: uuid, authToken: token }

deleteToken :: Effect Unit
deleteToken = traverse_ LocalStorage.removeItem [ "token", "uuid", "isAdmin" ]

requireToken :: forall m. MonadEffect m => m UserAuth
requireToken =
  loadToken >>= case _ of
    Nothing -> liftEffect $ throw "Did not find uuid/token in local storage."
    Just loginResponse -> pure loginResponse
