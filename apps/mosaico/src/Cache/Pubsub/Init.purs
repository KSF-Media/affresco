module Mosaico.Cache.Pubsub.Init where

import Prelude

import Data.Argonaut.Core (Json, toObject, toString)
import Data.Argonaut.Parser (jsonParser)
import Data.Array.NonEmpty as NonEmpty
import Data.Either (Either(..), hush)
import Data.Maybe (Maybe(..), maybe)
import Data.Nullable (Nullable)
import Data.Nullable as Nullable
import Data.String.Regex as Regex
import Data.String.Regex.Flags (noFlags)
import Effect (Effect)
import Effect.Exception (catchException)
import Foreign.Object (lookup)
import KSF.Random (randomString)
import Node.Encoding (Encoding(..))
import Node.FS.Sync (readTextFile, writeTextFile)
import Node.Process (lookupEnv, setEnv)

-- This function gets called from javascript Mosaico.Cache.Pubsub
applyCredentials :: Effect (Nullable String)
applyCredentials = do
  credentials <- lookupEnv "GOOGLE_APPLICATION_CREDENTIALS"
  -- Hack alert: we may get the credentials in the variable itself
  case credentials of
    Nothing -> pure Nullable.null
    Just c
      | Right json <- jsonParser c -> do
        fileName <- (\x -> "/tmp/gac." <> x) <$> randomString 10
        writeTextFile UTF8 fileName c
        setEnv "GOOGLE_APPLICATION_CREDENTIALS" fileName
        pure $ Nullable.toNullable $ getProjectIdFromCredentials json
      | otherwise -> do
        catchException (const $ pure Nullable.null) do
          eitherJson <- jsonParser <$> readTextFile UTF8 c
          case eitherJson of
            Right json -> pure $ Nullable.toNullable $ getProjectIdFromCredentials json
            _ -> pure Nullable.null

getProjectIdFromCredentials :: Json -> Maybe String
getProjectIdFromCredentials =
  NonEmpty.head <=< match <=< toString <=< lookup "client_email" <=< toObject
  where
    regex = Regex.regex "@([^.]+)" noFlags
    match =  maybe (const Nothing) Regex.match $ hush regex
