module KSF.Api where

import Prelude

import Data.UUID (UUID)
import Simple.JSON (class ReadForeign, class WriteForeign)

newtype Token = Token String
derive newtype instance showToken :: Show Token
derive newtype instance readforeignToken :: ReadForeign Token
derive newtype instance writeforeignToken :: WriteForeign Token

newtype Password = Password String

data InvalidateCache = InvalidateCache

data AuthScope = UserRead | UserWrite | UserPassword

invalidateCacheHeader :: InvalidateCache -> String
invalidateCacheHeader _ = "max-age=0"

oauthToken :: Token -> String
oauthToken (Token token) = "OAuth " <> token

type UserAuth =
  { userId    :: UUID
  , authToken :: Token
  }
