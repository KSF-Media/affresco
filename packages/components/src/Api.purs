module KSF.Api where

import Prelude

import Data.Maybe (Maybe)
import Data.String as String
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

parseToken :: String -> Maybe Token
parseToken = map Token <<< String.stripPrefix (String.Pattern "OAuth ")

type UserAuth =
  { userId    :: UUID
  , authToken :: Token
  }
