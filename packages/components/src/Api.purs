module KSF.Api where

import Prelude

import Simple.JSON (class ReadForeign, class WriteForeign)

newtype UUID = UUID String
derive newtype instance eqUUID :: Eq UUID
derive newtype instance showUUID :: Show UUID
derive newtype instance readforeignUUID :: ReadForeign UUID
derive newtype instance writeforeignUUID :: WriteForeign UUID

nullUuid :: UUID
nullUuid = UUID "00000000-0000-0000-0000-000000000000"

newtype Token = Token String
derive newtype instance showToken :: Show Token
derive newtype instance readforeignToken :: ReadForeign Token
derive newtype instance writeforeignToken :: WriteForeign Token

newtype Password = Password String

data InvalidateCache = InvalidateCache

invalidateCacheHeader :: InvalidateCache -> String
invalidateCacheHeader _ = "max-age=0"

oauthToken :: Token -> String
oauthToken (Token token) = "OAuth " <> token

type UserAuth =
  { userId    :: UUID
  , authToken :: Token
  }
