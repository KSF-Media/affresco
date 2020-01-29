module KSF.Api where

import Prelude

import Simple.JSON (class ReadForeign, class WriteForeign)

newtype UUID = UUID String
derive newtype instance showUUID :: Show UUID
derive newtype instance readforeignUUID :: ReadForeign UUID
derive newtype instance writeforeignUUID :: WriteForeign UUID

newtype Token = Token String
derive newtype instance showToken :: Show Token
derive newtype instance readforeignToken :: ReadForeign Token
derive newtype instance writeforeignToken :: WriteForeign Token

oauthToken :: Token -> String
oauthToken (Token token) = "OAuth " <> token

type UserAuth =
  { userId    :: UUID
  , authToken :: Token
  }
