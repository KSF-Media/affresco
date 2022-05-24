module Mosaico.Cache.Pubsub where

import Prelude

import Control.Promise (Promise, toAffE)
import Effect (Effect)
import Effect.Aff (Aff)
import Data.Maybe (Maybe)
import Data.Nullable (Nullable, toMaybe)
import Lettera.Models (CategoryLabel(..))
import Lettera (LetteraResponse(..))
import Node.Buffer (Buffer, toString)
import Node.Encoding (Encoding(..))

foreign import data Message :: Type

foreign import subscribeImpl :: (Message -> Effect Unit) -> Effect (Promise Unit)
foreign import enabled :: Boolean
foreign import categoryImpl :: Message -> Nullable String
foreign import maxAge :: Message -> Int
foreign import content :: Message -> Buffer

subscribe :: (Message -> Effect Unit) -> Aff Unit
subscribe = subscribeImpl >>> toAffE

category :: Message -> Maybe CategoryLabel
category = categoryImpl >>> toMaybe >>> map CategoryLabel

toLetteraResponse :: Message -> Effect (LetteraResponse String)
toLetteraResponse msg = do
  body <- pure <$> toString UTF8 (content msg)
  pure $ LetteraResponse
    { maxAge: pure (maxAge msg)
    , body
    }
