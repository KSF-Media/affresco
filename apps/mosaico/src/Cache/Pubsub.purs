module Mosaico.Cache.Pubsub where

import Prelude

import Control.Promise (Promise, toAffE)
import Effect (Effect)
import Effect.Aff (Aff)
import Data.Maybe (Maybe)
import Data.Nullable (Nullable, toMaybe)
import Lettera.Models (CategoryLabel(..))
import Lettera (LetteraResponse(..))

foreign import data Message :: Type

foreign import subscribeImpl :: (Message -> Effect Unit) -> Effect (Promise Unit)
foreign import enabled :: Boolean
foreign import categoryImpl :: Message -> Nullable String
foreign import maxAge :: Message -> Int
foreign import content :: Message -> String

subscribe :: (Message -> Effect Unit) -> Aff Unit
subscribe = subscribeImpl >>> toAffE

category :: Message -> Maybe CategoryLabel
category = categoryImpl >>> toMaybe >>> map CategoryLabel

toLetteraResponse :: Message -> LetteraResponse String
toLetteraResponse msg = LetteraResponse { maxAge: pure (maxAge msg)
                                        , body: pure (content msg)
                                        }
