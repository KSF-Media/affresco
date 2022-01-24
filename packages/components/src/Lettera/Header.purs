module Lettera.Header where

import Prelude

import Affjax.ResponseHeader as AX
import Data.Array (catMaybes, find, fromFoldable)
import Data.Int (fromString)
import Data.Maybe (Maybe(..))
import Data.String (Pattern(..), split, trim)

type Directive =
  { name :: String
  , value :: Maybe Int
  }

parseCacheControl :: String -> Array Directive
parseCacheControl =
  let toDirective :: Array String -> Maybe Directive
      toDirective [name] = Just { name, value: Nothing }
      toDirective [name, stringValue] = { name, value: _ } <<< Just <$> fromString stringValue
      toDirective _ = Nothing
  in catMaybes <<< map ((toDirective <<< map trim <<< split (Pattern "="))) <<< split (Pattern ",")

-- TODO: A more generic use should use Age header as well.
parseResponseHeaders :: Array AX.ResponseHeader -> Array Directive
parseResponseHeaders =
  join <<< fromFoldable <<< map parseCacheControl <<< map AX.value <<< find ((_ == "cache-control") <<< AX.name)

getMaxAge :: Array Directive -> Maybe Int
getMaxAge = join <<< map (_.value) <<< find (\{name} -> name == "max-age")
