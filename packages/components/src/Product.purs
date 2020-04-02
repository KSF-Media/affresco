module KSF.Product where

import Data.Maybe (Maybe)
import KSF.Api.Package as Package

type Product =
  { name        :: String
  , id          :: String
  , description :: Array String
  , price       :: Number
  , packageName :: Package.PackageName
  , imageUrl    :: Maybe String -- TODO: What to do with this?
  }
