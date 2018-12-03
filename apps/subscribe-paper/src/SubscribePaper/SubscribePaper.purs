module SubscribePaper.SubscribePaper where

type Product =
  { name :: String
  , price :: Number
  }

type Package =
  { name        :: String
  , price       :: Number
  , days        :: String
  , image       :: String
  , description :: String
  , checklist   :: Array { title :: String, content :: String }
  }
