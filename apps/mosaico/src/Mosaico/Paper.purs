module Mosaico.Paper where

import Data.Maybe (Maybe(..))
import KSF.Paper (Paper(..), fromString)

foreign import _mosaicoPaper :: String

-- Site wide paper code setting
mosaicoPaper :: Paper
mosaicoPaper =
  case fromString _mosaicoPaper of
    Just HBL -> HBL
    Just ON -> ON
    Just VN -> VN
    _ -> HBL
