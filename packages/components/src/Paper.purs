module KSF.Paper where

import Prelude

data Paper = HBL | ON | VN | KSF
derive instance eqPaper :: Eq Paper
