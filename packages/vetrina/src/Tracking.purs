module Tracking where

import Effect (Effect)
import Foreign (Foreign)
import Prelude (Unit)

foreign import data Tracker :: Type
foreign import newTracker :: Effect Tracker
foreign import transaction :: Effect Tracker

-- More about the data layer:
-- https://developers.google.com/tag-manager/devguide
type DataLayer = Array Foreign
