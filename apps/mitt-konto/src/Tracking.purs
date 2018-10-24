module Tracking where

import Effect (Effect)
import Foreign (Foreign)
import Prelude (Unit)

foreign import tracker_ :: Tracker

-- More about the data layer:
-- https://developers.google.com/tag-manager/devguide
type DataLayer = Array Foreign

type Tracker =
  { dataLayer :: DataLayer
  , pushPageLoad :: Effect Unit
  }

tracker :: Tracker
tracker = tracker_

pushPageLoad :: Effect Unit
pushPageLoad = tracker.pushPageLoad
