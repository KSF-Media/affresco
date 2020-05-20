module Tracking where

import Effect (Effect)
import Foreign (Foreign)
import Prelude (Unit)
import Data.Function.Uncurried (Fn2, Fn4, runFn2, runFn4)

foreign import data Tracker :: Type
foreign import newTracker :: Effect Tracker
foreign import pushPageLoad :: Tracker -> Effect Unit
foreign import reclamationEvent_ :: Fn4 Tracker Subsno DateString Claim (Effect Unit)
foreign import tempAdressChange_ :: Fn2 Tracker Subsno (Effect Unit)
foreign import pauseSubscription_ :: Fn2 Tracker Subsno (Effect Unit)

type Subsno = Int
type DateString = String
type Claim = String
  
-- Int = subsno, String = date
reclamationEvent :: Tracker -> Subsno -> DateString -> Claim -> Effect Unit
reclamationEvent = runFn4 reclamationEvent_

tempAdressChange :: Tracker -> Subsno -> Effect Unit
tempAdressChange = runFn2 tempAdressChange_

pauseSubscription :: Tracker -> Subsno -> Effect Unit
pauseSubscription = runFn2 pauseSubscription_

-- More about the data layer:
-- https://developers.google.com/tag-manager/devguide
type DataLayer = Array Foreign
