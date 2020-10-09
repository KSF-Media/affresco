module Tracking where

import Prelude

import Data.DateTime (DateTime)
import Data.Foldable (foldMap)
import Data.Maybe (Maybe, maybe)
import Effect (Effect)
import Effect.Uncurried (EffectFn2, EffectFn3, EffectFn5, runEffectFn2, runEffectFn3, runEffectFn5)
import Foreign (Foreign)
import KSF.Api.Subscription (Cusno)
import KSF.Helpers as Helpers

foreign import login_ :: EffectFn3 String LoginMethod Result Unit
foreign import reclamation_ :: EffectFn5 String Subsno DateString Claim Result Unit
foreign import tempAddressChange_ :: EffectFn5 String Subsno StartDateString EndDateString Result Unit
foreign import pauseSubscription_ :: EffectFn5 String Subsno StartDateString EndDateString Result Unit
foreign import unpauseSubscription_ :: EffectFn3 String Subsno Result Unit
foreign import deleteTempAddressChange_ :: EffectFn5 String Subsno StartDateString EndDateString Result Unit
foreign import changeName_ :: EffectFn2 String Result Unit
foreign import changeAddress_ :: EffectFn2 String Result Unit

type Subsno = String
type DateString = String
type Claim = String
type StartDateString = String
type EndDateString = String
type Result = String
type LoginMethod = String

reclamation :: Cusno -> Subsno -> DateTime -> Claim -> Result -> Effect Unit
reclamation cusno subsno date claim result =
  runEffectFn5 reclamation_ (show cusno) subsno (Helpers.formatDate date) claim result

tempAddressChange :: Cusno -> Subsno -> DateTime -> Maybe DateTime -> Result -> Effect Unit
tempAddressChange cusno subsno startDate endDate result =
  let endDateString = maybe "indefinite" Helpers.formatDate endDate
  in runEffectFn5 tempAddressChange_ (show cusno) subsno (Helpers.formatDate startDate) endDateString result

pauseSubscription :: Cusno -> Subsno -> DateTime -> DateTime -> Result -> Effect Unit
pauseSubscription cusno subsno startDate endDate result =
  runEffectFn5 pauseSubscription_ (show cusno) subsno (Helpers.formatDate startDate) (Helpers.formatDate endDate) result

unpauseSubscription :: Cusno -> Subsno -> Result -> Effect Unit
unpauseSubscription = runEffectFn3 unpauseSubscription_ <<< show

deleteTempAddressChange :: Cusno -> Subsno -> DateTime -> DateTime -> Result -> Effect Unit
deleteTempAddressChange cusno subsno startDate endDate result =
  runEffectFn5 deleteTempAddressChange_ (show cusno) subsno (Helpers.formatDate startDate) (Helpers.formatDate endDate) result

changeName :: Cusno -> Result -> Effect Unit
changeName = runEffectFn2 changeName_ <<< show

changeAddress :: Cusno -> Result -> Effect Unit
changeAddress = runEffectFn2 changeAddress_ <<< show

login :: Maybe Cusno -> LoginMethod -> Result -> Effect Unit
login maybeCusno method result = runEffectFn3 login_ (foldMap show maybeCusno) method result

-- More about the data layer:
-- https://developers.google.com/tag-manager/devguide
type DataLayer = Array Foreign
