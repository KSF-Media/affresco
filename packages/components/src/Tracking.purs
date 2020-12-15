module KSF.Tracking where

import Data.DateTime (DateTime)
import Data.Maybe (Maybe(..), maybe)
import Effect (Effect)
import Foreign (Foreign)
import Prelude (Unit)
import Effect.Uncurried (EffectFn2, EffectFn3, EffectFn5, runEffectFn2, runEffectFn3, runEffectFn5)
import KSF.Helpers as Helpers

foreign import login_ :: EffectFn3 Cusno LoginMethod Result Unit
foreign import reclamation_ :: EffectFn5 Cusno Subsno DateString Claim Result Unit
foreign import tempAddressChange_ :: EffectFn5 Cusno Subsno StartDateString EndDateString Result Unit
foreign import pauseSubscription_ :: EffectFn5 Cusno Subsno StartDateString EndDateString Result Unit
foreign import unpauseSubscription_ :: EffectFn3 Cusno Subsno Result Unit
foreign import cancelSubscription_ :: EffectFn5 Cusno Subsno Reason String Result Unit
foreign import deleteTempAddressChange_ :: EffectFn5 Cusno Subsno StartDateString EndDateString Result Unit
foreign import changeName_ :: EffectFn2 Cusno Result Unit
foreign import changeAddress_ :: EffectFn2 Cusno Result Unit

type Cusno = String
type Subsno = String
type DateString = String
type Claim = String
type Reason = String
type StartDate = DateTime
type StartDateString = String
type EndDate = DateTime
type EndDateString = String
type Result = String
type LoginMethod = String

reclamation :: Cusno -> Subsno -> DateTime -> Claim -> Result -> Effect Unit
reclamation cusno subsno date claim result =
  runEffectFn5 reclamation_ cusno subsno (Helpers.formatDate date) claim result

tempAddressChange :: Cusno -> Subsno -> StartDate -> Maybe EndDate -> Result -> Effect Unit
tempAddressChange cusno subsno startDate endDate result =
  let endDateString = maybe "indefinite" Helpers.formatDate endDate
  in runEffectFn5 tempAddressChange_ cusno subsno (Helpers.formatDate startDate) endDateString result

pauseSubscription :: Cusno -> Subsno -> StartDate -> EndDate -> Result -> Effect Unit
pauseSubscription cusno subsno startDate endDate result =
  runEffectFn5 pauseSubscription_ cusno subsno (Helpers.formatDate startDate) (Helpers.formatDate endDate) result

unpauseSubscription :: Cusno -> Subsno -> Result -> Effect Unit
unpauseSubscription = runEffectFn3 unpauseSubscription_

cancelSubscription :: Cusno -> Subsno -> Reason -> String -> Result -> Effect Unit
cancelSubscription = runEffectFn5 cancelSubscription_

deleteTempAddressChange :: Cusno -> Subsno -> StartDate -> EndDate -> Result -> Effect Unit
deleteTempAddressChange cusno subsno startDate endDate result =
  runEffectFn5 deleteTempAddressChange_ cusno subsno (Helpers.formatDate startDate) (Helpers.formatDate endDate) result

changeName :: Cusno -> Result -> Effect Unit
changeName = runEffectFn2 changeName_

changeAddress :: Cusno -> Result -> Effect Unit
changeAddress = runEffectFn2 changeAddress_

login :: Maybe Cusno -> LoginMethod -> Result -> Effect Unit
login (Just cusno) method result = runEffectFn3 login_ cusno method result
login Nothing method result      = runEffectFn3 login_ "" method result

-- More about the data layer:
-- https://developers.google.com/tag-manager/devguide
type DataLayer = Array Foreign
