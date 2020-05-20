module Tracking where

import Effect (Effect)
import Foreign (Foreign)
import Prelude (Unit)
import Effect.Uncurried (EffectFn2, EffectFn3, EffectFn5, runEffectFn2, runEffectFn3, runEffectFn5)

foreign import login_ :: EffectFn3 Cusno LoginMethod Result Unit
foreign import reclamation_ :: EffectFn5 Cusno Subsno DateString Claim Result Unit
foreign import tempAddressChange_ :: EffectFn5 Cusno Subsno StartDate EndDate Result Unit
foreign import pauseSubscription_ :: EffectFn5 Cusno Subsno StartDate EndDate Result Unit
foreign import unpauseSubscription_ :: EffectFn3 Cusno Subsno Result Unit
foreign import deleteTempAddressChange_ :: EffectFn5 Cusno Subsno StartDate EndDate Result Unit
foreign import changeName_ :: EffectFn2 Cusno Result Unit
foreign import changeAddress_ :: EffectFn2 Cusno Result Unit

type Cusno = String
type Subsno = Int
type DateString = String
type Claim = String
type StartDate = String
type EndDate = String
type Result = String
type LoginMethod = String
  
-- Int = subsno, String = date
reclamation :: Cusno -> Subsno -> DateString -> Claim -> Result -> Effect Unit
reclamation = runEffectFn5 reclamation_

tempAddressChange :: Cusno -> Subsno -> StartDate -> EndDate -> Result -> Effect Unit
tempAddressChange = runEffectFn5 tempAddressChange_

pauseSubscription :: Cusno -> Subsno -> StartDate -> EndDate -> Result -> Effect Unit
pauseSubscription = runEffectFn5 pauseSubscription_

unpauseSubscription :: Cusno -> Subsno -> Result -> Effect Unit
unpauseSubscription = runEffectFn3 unpauseSubscription_

deleteTempAddressChange :: Cusno -> Subsno -> StartDate -> EndDate -> Result -> Effect Unit
deleteTempAddressChange = runEffectFn5 deleteTempAddressChange_

changeName :: Cusno -> Result -> Effect Unit
changeName = runEffectFn2 changeName_

changeAddress :: Cusno -> Result -> Effect Unit
changeAddress = runEffectFn2 changeAddress_

login :: Cusno -> LoginMethod -> Result -> Effect Unit
login = runEffectFn3 login_

-- More about the data layer:
-- https://developers.google.com/tag-manager/devguide
type DataLayer = Array Foreign
