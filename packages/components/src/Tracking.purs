module KSF.Tracking where

import Bottega.Models as Bottega
import Data.Date (Date)
import Data.Maybe (Maybe, maybe)
import Data.Nullable (Nullable, toNullable)
import Effect (Effect)
import Foreign (Foreign)
import Prelude (Unit)
import Effect.Uncurried (EffectFn1, EffectFn2, EffectFn3, EffectFn5, EffectFn6, EffectFn7, runEffectFn1, runEffectFn2, runEffectFn3, runEffectFn5, runEffectFn6, runEffectFn7)
import KSF.Helpers as Helpers
import KSF.Api.Subscription (Subsno)
import KSF.User.Cusno

foreign import login_ :: EffectFn3 (Nullable Cusno) LoginMethod Result Unit
foreign import reclamation_ :: EffectFn5 Cusno Subsno DateString Claim Result Unit
foreign import tempAddressChange_ :: EffectFn5 Cusno Subsno StartDateString EndDateString Result Unit
foreign import editTempAddressChange_ :: EffectFn6 Cusno Subsno StartDateString StartDateString EndDateString Result Unit
foreign import pauseSubscription_ :: EffectFn5 Cusno Subsno StartDateString EndDateString Result Unit
foreign import editSubscriptionPause_ :: EffectFn7 Cusno Subsno StartDateString EndDateString StartDateString EndDateString Result Unit
foreign import unpauseSubscription_ :: EffectFn3 Cusno Subsno Result Unit
foreign import deleteTempAddressChange_ :: EffectFn5 Cusno Subsno StartDateString EndDateString Result Unit
foreign import updateCreditCard_ :: EffectFn5 Cusno (Nullable Subsno) CreditCard CreditCardRegisterNumber Result Unit
foreign import changeName_ :: EffectFn2 Cusno Result Unit
foreign import changeEmail_ :: EffectFn2 Cusno Result Unit
foreign import changeAddress_ :: EffectFn2 Cusno Result Unit
foreign import deletePendingAddressChanges_ :: EffectFn2 Cusno Result Unit
foreign import updateResetPassword_ :: EffectFn1 Result Unit

type DateString = String
type Claim = String
type StartDate = Date
type StartDateString = String
type EndDate = Date
type EndDateString = String
type Result = String
type LoginMethod = String
type CreditCardRegisterNumber = String

type CreditCard =
  { id              :: Int
  , paymentMethodId :: Int
  , maskedPan       :: String
  , expiryDate      :: String
  }
readBottegaCreditCard :: Bottega.CreditCard -> CreditCard
readBottegaCreditCard { id: Bottega.CreditCardId id, expiryDate, paymentMethodId: Bottega.PaymentMethodId paymentMethodId, maskedPan } =
  { expiryDate
  , id
  , maskedPan
  , paymentMethodId
  }

reclamation :: Cusno -> Subsno -> Date -> Claim -> Result -> Effect Unit
reclamation cusno subsno date claim result =
  runEffectFn5 reclamation_ cusno subsno (Helpers.formatDate date) claim result

tempAddressChange :: Cusno -> Subsno -> StartDate -> Maybe EndDate -> Result -> Effect Unit
tempAddressChange cusno subsno startDate endDate result =
  let endDateString = maybe "indefinite" Helpers.formatDate endDate
  in runEffectFn5 tempAddressChange_ cusno subsno (Helpers.formatDate startDate) endDateString result

editTempAddressChange :: Cusno -> Subsno -> StartDate -> StartDate -> Maybe EndDate -> Result -> Effect Unit
editTempAddressChange cusno subsno oldStartDate startDate endDate result =
  let endDateString = maybe "indefinite" Helpers.formatDate endDate
  in runEffectFn6 editTempAddressChange_ cusno subsno (Helpers.formatDate oldStartDate) (Helpers.formatDate startDate) endDateString result

pauseSubscription :: Cusno -> Subsno -> StartDate -> EndDate -> Result -> Effect Unit
pauseSubscription cusno subsno startDate endDate result =
  runEffectFn5 pauseSubscription_ cusno subsno (Helpers.formatDate startDate) (Helpers.formatDate endDate) result

editSubscriptionPause  :: Cusno -> Subsno -> StartDate -> EndDate -> StartDate -> EndDate -> Result -> Effect Unit
editSubscriptionPause cusno subsno oldStartDate oldEndDate newStartDate newEndDate result =
  runEffectFn7 editSubscriptionPause_ cusno subsno (Helpers.formatDate oldStartDate) (Helpers.formatDate oldEndDate) (Helpers.formatDate newStartDate) (Helpers.formatDate newEndDate) result

unpauseSubscription :: Cusno -> Subsno -> Result -> Effect Unit
unpauseSubscription = runEffectFn3 unpauseSubscription_

deleteTempAddressChange :: Cusno -> Subsno -> StartDate -> Maybe EndDate -> Result -> Effect Unit
deleteTempAddressChange cusno subsno startDate endDate result =
  let endDateString = maybe "indefinite" Helpers.formatDate endDate
  in runEffectFn5 deleteTempAddressChange_ cusno subsno (Helpers.formatDate startDate) endDateString result

updateCreditCard :: Cusno -> Maybe Subsno -> CreditCard -> CreditCardRegisterNumber -> Result -> Effect Unit
updateCreditCard cusno subsno oldCreditCard registerNumber result =
  runEffectFn5 updateCreditCard_ cusno (toNullable subsno) oldCreditCard registerNumber result

changeName :: Cusno -> Result -> Effect Unit
changeName = runEffectFn2 changeName_

changeEmail :: Cusno -> Result -> Effect Unit
changeEmail = runEffectFn2 changeEmail_

changeAddress :: Cusno -> Result -> Effect Unit
changeAddress = runEffectFn2 changeAddress_

deletePendingAddressChanges :: Cusno -> Result -> Effect Unit
deletePendingAddressChanges = runEffectFn2 deletePendingAddressChanges_

updateResetPassword :: Result -> Effect Unit
updateResetPassword = runEffectFn1 updateResetPassword_

login :: Maybe Cusno -> LoginMethod -> Result -> Effect Unit
login cusno method result = runEffectFn3 login_ (toNullable cusno) method result

-- More about the data layer:
-- https://developers.google.com/tag-manager/devguide
type DataLayer = Array Foreign
