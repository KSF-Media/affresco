module KSF.User.Login.Google where

import Control.Promise (Promise)
import Control.Promise as Promise
import Data.Unit (Unit)
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Uncurried (EffectFn1, mkEffectFn1, runEffectFn1)

newtype Email = Email String

type AuthResponse =
  { "Zi" :: { access_token :: String }
  , w3 :: { "U3" :: Email }
  }

type Error =
  { error :: String
  , details :: String
  }

foreign import signOut_ :: Effect (Promise Unit)
foreign import isSignedIn_ :: Effect Boolean
foreign import loadGapi_ :: EffectFn1 { onSuccess :: EffectFn1 AuthResponse Unit , onFailure :: EffectFn1 Error Unit } Unit

isSignedIn :: Effect Boolean
isSignedIn = isSignedIn_

signOut :: Aff Unit
signOut = Promise.toAffE signOut_

loadGapi :: { onSuccess :: AuthResponse -> Effect Unit , onFailure :: Error -> Effect Unit } -> Effect Unit
loadGapi { onSuccess, onFailure } = runEffectFn1 loadGapi_ { onSuccess: mkEffectFn1 onSuccess, onFailure: mkEffectFn1 onFailure }
