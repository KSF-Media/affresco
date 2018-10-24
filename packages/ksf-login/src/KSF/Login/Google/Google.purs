module KSF.Login.Google where

import Data.Unit (Unit)
import Effect (Effect)
import Effect.Uncurried (EffectFn1, mkEffectFn1, runEffectFn1)
import Web.DOM as DOM

newtype Email = Email String

type AuthResponse =
  { "Zi" :: { access_token :: String }
  , w3 :: { "U3" :: Email }
  }

type Error =
  { error :: String
  , details :: String
  }

foreign import attachClickHandler_
  :: forall options.
     EffectFn1
       { node :: DOM.Node
       , options :: { | options }
       , onSuccess :: EffectFn1 AuthResponse Unit
       , onFailure :: EffectFn1 Error Unit
       }
       Unit
foreign import signOut_ :: EffectFn1 (Effect Unit) Unit
foreign import isSignedIn_ :: Effect Boolean

attachClickHandler
  :: forall options
   . { node :: DOM.Node
     , options :: { | options }
     , onSuccess :: AuthResponse -> Effect Unit
     , onFailure :: Error -> Effect Unit
     }
  -> Effect Unit
attachClickHandler { node, options, onSuccess, onFailure } =
  runEffectFn1 attachClickHandler_ { node, options, onSuccess: mkEffectFn1 onSuccess, onFailure: mkEffectFn1 onFailure }

isSignedIn :: Effect Boolean
isSignedIn = isSignedIn_

signOut :: Effect Unit -> Effect Unit
signOut = runEffectFn1 signOut_
