module KSF.User.JS where

import           Prelude

import           Data.Either      (Either (..))
import           Effect           (Effect)
import           Effect.Aff       as Aff
import           Effect.Exception (Error)
import           Effect.Uncurried (EffectFn1, EffectFn2, mkEffectFn2,
                                   runEffectFn1)
import           KSF.User         as User
import           KSF.User.Login   as Login
import           React.Basic      as React

jsLoginForm :: React.ReactComponent Login.JSProps
jsLoginForm = Login.jsComponent

-- | JS-compatible version of 'logout', takes onSuccess and onFailure callbacks
jsLogout :: EffectFn2 (Effect Unit) (EffectFn1 Error Unit) Unit
jsLogout = mkEffectFn2 $ (\onSuccess onFailure -> Aff.launchAff_ $
                             User.logout (case _ of
                               Right _  -> onSuccess
                               Left err -> runEffectFn1 onFailure err))
