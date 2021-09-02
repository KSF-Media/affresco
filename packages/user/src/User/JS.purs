module KSF.User.JS where

import           Prelude

import           Data.Either      (Either (..))
import           Data.Maybe       (Maybe (..))
import           Effect           (Effect)
import           Effect.Aff       as Aff
import           Effect.Exception (Error)
import           Effect.Uncurried (EffectFn1, EffectFn2, EffectFn3, mkEffectFn2,
                                   mkEffectFn3, runEffectFn1)
import           Effect.Unsafe    (unsafePerformEffect)
import           KSF.Api          (InvalidateCache (..))
import           KSF.User         (User)
import           KSF.User         as User
import           KSF.User.Login   as Login
import           React.Basic      as React

jsLoginForm :: React.ReactComponent Login.JSProps
jsLoginForm = unsafePerformEffect Login.jsComponent

-- | JS-compatible version of 'logout', takes onSuccess and onFailure callbacks
jsLogout :: EffectFn2 (Effect Unit) (EffectFn1 Error Unit) Unit
jsLogout = mkEffectFn2 $ (\onSuccess onFailure -> Aff.launchAff_ $
                             User.logout (case _ of
                               Right _  -> onSuccess
                               Left err -> runEffectFn1 onFailure err))

jsMagicLogin :: EffectFn3 Boolean (EffectFn1 User Unit) (EffectFn1 String Unit) Unit
jsMagicLogin = mkEffectFn3 doMagicLogin
  where
    doMagicLogin invalidateCache onSuccess onFailure =
      Aff.launchAff_ do
        let clearCache =
              if invalidateCache
              then Just InvalidateCache
              else Nothing
        User.magicLogin clearCache case _ of
          Right user -> runEffectFn1 onSuccess user
          Left err   -> runEffectFn1 onFailure (show err)
