module KSF.User.JS where

import           Prelude

import           Data.Either       (Either (..), either)
import           Effect            (Effect)
import           Effect.Aff        (Aff)
import           Effect.Aff        as Aff
import           Effect.Aff.Compat (EffectFn2)
import           Effect.Exception  (Error)
import           Effect.Uncurried  (EffectFn1, mkEffectFn1, mkEffectFn2)
import           KSF.User          as User
import           KSF.User.Login    as Login
import           React.Basic       as React

jsLoginForm :: React.ReactComponent Login.JSProps
jsLoginForm = Login.jsComponent

-- | JS-compatible version of 'logout', takes a callback
--   that will be called when it's done.
jsLogout :: EffectFn2 (Effect Unit) (Error -> Effect Unit) Unit
jsLogout = mkEffectFn2 $ \onSuccess onFailure -> Aff.runAff_ (either onFailure (const onSuccess)) User.logout
