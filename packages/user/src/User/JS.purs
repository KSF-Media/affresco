module KSF.User.JS where

import Prelude

import Effect (Effect)
import Effect.Aff as Aff
import Effect.Uncurried (EffectFn1, mkEffectFn1)
import KSF.User as User
import KSF.User.Login as Login
import React.Basic as React

jsLoginForm :: React.ReactComponent Login.JSProps
jsLoginForm = Login.jsComponent

-- | JS-compatible version of 'logout', takes a callback
--   that will be called when it's done.
jsLogout :: EffectFn1 (Effect Unit) Unit
jsLogout = mkEffectFn1 \callback -> Aff.runAff_ (\_ -> callback) User.logout
