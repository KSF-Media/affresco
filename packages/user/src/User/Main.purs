module KSF.User.Main where

import           Prelude

import           Effect         (Effect)
import           Effect.Aff     as Aff
import           KSF.User.Login as Login
import           KSF.User.User  as User
import           React.Basic    (JSX)
import           React.Basic    as React

jsLoginForm :: React.ReactComponent Login.JSProps
jsLoginForm = Login.jsComponent

loginForm :: Login.Props -> JSX
loginForm = Login.login

-- | JS-compatible version of 'logout', takes a callback
--   that will be called when it's done.
jsLogout :: Effect Unit -> Effect Unit
jsLogout callback = Aff.runAff_ (\_ -> callback) User.logout
