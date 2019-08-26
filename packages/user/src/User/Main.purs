module KSF.User.Main where

import           Prelude

import           KSF.User.Login as Login
import           React.Basic    (JSX)

loginForm :: Login.Props -> JSX
loginForm = Login.login
