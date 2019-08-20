module KSF.Login.Facebook.Success where

import Prelude

import Data.Maybe (isJust)
import Effect (Effect)
import KSF.LocalStorage as LocalStorage

successKey :: String
successKey = "KSF_FACEBOOK_LOGIN_SUCCESS"

setFacebookSuccess :: Effect Unit
setFacebookSuccess = LocalStorage.setItem successKey "true"

unsetFacebookSuccess :: Effect Unit
unsetFacebookSuccess = LocalStorage.removeItem successKey

getFacebookSuccess :: Effect Boolean
getFacebookSuccess = isJust <$> LocalStorage.getItem successKey
