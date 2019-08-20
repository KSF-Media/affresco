module KSF.Login.Types where

import Prelude

data Error =
  InvalidCredentials
  | FacebookEmailMissing
  | EmailMismatchError
  | GoogleAuthInitError
  | SomethingWentWrong

data SocialLoginProvider = Facebook | Google
derive instance eqSocialLoginOption :: Eq SocialLoginProvider
derive instance ordSocialLoginOption :: Ord SocialLoginProvider
