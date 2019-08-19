module KSF.Login.Login where

import Prelude

data Error =
  InvalidCredentials
  | FacebookEmailMissing
  | EmailMismatchError
  | GoogleAuthInitError
  | SomethingWentWrong

data SocialLoginOption = Facebook | Google
derive instance eqSocialLoginOption :: Eq SocialLoginOption
derive instance ordSocialLoginOption :: Ord SocialLoginOption
