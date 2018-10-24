module KSF.Login.Login where

data Error =
  InvalidCredentials
  | FacebookEmailMissing
  | EmailMismatchError
  | GoogleAuthInitError
