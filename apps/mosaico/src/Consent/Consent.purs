module Consent.Consent where

import Prelude

import Effect (Effect)

foreign import startConsentCookieSetup :: Effect Unit

startConsentCookieSetup2 :: Effect Unit
startConsentCookieSetup2 = startConsentCookieSetup