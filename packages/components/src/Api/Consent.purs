module KSF.Api.Consent where

type GdprConsent =
  { brand      :: String
  , consentKey :: String
  , value      :: Boolean
  }

type LegalConsent =
  { consentId :: String
  , screenName :: String
  , dateAccepted :: String
  }
