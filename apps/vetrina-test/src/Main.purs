module VetrinaTest.Main where

import Prelude

import Bottega.Models.Order (OrderSource(..))
import Bottega.Models.PaymentMethod (PaymentMethod(..))
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Set as Set
import Effect.Console (error)
import Effect.Unsafe (unsafePerformEffect)
import KSF.Paper (Paper(..))
import KSF.User.Login as Login
import KSF.Vetrina as Vetrina
import KSF.Vetrina.Products.Premium (hblPremium)
import React.Basic (JSX)
import React.Basic.DOM as DOM
import React.Basic.DOM.Events (capture_)
import React.Basic.Hooks as React
import React.Basic.Hooks (Component, useState', (/\))

app :: Component {}
app = do
  vetrina <- Vetrina.component
  login <- Login.login
  React.component "VetrinaTest" \_ -> React.do
    user /\ setUser <- useState' Nothing
    loggingIn /\ setLoggingIn <- useState' false
    let onUserFetch (Right u) = do
          setLoggingIn false
          setUser $ Just u
        onUserFetch (Left _) = do
          error "user fetch failed"
    pure $ case loggingIn of
      false -> vetrina
        { onClose: Nothing
        , onLogin: capture_ $ setLoggingIn true
        , user
        , setUser: setUser <<< Just
        , products: [ hblPremium ]
        , unexpectedError: mempty
        , accessEntitlements: Set.fromFoldable ["hbl-365", "hbl-web"]
        , headline: Just $ vetrinaTestHeadline
        , paper: Just HBL
        , paymentMethods: [CreditCard]
        , customNewPurchase: Nothing
        , subscriptionExists: mempty
        , askAccountAlways: false
        , orderSource: PaywallSource
        }
      true -> login
        { onMerge: pure unit
        , onMergeCancelled: pure unit
        , onRegister: pure unit
        , onRegisterCancelled: pure unit
        , onUserFetch
        , onLogin: const $ pure unit
        , paper: Just HBL
        , disableSocialLogins: mempty
        }

vetrinaTestHeadline :: JSX
vetrinaTestHeadline =
  DOM.div
    { className: "text-3xl"
    , children: [ DOM.text $ "Prova HBL Digital utan kostnad i en månad"]
    }

jsApp :: {} -> JSX
jsApp = unsafePerformEffect app
