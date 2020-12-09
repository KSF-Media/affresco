module Vetrina.Purchase.InsufficientAccount where

import Prelude

import Effect (Effect)
import Data.Maybe (Maybe)
import React.Basic (JSX)
import React.Basic.DOM as DOM

import KSF.User as User


type Props =
  { user    :: Maybe User.User
  , onRetry :: Effect Unit
  }

insufficientAccount :: Props -> JSX
insufficientAccount props =
  DOM.div
    { className: "vetrina--insufficient-account"
    , children:
       [ DOM.h1
           { className: "vetrina--headline"
           , children: [ DOM.text "Adressuppgifter" ]
           }
       , DOM.p { children: [ DOM.text "Vänligen komplettera ditt konto med följande uppgifter" ] }
       , DOM.form
        { className: "vetrina-form"
        , children:
          [ DOM.label { htmlFor: "account--first-name" }
          , DOM.input { type: "text", name: "account--first-name" }

          , DOM.label { htmlFor: "account--last-name" }
          , DOM.input { type: "text", name: "account--last-name" }

          , DOM.label { htmlFor: "account--street-address" }
          , DOM.input { type: "text", name: "account--street-address" }

          , DOM.label { htmlFor: "account--city" }
          , DOM.input { type: "text", name: "account--city" }

          , DOM.label { htmlFor: "account--postal-code" }
          , DOM.input { type: "text", name: "account--postal-code" }

          , DOM.label { htmlFor: "account--country" }
          , DOM.input { type: "text", name: "account--country" }
          ]
        }
      ]
    }
