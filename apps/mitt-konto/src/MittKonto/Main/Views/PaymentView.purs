module MittKonto.Main.Views.PaymentView where

import Prelude

import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Effect.Class (liftEffect)
import KSF.Alert.Component as Alert
import KSF.PaymentAccordion as PaymentAccordion
import KSF.User (User)
import KSF.User as User
import MittKonto.Main.Helpers as Helpers
import MittKonto.Main.Types as Types
import React.Basic (JSX, element)
import React.Basic.DOM as DOM
import React.Basic.Router as Router

-- | Specialized view with user's payment list
paymentView :: Types.Self -> User -> JSX
paymentView self user = DOM.div_
  [ element
      Router.link
        { to: { pathname: "/", state: {} }
        , children: [ ]
        , className: "mitt-konto--backwards"
        }
  , PaymentAccordion.payments { paymentsLoad }
  ]
  where
    paymentsLoad =
      case self.state.payments of
        Just payments -> pure payments
        Nothing ->
          Helpers.withSpinner (self.setState <<< Helpers.setLoading) do
            p <- User.getPayments user.uuid
            case p of
              Right payments -> do
                liftEffect $ self.setState _ { payments = Just payments }
                pure payments
              Left _         -> do
                liftEffect $ self.setState $ Helpers.setAlert $ Just
                  { level: Alert.warning
                  , title: "Laddningen misslyckades."
                  , message: "Något gick fel, ta kontakt med kundservice."
                  }
                pure []