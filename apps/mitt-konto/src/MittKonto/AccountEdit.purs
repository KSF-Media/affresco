module MittKonto.AccountEdit where

import Prelude

import Bottega.Models (CreditCard)
import Data.Array (cons)
import Data.Array as Array
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Aff as Aff
import Effect.Class (liftEffect)
import KSF.AsyncWrapper as AsyncWrapper
import KSF.CreditCard.Update as CreditCard
import KSF.Sentry as Sentry
import KSF.User as User
import MittKonto.ActionsWrapper as ActionsWrapper
import React.Basic (make, JSX)
import React.Basic as React
import React.Basic.DOM as DOM
import React.Basic.Events (EventHandler, handler_)

import KSF.IconAction as Aa

type Self = React.Self Props State

type Props =
  { formatIconAction  :: { element :: JSX -> JSX, description :: String, className :: String } -> JSX
  , accountEditAnchor :: String -> Boolean-> JSX -> JSX
  , accountEditDiv    :: EventHandler -> JSX -> JSX
  , logger            :: Sentry.Logger
  }

type State =
  { wrapperProgress :: AsyncWrapper.Progress JSX
  , editAction      :: Maybe AccountEditAction
  , creditCards     :: Array CreditCard
  }

data AccountEditAction
  = UpdateCreditCard

component :: React.Component Props
component = React.createComponent "AccountEdit"

accountEdit :: Props -> JSX
accountEdit = make component
  { initialState:
      { wrapperProgress: AsyncWrapper.Ready
      , editAction: Nothing
      , creditCards: []
      }
  , render
  , didMount
  }

didMount :: Self -> Effect Unit
didMount self =
  Aff.launchAff_ do
    creditCards <- User.getCreditCards
    case creditCards of
      Left err    -> liftEffect $ self.props.logger.log ("Error while fetching credit cards: " <> err) Sentry.Error
      Right cards -> case Array.head cards of
        Nothing   -> pure unit
        Just card -> liftEffect $ self.setState _ { creditCards = cards }

render :: Self -> JSX
render self =
  ActionsWrapper.actionsWrapper
    { actions: accountEditActions
    , wrapperState: self.state.wrapperProgress
    , onTryAgain: self.setState _ { wrapperProgress = AsyncWrapper.Editing creditCardUpdateComponent }
    , containerClass: "account-edit--actions-container"
    }
  where
    accountEditActions :: Array JSX
    accountEditActions =
      [ Aa.iconAction
          { iconClassName: passwordChangeClass
          , description: "Byt lösenord"
          , href: "https://www.hbl.fi/losenord"
          , onClick: Aa.Href "https://www.hbl.fi/losenord"
          }
      , Aa.iconAction
          { iconClassName: paymentHistoryClass
          , description: "Fakturor"
          , href: "/fakturor"
          , onClick: Aa.Href "/fakturor"
          }
      , Aa.iconAction
          { iconClassName: creditCardUpdateClass
          , description: "Uppdatera ditt kredit- eller bankkort"
          , href: ""
          , onClick:
              Aa.Custom $ self.setState _
                { editAction = Just UpdateCreditCard
                , wrapperProgress = AsyncWrapper.Editing creditCardUpdateComponent
                }
          }
      ] <>
      map self.props.formatIconAction
      [
        -- { element: self.props.accountEditAnchor "https://www.hbl.fi/losenord" true
        -- , description: "Byt lösenord"
        -- , className: passwordChangeClass
        -- }
      --  { element: self.props.accountEditAnchor "/fakturor" false
      --   , description: "Fakturor"
      --   , className: paymentHistoryClass
      --   }
      -- , case self.state.creditCards of
      --   [] -> mempty
      --   _ ->
      --     { element: self.props.accountEditDiv showCreditCardUpdate
      --     , description: "Uppdatera ditt kredit- eller bankkort"
      --     , className: creditCardUpdateClass
      --     }
      ]
      where
        passwordChangeClass = "account-edit--password-change"
        paymentHistoryClass = "account-edit--payment-history"
        creditCardUpdateClass = "account-edit--credit-card-update"

        showCreditCardUpdate = handler_ $
          self.setState _
            { editAction = Just UpdateCreditCard
            , wrapperProgress = AsyncWrapper.Editing creditCardUpdateComponent
            }

    creditCardUpdateComponent = CreditCard.update
      { creditCards: self.state.creditCards
      , logger: self.props.logger
      , onCancel: self.setState _ { wrapperProgress = AsyncWrapper.Ready }
      , onLoading: self.setState _ { wrapperProgress = AsyncWrapper.Loading mempty }
      , onSuccess: self.setState _ { wrapperProgress = AsyncWrapper.Success $ Just "Uppdateringen av betalningsinformationen lyckades." }
      , onError: self.setState _ { wrapperProgress = AsyncWrapper.Error "Något gick fel. Vänligen försök pånytt, eller ta kontakt med vår kundtjänst." }
      }

-- formatIconAction :: { element :: JSX -> JSX, description :: String, className :: String } -> JSX
-- formatIconAction { element, description, className } =
--   DOM.div
--     { className: "clearfix mitt-konto--account-edit-container"
--     , children:
--         [ DOM.div
--             { className: "col-12 mt1"
--             , children:
--                 [ element
--                   $ classy DOM.div "mitt-konto--icon-container col circle"
--                             [ classy DOM.div className [] ]
--                         , classy DOM.div "col col-8 pl1 pt2"
--                           [ element $ DOM.text description ]
--                         ]
--             }
--         ]
--     }

-- accountEditAnchor :: String -> Boolean -> JSX -> JSX
-- accountEditAnchor href targetBlank children =
--   DOM.a
--     { href
--     , className: ""
--     , children: [ children ]
--     , target: if targetBlank then "_blank" else ""
--     }

-- accountEditDiv :: EventHandler -> JSX -> JSX
-- accountEditDiv onClick children =
--   DOM.div
--     { className: ""
--     , children: [ children ]
--     , onClick: onClick
--     }
