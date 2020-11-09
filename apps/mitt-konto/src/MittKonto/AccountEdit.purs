module MittKonto.AccountEdit where

import Prelude

import Bottega.Models (CreditCard)
import Data.Array (null)
import Data.Array as Array
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Aff as Aff
import Effect.Class (liftEffect)
import KSF.AsyncWrapper as AsyncWrapper
import KSF.CreditCard.Update as CreditCard
import KSF.IconAction as IconAction
import KSF.Sentry as Sentry
import KSF.User as User
import MittKonto.ActionsWrapper as ActionsWrapper
import React.Basic (make, JSX)
import React.Basic as React
import React.Basic.Events (handler_)

type Self = React.Self Props State

type Props =
  { logger :: Sentry.Logger
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
      [ IconAction.iconAction
          { iconClassName: passwordChangeClass
          , description: "Byt lösenord"
          , onClick: IconAction.Href "https://www.hbl.fi/losenord" true
          }
      , IconAction.iconAction
          { iconClassName: paymentHistoryClass
          , description: "Fakturor"
          , onClick: IconAction.Href "/fakturor" false
          }
      , if not $ null self.state.creditCards
        then
          IconAction.iconAction
            { iconClassName: creditCardUpdateClass
            , description: "Uppdatera ditt kredit- eller bankkort"
            , onClick:
                IconAction.Action $ self.setState _
                  { editAction = Just UpdateCreditCard
                  , wrapperProgress = AsyncWrapper.Editing creditCardUpdateComponent
                  }
            }
        else mempty
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
