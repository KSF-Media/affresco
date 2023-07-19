module KSF.CreditCard.Choice where

import Prelude

import Bottega.Models (CreditCard)
import Data.Array (snoc)
import Data.Foldable (foldMap)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import KSF.Grid as Grid
import KSF.InputField as InputField
import KSF.CreditCard.Menu (menu) as Menu
import React.Basic (JSX)
import React.Basic.DOM as DOM
import React.Basic.DOM.Events (preventDefault)
import React.Basic.Events (handler)
import React.Basic.Hooks (Component, useState', (/\))
import React.Basic.Hooks as React

type State =
  { chosenCard :: Maybe CreditCard
  , validationError :: Maybe String
  }

type Props =
  { creditCards :: Array CreditCard
  , onSubmit :: CreditCard -> Effect Unit
  , onCancel :: Effect Unit
  }

component :: Component Props
component = React.component "choice" $ \props -> React.do
  chosenCard /\ setChosenCard <- useState' Nothing
  validationError /\ setValidationError <- useState' Nothing
  pure $ render props chosenCard setChosenCard validationError setValidationError

render
  :: Props
  -> Maybe CreditCard
  -> (Maybe CreditCard -> Effect Unit)
  -> Maybe String
  -> (Maybe String -> Effect Unit)
  -> JSX
render { creditCards, onSubmit } chosenCard setChosenCard validationError setValidationError =
  DOM.div
    { className: "credit-card-choice--form-wrapper"
    , children: [ DOM.form
                    { onSubmit: handler preventDefault $ (\_ -> submitForm chosenCard)
                    , className: "credit-card-choice--form"
                    , children: [ description
                                , Menu.menu
                                    { creditCards: creditCards
                                    , onSelect: setChosenCard <<< Just
                                    }
                                ]
                        `snoc` foldMap InputField.errorMessage validationError
                        `snoc` DOM.div
                          { children: [ submitFormButton ]
                          , className: "mitt-konto--form-submit-container"
                          }
                    }
                ]
    }
  where
    description :: JSX
    description = DOM.div
      { className: "credit-card-choice--description"
      , children: [ DOM.text "Välj ett av dina kort ur listan nedan:" ]
      }

    submitFormButton :: JSX
    submitFormButton =
      Grid.columnThird $
        DOM.button
          { type: "submit"
          , children: [ DOM.text "Fortsätt" ]
          , className: "button-green"
          }

    submitForm :: Maybe CreditCard -> Effect Unit
    submitForm (Just creditCard) = onSubmit creditCard
    submitForm Nothing = setValidationError $ Just "Välj ett alternativ."
