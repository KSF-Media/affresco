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
import React.Basic as React
import React.Basic (JSX, make)
import React.Basic.DOM as DOM
import React.Basic.DOM.Events (preventDefault)
import React.Basic.Events (handler, handler_)

type Self = React.Self Props State

type State =
  { chosenCard :: Maybe CreditCard
  , validationError :: Maybe String
  }

type Props =
  { creditCards :: Array CreditCard
  , title :: JSX
  , onSubmit :: CreditCard -> Effect Unit
  , onCancel :: Effect Unit
  }

choice :: Props -> JSX
choice = make component
  { initialState
  , render
  }

component :: React.Component Props
component = React.createComponent "choice"

initialState :: State
initialState = { chosenCard: Nothing
               , validationError: Nothing
               }

render :: Self -> JSX
render self@{ setState, state: { chosenCard, validationError }, props: { creditCards, title, onSubmit, onCancel } } = 
  DOM.div_ [ header, creditCardsForm ]
  where
    header :: JSX
    header = Grid.row_
      [ DOM.div
          { className: "col col-11"
          , children: [ title ]
          }
      , DOM.div
          { className: "col-1 flex credit-card-choice--close-icon"
          , children: [ DOM.div { className: "close-icon" } ]
          , onClick: handler_ onCancel
          }
      ]

    creditCardsForm :: JSX
    creditCardsForm = DOM.div
                        { className: "credit-card-choice--form-wrapper"
                        , children: [ DOM.form
                                        { onSubmit: handler preventDefault $ (\_ -> submitForm chosenCard)
                                        , className: "credit-card-choice--form"
                                        , children: [ description
                                                    , Menu.menu
                                                        { creditCards: creditCards
                                                        , onSelect: \creditCard -> setState _ { chosenCard = Just creditCard }
                                                        }
                                                    ]
                                            `snoc` foldMap InputField.errorMessage validationError
                                            `snoc` DOM.div
                                              { children: [ submitFormButton ]
                                              , className: "mt2 clearfix"
                                              }
                                        }
                                    ]
                        }

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
    submitForm Nothing = setState _  { validationError = Just "Välj ett alternativ." }