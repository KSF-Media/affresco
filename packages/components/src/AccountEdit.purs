module KSF.AccountEdit where

import Prelude

import Control.Alt ((<|>))
import Control.Monad.Error.Class (throwError)
import Data.Array (any, catMaybes, filter, intercalate, mapMaybe, null, (:))
import Data.Array as Array
import Data.DateTime (DateTime)
import Data.Either (Either(..))
import Data.Formatter.DateTime (FormatterCommand(..), format)
import Data.JSDate (JSDate, toDateTime)
import Data.List (fromFoldable)
import Data.Maybe (Maybe(..), fromMaybe, isNothing, maybe)
import Data.Nullable (toMaybe)
import Data.Nullable as Nullable
import Data.Set (Set)
import Data.Set as Set
import Data.Validation.Semigroup (isValid, unV)
import Effect (Effect)
import Effect.Aff as Aff
import Effect.Class (liftEffect)
import Effect.Class.Console as Console
import Effect.Exception (error)
import Effect.Now as Now
import KSF.AsyncWrapper (Progress(..))
import KSF.AsyncWrapper as AsyncWrapper
import KSF.CountryDropDown as CountryDropDown
import KSF.CreditCard.Update as CreditCard
import KSF.DescriptionList.Component as DescriptionList
import KSF.InputField as InputField
import KSF.JSError as Error
import KSF.Sentry as Sentry
import KSF.User (User)
import KSF.User as User
import KSF.ValidatableForm (class ValidatableField, ValidatedForm, inputFieldErrorMessage, validateEmptyField, validateField, validateZipCode)
import React.Basic (make, JSX)
import React.Basic as React
import React.Basic.DOM as DOM
import React.Basic.DOM.Events (capture_, preventDefault)
import React.Basic.Events (EventHandler, handler_)
import React.Basic.Events as Events

type Self = React.Self Props State

type Props =
  { formatIconAction  :: { element :: JSX -> JSX, description :: String, className :: String } -> JSX
  , accountEditAnchor :: String -> JSX -> JSX
  , accountEditDiv    :: EventHandler -> JSX -> JSX
  }

type State =
  { wrapperProgress :: AsyncWrapper.Progress JSX
  , editAction      :: Maybe AccountEditAction
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
      }
  , render
  }

render :: Self -> JSX
render self =
  DOM.div_
    [ AsyncWrapper.asyncWrapper
        { wrapperState: self.state.wrapperProgress
        , readyView: DOM.div_ accountEditActions
        , editingView: identity
        , successView: \_ -> DOM.div_ [ DOM.text "Success!" ]
        , errorView: \_ -> DOM.div_ [ DOM.text "Error!" ]
        , loadingView: identity
        } 
    ]
  where
    accountEditActions :: Array JSX
    accountEditActions = map self.props.formatIconAction
      [ { element: self.props.accountEditAnchor "https://www.hbl.fi/losenord"
        , description: "Byt lösenord"
        , className: passwordChangeClass
        }
      , { element: self.props.accountEditDiv showCreditCardUpdate
        , description: "Update credit card"
        , className: creditCardUpdateClass
        }
      ]
      where
        passwordChangeClass = "mitt-konto--password-change"
        creditCardUpdateClass = "mitt-konto--credit-card-update"

        creditCardUpdateComponent = CreditCard.update {}

        showCreditCardUpdate = handler_ $
          self.setState _
            { editAction = Just UpdateCreditCard
            , wrapperProgress = AsyncWrapper.Editing creditCardUpdateComponent
            }