module Vetrina.Purchase.AccountForm where

import Prelude

import Control.Alt ((<|>))
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Nullable (toMaybe)
import Data.Validation.Semigroup (unV)
import Effect (Effect)
import Effect.Aff as Aff
import Effect.Class (liftEffect)
import KSF.CountryDropDown as CountryDropdown
import KSF.InputField as InputField
import KSF.Spinner as Spinner
import KSF.User (User, UserError, UserUpdate(..))
import KSF.User as User
import KSF.ValidatableForm (CommonContactInformation, CommonContactInformationFormField(..), emptyCommonContactInformation)
import KSF.ValidatableForm as Form
import React.Basic (JSX)
import React.Basic.DOM as DOM
import React.Basic.DOM.Events (preventDefault)
import React.Basic.Events (handler)
import React.Basic.Hooks (Component, component, fragment, useState, (/\))
import React.Basic.Hooks as React

type Props =
  { user :: User
  , retryPurchase :: User -> Effect Unit
  , setLoading :: Maybe Spinner.Loading -> Effect Unit
  , onError :: UserError -> Effect Unit
  }

type State =
  { contactForm :: CommonContactInformation
  }

type Self =
  { state :: State
  , setState :: (State -> State) -> Effect Unit
  }

mkAccountForm :: Component Props
mkAccountForm = do
  component "insufficientAccount" \props -> React.do
    -- Set initial form values from user if defined
    let initialContactInformation =
          emptyCommonContactInformation
            { firstName = toMaybe props.user.firstName
            , lastName = toMaybe props.user.lastName
            , streetAddress = _.streetAddress <$> toMaybe props.user.address
            , city = toMaybe <<< _.city =<< toMaybe props.user.address
            , zipCode =  toMaybe <<< _.zipCode =<< toMaybe props.user.address
            , countryCode = _.countryCode <$> toMaybe props.user.address <|> Just "FI"
            }
    state /\ setState <- useState { contactForm: initialContactInformation }
    let self = { state, setState }
    pure $ render props self

render :: Props -> Self -> JSX
render props self@{ state: { contactForm } } = fragment
  [ DOM.h1
      { className: "vetrina--headline"
      , children: [ DOM.text "Adressuppgifter" ]
      }
  , DOM.form
    { className: "vetrina--form"
    , onSubmit: handler preventDefault $ (\_ -> submitForm formValidations)
    , children:
        [ DOM.div
            { className: "vetrina--step"
            , children:
                [ DOM.span
                    { className: "vetrina--step__headline"
                    , children: [ DOM.text "Dina uppgifter" ]
                    }
                , DOM.text "STEG 2 / 2 KONTOINFORMATION"
                ]
            }
        , InputField.inputField
            { type_: InputField.Text
            , label: Just "Förnamn"
            , name: "firstName"
            , placeholder: "Förnamn"
            , onChange: \newFirstName -> self.setState _ { contactForm { firstName = newFirstName }}
            , validationError: Form.inputFieldErrorMessage $ Form.validateField FirstName contactForm.firstName []
            , value: contactForm.firstName
            }
        , InputField.inputField
            { type_: InputField.Text
            , label: Just "Efternamn"
            , name: "lastName"
            , placeholder: "Efternamn"
            , onChange: \newLastName -> self.setState _ { contactForm { lastName = newLastName }}
            , validationError: Form.inputFieldErrorMessage $ Form.validateField LastName contactForm.lastName []
            , value: contactForm.lastName
            }
        , InputField.inputField
            { type_: InputField.Text
            , label: Just "Adress"
            , name: "streetAddress"
            , placeholder: "Adress"
            , onChange: \newStreetAddress -> self.setState _ { contactForm { streetAddress = newStreetAddress }}
            , validationError: Form.inputFieldErrorMessage $ Form.validateField StreetAddress contactForm.streetAddress []
            , value: contactForm.streetAddress
            }
        , InputField.inputField
            { type_: InputField.Text
            , label: Just "Stad"
            , name: "city"
            , placeholder: "Stad"
            , onChange: \newCity -> self.setState _ { contactForm { city = newCity }}
            , validationError: Form.inputFieldErrorMessage $ Form.validateField City contactForm.city []
            , value: contactForm.city
            }
        , InputField.inputField
            { type_: InputField.Text
            , label: Just "Postnummer"
            , name: "zipCode"
            , placeholder: "Postnummer"
            , onChange: \newZipCode -> self.setState _ { contactForm { zipCode = newZipCode }}
            , validationError: Form.inputFieldErrorMessage $ Form.validateField Zip contactForm.zipCode []
            , value: contactForm.zipCode
            }
        , CountryDropdown.defaultCountryDropDown
            (\newCountry -> self.setState _ { contactForm { countryCode = newCountry } })
            (contactForm.countryCode <|> _.countryCode <$> toMaybe props.user.address)
        , DOM.input
            { type: "submit"
            , className: "vetrina--button vetrina--button__contact_information"
            , disabled: Form.isFormInvalid formValidations
            , value: "Bekräfta och gå vidare"
            , onSubmit: handler preventDefault $ (\_ -> submitForm formValidations)
            }
        ]
    }
  ]
  where
    formValidations =
      { firstName: _
      , lastName: _
      , streetAddress: _
      , city: _
      , zipCode: _
      , countryCode: _
      }
      <$> Form.validateField FirstName self.state.contactForm.firstName mempty
      <*> Form.validateField LastName  self.state.contactForm.lastName  mempty
      <*> Form.validateField StreetAddress self.state.contactForm.streetAddress mempty
      <*> Form.validateField City self.state.contactForm.city mempty
      <*> Form.validateField Zip self.state.contactForm.zipCode mempty
      <*> Form.validateField Country self.state.contactForm.countryCode mempty

    submitForm = unV
      (\errors -> do
          let form = self.state.contactForm
          self.setState _
            { contactForm
                { firstName       = form.firstName     <|> Just ""
                , lastName        = form.lastName      <|> Just ""
                , streetAddress   = form.streetAddress <|> Just ""
                , city            = form.city          <|> Just ""
                , zipCode         = form.zipCode       <|> Just ""
                , countryCode     = form.countryCode   <|> Just ""
                }
            })
      (\validForm -> do
          let updateAddress = do
                firstName     <- validForm.firstName
                lastName      <- validForm.lastName
                streetAddress <- validForm.streetAddress
                city          <- validForm.city
                zipCode       <- validForm.zipCode
                countryCode   <- validForm.countryCode
                pure { firstName, lastName, streetAddress, city, zipCode, countryCode }
          case updateAddress of
            Nothing -> pure unit
            Just addr -> Aff.launchAff_ do
              -- TODO: This `finally` thing could be implemented as a generic solution in `Spinner`
              -- FIXME: STILL FLASHES THE FORM AFTER PATCHING USER!!
              liftEffect $ props.setLoading (Just Spinner.Loading)
              Aff.finally
                (liftEffect $ props.setLoading Nothing)
                do eitherUser <- User.updateUser props.user.uuid $ UpdateFull addr
                   case eitherUser of
                     Right user -> liftEffect $ props.retryPurchase user
                     Left err -> liftEffect $ props.onError err

      )
