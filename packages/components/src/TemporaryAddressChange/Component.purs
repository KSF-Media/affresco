module KSF.TemporaryAddressChange.Component where

import Prelude

import Control.Alt ((<|>))
import Data.Array (length)
import Data.Date (Date, adjust)
import Data.Date as Date
import Data.Either (Either(..))
import Data.JSDate (toDate)
import Data.Maybe (Maybe(..), isNothing, isJust, maybe)
import Data.Nullable (toMaybe)
import Data.Time.Duration as Time.Duration
import Data.UUID (UUID)
import Data.Validation.Semigroup (validation)
import DatePicker.Component as DatePicker
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Aff as Aff
import Effect.Class (liftEffect)
import Effect.Class.Console as Console
import Effect.Now as Now
import KSF.Api.Subscription (Subsno)
import KSF.Api.Subscription (toString) as Subsno
import KSF.Grid as Grid
import KSF.InputField as InputField
import KSF.InputField.Checkbox as InputCheckbox
import KSF.User as User
import KSF.User.Cusno (Cusno)
import KSF.ValidatableForm as VF
import KSF.CountryDropDown (countryDropDown)
import KSF.TemporaryAddressChange.DropDown (pastTemporaryAddressDropDown)
import KSF.TemporaryAddressChange.Types (AddressChange)
import React.Basic (JSX)
import React.Basic.Classic (make)
import React.Basic.Classic as React
import React.Basic.DOM as DOM
import React.Basic.DOM.Events (preventDefault)
import React.Basic.Events (handler, handler_)
import KSF.Tracking as Tracking

type State =
  { startDate      :: Maybe Date
  , minStartDate   :: Maybe Date
  , endDate        :: Maybe Date
  , minEndDate     :: Maybe Date
  , streetAddress  :: Maybe String
  , zipCode        :: Maybe String
  -- | Ignored on save and inferred from zipCode
  , cityName       :: Maybe String
  , countryCode    :: Maybe String
  , temporaryName  :: Maybe String
  , isIndefinite   :: Boolean
  }

type Self = React.Self Props State

type Props =
  { subsno        :: Subsno
  , cusno         :: Cusno
  , pastAddresses :: Array AddressChange
  , nextDelivery  :: Maybe Date
  , lastDelivery  :: Maybe Date
  , editing       :: Maybe User.PendingAddressChange
  , userUuid      :: UUID
  , onCancel      :: Effect Unit
  , onLoading     :: Effect Unit
  , onSuccess     :: User.Subscription -> Effect Unit
  , onError       :: User.InvalidDateInput -> Effect Unit
  }

data Action
  = SetStartDate (Maybe Date)
  | SetMinStartDate (Maybe Date)
  | SetEndDate (Maybe Date)

data AddressChangeFields
  = StreetAddress
  | Zip
  | CityName
  | CountryCode
  | TemporaryName
instance validatableFieldAddressChangeFields :: VF.ValidatableField AddressChangeFields where
  validateField field value _serverErrors = case field of
    StreetAddress -> VF.validateEmptyField field "Adress krävs." value
    Zip           -> VF.validateZipCode field value
    CityName      -> VF.noValidation value
    CountryCode   -> VF.validateEmptyField field "Land krävs." value
    TemporaryName -> VF.noValidation value

temporaryAddressChange :: Props -> JSX
temporaryAddressChange = make component { initialState, render, didMount }

initialState :: State
initialState =
  { startDate: Nothing
  , minStartDate: Nothing
  , endDate: Nothing
  , minEndDate: Nothing
  , streetAddress: Nothing
  , zipCode: Nothing
  , cityName: Nothing
  , countryCode: Just "FI"
  , temporaryName: Nothing
  , isIndefinite: false
  }

component :: React.Component Props
component = React.createComponent "TemporaryAddressChange"

-- | Minimum temporary address change period is one week
calcMinEndDate :: Maybe Date -> Maybe Date -> Maybe Date
calcMinEndDate _ Nothing = Nothing
calcMinEndDate lastDelivery (Just startDate) = do
  -- 6 days added to the starting date = 7 (one week)
  let week = Time.Duration.Days 6.0
      diffToLastDelivery = maybe (Time.Duration.Days 0.0)
                           (\x -> Date.diff x startDate) lastDelivery
      -- Week from the delivery date of the last product in
      -- subscription
      span = if diffToLastDelivery > Time.Duration.Days 0.0 then week <> diffToLastDelivery else week
  adjust span startDate

didMount :: Self -> Effect Unit
didMount self = do
  now <- Now.nowDate
  -- We set the minimum start date two days ahead because of system issues.
  -- TODO: This could be set depending on the time of day
  let dayAfterTomorrow = adjust (Time.Duration.Days 2.0) now
      byNextIssue = max <$> dayAfterTomorrow <*> self.props.nextDelivery
  self.setState _ { minStartDate = byNextIssue <|> dayAfterTomorrow }
  case self.props.editing of
    Just p -> do
      self.setState _ { streetAddress = toMaybe p.address.streetAddress
                      , zipCode = Just p.address.zipcode
                      , cityName = toMaybe p.address.city
                      , temporaryName = toMaybe p.address.temporaryName
                      , startDate = toDate p.startDate
                      , endDate = toDate =<< toMaybe p.endDate
                      , isIndefinite = isNothing $ toMaybe p.endDate
                      }
    _ -> pure unit

render :: Self -> JSX
render self@{ state: { startDate, endDate, streetAddress, zipCode, countryCode, temporaryName, isIndefinite }} =
  DOM.div
    { className: "temporary-address-change--container"
    , children:
        [ DOM.div
            { className: "temporary-address-change--header"
            , children:
                [ DOM.div_ [ DOM.h3_ [ DOM.text titleText ] ]
                , DOM.div
                    { className: "temporary-address-change--close-icon"
                    , children: [ DOM.div { className: "close-icon" } ]
                    , onClick: handler_ self.props.onCancel
                    }
                ]
            }
        , addressChangeForm
        ]
    }
  where
    titleText =
      case self.props.editing of
        Just _ -> "Ändra datum för din adressändring"
        Nothing -> "Gör tillfällig adressändring"
    pastTempSelection =
      pastTemporaryAddressDropDown
        self.props.pastAddresses
        (\newTemp -> do
            case newTemp of
              Just tmp -> self.setState _ { streetAddress = tmp.streetAddress
                                          , zipCode = tmp.zipCode
                                          , cityName = tmp.cityName
                                          , countryCode = tmp.countryCode
                                          , temporaryName = tmp.temporaryName
                                          }
              Nothing -> mempty
        )
    addressChangeForm =
      DOM.form
          { onSubmit: handler preventDefault (\_ -> submitForm ((toDate <<< _.startDate) =<< self.props.editing) startDate (if self.state.isIndefinite then Nothing else endDate) self.props.editing { streetAddress, zipCode, cityName: Nothing, countryCode, temporaryName })
          , children:
              (if length self.props.pastAddresses == 0 || isJust self.props.editing
                 then identity
                 else ([ pastTempSelection ] <> _))
              [ DOM.div { children: [ startDayInput, isIndefiniteCheckbox ] }
              , endDayInput
              , addressInput
              , zipInput
              , cityInput
              , countryInput
              , temporaryNameInput
              , DOM.div
                  { children: [ submitFormButton ]
                  , className: "mt2 clearfix"
                  }
              ]
          }

    startDayInput =
      dateInput
        self
        { action: \newStartDate ->
                    self.setState _
                      { startDate = newStartDate
                      , minEndDate = calcMinEndDate self.props.lastDelivery newStartDate
                      }
        , value: self.state.startDate
        , minDate: self.state.minStartDate
        , maxDate: Nothing
        , disabled: false
        , label: "Börjar från"
        , id: "edit-start"
        }

    isIndefiniteCheckbox =
      InputCheckbox.inputCheckbox
        { type_: InputCheckbox.Checkbox
        , name: "indefinite"
        , checked: self.state.isIndefinite
        , onChange: \checked -> self.setState _ { isIndefinite = checked }
        , label: Just "Tillsvidare"
        , id: "edit-indefinite--" <> Subsno.toString self.props.subsno
        }

    endDayInput =
      dateInput
        self
        { action: \newEndDate -> self.setState _ { endDate = newEndDate }
        , value: self.state.endDate
        , minDate: self.state.minEndDate
        , maxDate: Nothing
        , disabled: isNothing self.state.startDate || self.state.isIndefinite
        , label: "Avslutas"
        , id: "edit-end"
        }

    addressInput =
      InputField.inputField
        { type_: InputField.Text
        , placeholder: "Gatuadress"
        , name: "address"
        , onChange: \newAddress -> self.setState _ { streetAddress = newAddress }
        , value: self.state.streetAddress
        , label: Just "Gatuadress"
        , validationError: VF.inputFieldErrorMessage $ VF.validateField StreetAddress self.state.streetAddress []
        , disabled: isJust self.props.editing
        }

    zipInput =
      InputField.inputField
        { type_: InputField.Text
        , placeholder: "Postnummer"
        , name: "zipCode"
        , onChange: \newZip -> self.setState _ { zipCode = newZip }
        , value: self.state.zipCode
        , label: Just "Postnummer"
        , validationError: VF.inputFieldErrorMessage $ VF.validateField Zip self.state.zipCode []
        , disabled: isJust self.props.editing
        }

    cityInput =
      InputField.inputField
        { type_: InputField.Text
        , placeholder: "Stad"
        , name: "city"
        , onChange: \newCity -> self.setState _ { cityName = newCity }
        , value: self.state.cityName
        , validationError: Nothing
        , label: Just "Stad"
        , disabled: isJust self.props.editing
        }

    countryInput =
      countryDropDown
        [ { countryCode: "FI", countryName: "Finland" }
        , { countryCode: "AX", countryName: "Åland" }
        ]
        (isJust self.props.editing)
        (\newCountryCode -> self.setState _ { countryCode = newCountryCode })
        self.state.countryCode

    temporaryNameInput =
      InputField.inputField
        { type_: InputField.Text
        , placeholder: "Tillfällig namnändring eller C/O"
        , name: "temporaryName"
        , onChange: \newTemporaryName -> self.setState _ { temporaryName = newTemporaryName }
        , value: self.state.temporaryName
        , validationError: Nothing
        , label: Just "Tillfällig namnändring eller C/O"
        , disabled: isJust self.props.editing
        }

    submitFormButton =
      Grid.columnThird $
        DOM.button
          { type: "submit"
          , children: [ DOM.text "Skicka" ]
          , className: "button-green"
          }

    submitForm :: Maybe Date -> Maybe Date -> Maybe Date -> Maybe User.PendingAddressChange -> AddressChange -> Effect Unit
    submitForm Nothing (Just startDate') endDate' Nothing addressChangeFormValues = do
      Aff.launchAff_ do
        validation
          -- Shows validation errors if submit button is pushed with uninitialized values
          (\_ -> liftEffect $ self.setState _
                    { streetAddress = self.state.streetAddress <|> Just ""
                    , zipCode = self.state.zipCode             <|> Just ""
                    , countryCode = self.state.countryCode     <|> Just ""
                    })
          makeTemporaryAddressChange
          (validateTemporaryAddressChangeForm addressChangeFormValues)
      where
        makeTemporaryAddressChange :: AddressChange -> Aff Unit
        makeTemporaryAddressChange { streetAddress: Just streetAddress'
                                   , zipCode: Just zipCode'
                                   , countryCode: Just countryCode'
                                   , temporaryName: temporaryName'
                                   } = do
          liftEffect $ self.props.onLoading
          User.temporaryAddressChange self.props.userUuid self.props.subsno startDate' endDate' streetAddress' zipCode' countryCode' temporaryName' >>=
            case _ of
              Right sub -> liftEffect do
                self.props.onSuccess sub
                Tracking.tempAddressChange self.props.cusno self.props.subsno startDate' endDate' "success"
              Left invalidDateInput -> liftEffect do
                self.props.onError invalidDateInput
                Tracking.tempAddressChange self.props.cusno self.props.subsno startDate' endDate' "error: invalidDateInput"
        makeTemporaryAddressChange _ = Console.error "Form should be valid, however it looks like it's not"
    submitForm (Just oldStartDate) (Just startDate') endDate' (Just _) _ = do
      self.props.onLoading
      Aff.launchAff_ $ User.editTemporaryAddressChange self.props.userUuid self.props.subsno oldStartDate startDate' endDate' >>=
        case _ of
          Right sub -> liftEffect do
            self.props.onSuccess sub
            Tracking.editTempAddressChange self.props.cusno self.props.subsno oldStartDate startDate' endDate' "success"
          Left invalidDateInput -> liftEffect do
            self.props.onError invalidDateInput
            Tracking.editTempAddressChange self.props.cusno self.props.subsno oldStartDate startDate' endDate' "error: invalidDateInput"
    submitForm _ _ _ _ _ = Console.error "Temporary address change dates were not defined."

type DateInputField =
  { action   :: Maybe Date -> Effect Unit
  , value    :: Maybe Date
  , minDate  :: Maybe Date
  , maxDate  :: Maybe Date
  , disabled :: Boolean
  , label    :: String
  , id       :: String
  }

dateInput :: Self -> DateInputField -> JSX
dateInput self { action, value, minDate, maxDate, disabled, label, id } =
  Grid.row
    [ Grid.row_ [ DOM.label_ [ DOM.text label ] ]
    , Grid.row_
        [ DatePicker.datePicker
            { onChange: (action =<< _)
            , className: "temporary-address-change--date-picker"
            , value: value
            , format: "d.M.yyyy"
            , required: true
            , minDate: minDate
            , maxDate: maxDate
            , disabled
            , locale: "sv-FI"
            }
        ]
    ]
    { extraClasses: [ "mb2" ]
    , id: id <> "--" <> Subsno.toString self.props.subsno
    }

validateTemporaryAddressChangeForm :: AddressChange -> VF.ValidatedForm AddressChangeFields AddressChange
validateTemporaryAddressChangeForm form =
  { streetAddress: _
  , zipCode: _
  , cityName: _
  , countryCode: _
  , temporaryName: _
  }
  <$> VF.validateField StreetAddress form.streetAddress []
  <*> VF.validateField Zip form.zipCode []
  <*> VF.validateField CityName form.cityName []
  <*> VF.validateField CountryCode form.countryCode []
  <*> VF.validateField TemporaryName form.temporaryName []
