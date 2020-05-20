  module KSF.Profile.Component where

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
import React.Basic.Events (handler_)
import React.Basic.Events as Events
import Tracking as Tracking

type Self = React.Self Props State

type Props =
  { profile :: User
  , onUpdate :: User -> Effect Unit
  , logger :: Sentry.Logger
  }

type State =
  { name :: Name
  , address :: Address
  , now :: Maybe DateTime
  , editFields :: Set EditField
  , editName :: AsyncWrapper.Progress JSX
  , editAddress :: AsyncWrapper.Progress JSX
  }

type Name =
  { firstName :: Maybe String
  , lastName :: Maybe String
  }

type Address =
  { zipCode       :: Maybe String
  , countryCode   :: Maybe String
  , streetAddress :: Maybe String
  , city          :: Maybe String
  }

data EditField = EditAddress | EditName
derive instance eqEditField :: Eq EditField
derive instance ordEditField :: Ord EditField

data NameFormFields
  = FirstName
  | LastName
instance validatableFieldNameFormFields :: ValidatableField NameFormFields where
  validateField field value _serverErrors = case field of
    FirstName -> validateEmptyField field "Förnamn krävs." value
    LastName  -> validateEmptyField field "Efternamn krävs." value

data AddressFormFields
  = StreetAddress
  | City
  | Zip
  | CountryCode
instance validatableFieldAddressFormFields :: ValidatableField AddressFormFields where
  validateField field value _serverErrors = case field of
    StreetAddress -> validateEmptyField field "Adress krävs." value
    City          -> validateEmptyField field "Stad krävs." value
    Zip           -> validateZipCode field value
    CountryCode   -> validateEmptyField field "Land krävs." value

jsComponent :: React.Component Props
jsComponent = component

component :: React.Component Props
component = React.createComponent "Profile"

profile :: Props -> JSX
profile = make component
  { initialState:
      { name: { firstName: Nothing, lastName: Nothing }
      , address: { zipCode: Nothing, countryCode: Nothing, streetAddress: Nothing, city: Nothing }
      , now: Nothing
      , editFields: Set.empty
      , editName: Ready
      , editAddress: Ready
      }
  , render
  , didMount
  }

addressArray :: User.Address -> Array String
addressArray { streetAddress, zipCode, city } =
  let takeJust = catMaybes <<< map Nullable.toMaybe
  in streetAddress : takeJust [ zipCode, city ]

didMount :: Self -> Effect Unit
didMount self = do
  now <- Now.nowDateTime
  self.setState _ { now = Just now }
  resetFields self EditAddress
  resetFields self EditName

render :: Self -> JSX
render self@{ props: { profile: user } } =
  DOM.div_ $
    [ profileName
    , profileAddress
    , DescriptionList.descriptionList
        { definitions:
          showPendingAddressChanges self <>
            [ { term: "E-postadress:", description: [ DOM.text user.email ] }
            , { term: "Kundnummer:", description: [ DOM.text user.cusno ] }
            ]
        }
    ]
  where
    profileName =
      AsyncWrapper.asyncWrapper
        { wrapperState: self.state.editName
        , readyView: profileNameReady
        , editingView: \_ -> profileNameEditing
        , loadingView: profileNameLoading
        , successView: \_ -> profileNameReady
        , errorView: editingError self EditName
        }
      where
        profileNameReady = DOM.div
          { className: "profile--profile-row"
          , children:
              [ currentName
              , changeNameButton self
              ]
          }
        profileNameEditing = DOM.div_
          [ DescriptionList.descriptionList
              { definitions:
                  [ { term: "Namn:"
                    , description: [ editName self ]
                    }
                  ]
              }
          ]
        profileNameLoading spinner = DOM.div
          { className: "profile--profile-row"
          , children:
              [ currentName
              , spinner
              ]
          }
        currentName =
          DescriptionList.descriptionList
            { definitions:
                [ { term: "Namn:"
                  , description: map DOM.text $ mapMaybe toMaybe [ user.firstName, user.lastName ]
                  }
                ]
            }

    profileAddress =
      AsyncWrapper.asyncWrapper
        { wrapperState: self.state.editAddress
        , readyView: profileAddressReady
        , editingView: \_ -> profileAddressEditing
        , loadingView: profileAddressLoading
        , successView: \_ -> profileAddressReady
        , errorView: editingError self EditAddress
        }
      where
        profileAddressReady =
          DOM.div
            { className: "profile--profile-row"
            , children:
                [ currentAddress
                  -- Don't allow to edit address if already pending for a change
                , case any (isUpcomingPendingChange self.state.now) pendingChanges of
                    false
                      | isNothing $ toMaybe user.address -> addAddressButton self
                      | otherwise -> changeAddressButton self
                    true -> mempty
                ]
            }
          where
            pendingChanges = fromMaybe [] $ toMaybe user.pendingAddressChanges

        profileAddressEditing = DOM.div_
          [ DescriptionList.descriptionList
              { definitions:
                  [ { term: "Permanent adress:"
                    , description: [ editAddress self ]
                    }
                  ]
              }
          ]
        profileAddressLoading spinner =
          DOM.div
            { className: "profile--profile-row"
            , children:
                [ currentAddress
                , spinner
                ]
            }
        currentAddress =
          DescriptionList.descriptionList
            { definitions:
                [ { term: "Adress:"
                  , description: map DOM.text $ fromMaybe [] $ addressArray <$> toMaybe user.address
                  }
                ]
            }

editingError :: Self -> EditField -> String -> JSX
editingError self fieldName errMessage =
   DOM.div
     { className: "profile--edit-error"
     , children:
         [ DOM.text $ errMessage <> " "
         , DOM.span
             { className: "profile--edit-try-again"
             , children: [ DOM.text "Försök igen" ]
             , onClick: handler_ $ case fieldName of
               EditAddress -> self.setState _ { editAddress = AsyncWrapper.Ready }
               EditName    -> self.setState _ { editName    = AsyncWrapper.Ready }
             }
         ]
     }

showPendingAddressChanges :: Self -> Array DescriptionList.Definition
showPendingAddressChanges self =
  case toMaybe self.props.profile.pendingAddressChanges of
    Just pendingChanges
      -- In the pendingChanges array, we might have changes that have already happened.
      -- These should not be shown to the user.
      | upcomingChanges <- filter (isUpcomingPendingChange self.state.now) pendingChanges
      , not $ null upcomingChanges -> Array.singleton
        { term: "Addressändrig:"
        , description: map (DOM.text <<< pendingAddressChangeText) upcomingChanges
        }
    _ -> mempty

editAddress :: Self -> JSX
editAddress self =
  DOM.form
    { className: "profile--edit-address"
    , children:
        [ InputField.inputField
            { type_: InputField.Text
            , name: "streetAddress"
            , placeholder: "Gatuadress"
            , value: self.state.address.streetAddress
            , onChange: \newStreetAddr -> self.setState _ { address { streetAddress = newStreetAddr } }
            , label: Just "Gatuadress"
            , validationError: inputFieldErrorMessage $ validateField StreetAddress self.state.address.streetAddress []
            }
        , InputField.inputField
            { type_: InputField.Text
            , name: "zipCode"
            , placeholder: "Postnummer"
            , value: self.state.address.zipCode
            , onChange: \newZip -> self.setState _ { address { zipCode = newZip } }
            , label: Just "Postnummer"
            , validationError: inputFieldErrorMessage $ validateField Zip self.state.address.zipCode []
            }
        , InputField.inputField
            { type_: InputField.Text
            , name: "city"
            , placeholder: "Stad"
            , value: self.state.address.city
            , onChange: \newCity -> self.setState _ { address { city = newCity } }
            , label: Just "Stad"
            , validationError: inputFieldErrorMessage $ validateField City self.state.address.city []
            }
        , CountryDropDown.defaultCountryDropDown
            (\newCountryCode -> self.setState _ { address { countryCode = newCountryCode } })
            self.state.address.countryCode
        , submitButton
        , DOM.div { className: "profile--submit-buttons", children: [ iconClose self EditAddress ] }
        ]
    , onSubmit: Events.handler preventDefault $ \_ -> submitNewAddress $ validateAddressForm self.state.address
    }
  where
    submitButton = iconSubmit $ isValid (validateAddressForm self.state.address)

    validateAddressForm :: Address -> ValidatedForm AddressFormFields Address
    validateAddressForm form =
      { streetAddress: _
      , zipCode: _
      , city: _
      , countryCode: _
      }
      <$> validateField StreetAddress form.streetAddress []
      <*> validateField Zip form.zipCode []
      <*> validateField City form.city []
      <*> validateField CountryCode form.countryCode []

    submitNewAddress :: ValidatedForm AddressFormFields Address -> Effect Unit
    submitNewAddress = unV
      (\errors -> Console.error "Could not submit address.")
      updateAddress

    updateAddress :: Address -> Effect Unit
    updateAddress { streetAddress: Just streetAddress
                  , zipCode:       Just zipCode
                  , countryCode:   Just countryCode
                  } = do
      self.setState _ { editAddress = Loading mempty }
      Aff.launchAff_ do
        newUser <- User.updateUser self.props.profile.uuid $ User.UpdateAddress { streetAddress, zipCode, countryCode }
        case newUser of
          Right u -> liftEffect do
            self.props.onUpdate u
            self.setState _ { editAddress = Success Nothing }
            Tracking.changeAddress self.props.profile.cusno "error: unexpected error when updating name"
          Left err -> do
            liftEffect do
              self.props.logger.error $ Error.userError $ show err
              self.setState _ { editAddress = AsyncWrapper.Error "Adressändringen misslyckades." }
              Tracking.changeAddress self.props.profile.cusno "error: unexpected error when updating name"
    updateAddress _ = pure unit

editName :: Self -> JSX
editName self =
  DOM.form
    { className: "profile--edit-name"
    , children:
        [ InputField.inputField
            { type_: InputField.Text
            , name: "firstName"
            , placeholder: "Förnamn"
            , value: self.state.name.firstName
            , onChange: \newFirstName -> self.setState _ { name { firstName = newFirstName } }
            , label: Just "Förnamn"
            , validationError: inputFieldErrorMessage $ validateField FirstName self.state.name.firstName []
            }
        , InputField.inputField
            { type_: InputField.Text
            , name: "lastName"
            , placeholder: "Efternamn"
            , value: self.state.name.lastName
            , onChange: \newLastName -> self.setState _ { name { lastName = newLastName } }
            , label: Just "Efternamn"
            , validationError: inputFieldErrorMessage $ validateField LastName self.state.name.lastName []
            }
        , submitButton
        , DOM.div { className: "profile--submit-buttons", children: [ iconClose self EditName ] }
        ]
    , onSubmit: Events.handler preventDefault $ \_ -> submitNewName $ validateNameForm self.state.name
    }
    where
      submitButton = iconSubmit $ isValid (validateNameForm self.state.name)

      validateNameForm :: Name -> ValidatedForm NameFormFields Name
      validateNameForm form =
        { firstName: _
        , lastName: _
        }
        <$> validateField FirstName form.firstName []
        <*> validateField LastName form.lastName []

      submitNewName :: ValidatedForm NameFormFields Name -> Effect Unit
      submitNewName = unV
        (\errors -> Console.error "Could not submit name.")
        updateName

      updateName :: Name -> Effect Unit
      updateName { firstName: Just fname, lastName: Just lname } = do
        self.setState _ { editName = Loading mempty }
        Aff.launchAff_ do
          newUser <- User.updateUser self.props.profile.uuid $ User.UpdateName { firstName: fname, lastName: lname }
          case newUser of
            Right u -> liftEffect do
              Tracking.changeName self.props.profile.cusno "success"
              self.props.onUpdate u
              self.setState _ { editName = Success Nothing }
              Tracking.changeName self.props.profile.cusno "error: unexpected error when updating name"
            Left err -> do
              liftEffect do
                self.props.logger.error $ Error.userError $ show err
                self.setState _ { editName = AsyncWrapper.Error "Namnändringen misslyckades." }
                Tracking.changeName self.props.profile.cusno "error: unexpected error when updating name"
              throwError $ error "Unexpected error when updating name."
      updateName _ = pure unit

iconSubmit :: Boolean -> JSX
iconSubmit isFormValid = DOM.div
  { className: "editable--submit"
  , children:
      [ DOM.button
          { type: "submit"
          , children: [ DOM.text "Spara" ]
          , className: "button-green"
          , disabled: not isFormValid
          }
      ]
  }

iconClose :: Self -> EditField -> JSX
iconClose self field = DOM.div
  { className: "close-icon"
  , onClick: capture_ do
      resetFields self field
      self.setState _ { editFields = Set.delete field self.state.editFields }
      switchEditProgress self field Ready
  }

changeAttributeButton :: Self -> EditField -> JSX
changeAttributeButton = editButton "Ändra"

addAddressButton :: Self -> JSX
addAddressButton self = editButton "Lägg till adress" self EditAddress

changeAddressButton :: Self -> JSX
changeAddressButton self = changeAttributeButton self EditAddress

changeNameButton :: Self -> JSX
changeNameButton self = changeAttributeButton self EditName

editButton :: String -> Self -> EditField -> JSX
editButton buttonText self field =
  DOM.div
    { className: "profile--edit-attribute-button"
    , children:
        [ DOM.div
            { className: "edit-icon circle"
            , onClick: capture_ startEdit
            }
        , DOM.span
            { className: "profile--edit-text"
            , onClick: capture_ startEdit
            , children:
                [ DOM.u_ [ DOM.text buttonText ] ]
            }
        ]
    }
  where
    startEdit = do
      self.setState _ { editFields = Set.insert field self.state.editFields }
      switchEditProgress self field (Editing mempty)

switchEditProgress :: Self -> EditField -> AsyncWrapper.Progress JSX -> Effect Unit
switchEditProgress self EditName progress = self.setState _ { editName = progress }
switchEditProgress self EditAddress progress = self.setState _ { editAddress = progress }

isUpcomingPendingChange :: Maybe DateTime -> User.PendingAddressChange -> Boolean
isUpcomingPendingChange Nothing _ = true
isUpcomingPendingChange (Just now) { startDate } =
  maybe true (_ > now) $ toDateTime startDate

pendingAddressChangeText :: User.PendingAddressChange -> String
pendingAddressChangeText { address, startDate, endDate } =
  let addressString = formatAddress address
      pendingPeriod = formatDateString startDate
  in addressString <> " (fr.o.m. " <> pendingPeriod <> ")"

resetFields :: Self -> EditField -> Effect Unit
resetFields self EditAddress =
  self.setState _
    { address =
        { streetAddress: _.streetAddress <$> toMaybe self.props.profile.address
        , countryCode: (_.countryCode <$> toMaybe self.props.profile.address) <|> Just "FI"
        , zipCode: toMaybe <<< _.zipCode =<< toMaybe self.props.profile.address
        , city: toMaybe <<< _.city =<< toMaybe self.props.profile.address
        }
    }
resetFields self EditName =
  self.setState _ { name = { firstName: toMaybe self.props.profile.firstName
                           , lastName:  toMaybe self.props.profile.lastName
                           }
                  }

formatAddress :: User.DeliveryAddress -> String
formatAddress { temporaryName, streetAddress, zipcode, city } =
  (maybe "" (_ <> ", ") $ toMaybe temporaryName) <>
  intercalate ", " [ fromMaybe "-" $ toMaybe streetAddress, zipcode, fromMaybe "-" $ toMaybe city ]

formatDateString :: JSDate -> String
formatDateString startDate
  | Just startString <- formatDate startDate = startString
  | otherwise = mempty

formatDate :: JSDate -> Maybe String
formatDate date = format formatter <$> toDateTime date
  where
    dot = Placeholder "."
    formatter = fromFoldable
      [ DayOfMonthTwoDigits
      , dot
      , MonthTwoDigits
      , dot
      , YearFull
      ]
