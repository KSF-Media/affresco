  module KSF.Profile.Component where

import Prelude

import Control.Alt ((<|>))
import Control.Monad.Error.Class (throwError)
import Data.Array (any, catMaybes, filter, intercalate, length, mapMaybe, null, (:))
import Data.Array as Array
import Data.Date as Date
import Data.DateTime (date, DateTime)
import Data.Date (Date)
import Data.Time.Duration (Days(..))
import Data.Either (Either(..))
import Data.JSDate (JSDate, toDate)
import Data.Maybe (Maybe(..), fromMaybe, isNothing, maybe)
import Data.Monoid (guard)
import Data.Nullable (toMaybe)
import Data.Nullable as Nullable
import Data.Set (Set)
import Data.Set as Set
import Data.Validation.Semigroup (isValid, validation)
import DatePicker.Component as DatePicker
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
import KSF.Grid as Grid
import KSF.Helpers (formatDateDots)
import KSF.InputField as InputField
import KSF.JSError as Error
import KSF.Sentry as Sentry
import KSF.User (User, UserError(UniqueViolation), PendingAddressChange (..))
import KSF.User as User
import KSF.User.Cusno as Cusno
import KSF.ValidatableForm (class ValidatableField, ValidatedForm, inputFieldErrorMessage, validateEmailAddress, validateEmptyField, validateField, validatePhone, validateFinnishZipCode, validateZipCode)
import React.Basic (JSX)
import React.Basic.Classic (make)
import React.Basic.Classic as React
import React.Basic.DOM as DOM
import React.Basic.DOM.Events (capture_, preventDefault)
import React.Basic.Events (handler_)
import React.Basic.Events as Events
import KSF.Tracking as Tracking

type Self = React.Self Props State

type Props =
  { profile :: User
  , onUpdate :: User -> Effect Unit
  , logger :: Sentry.Logger
  }

type State =
  { name :: Name
  , address :: Address
  , email :: Maybe String
  , phone :: Maybe String
  , now :: Maybe Date
  , changeDate :: Maybe Date
  , editFields :: Set EditField
  , editName :: AsyncWrapper.Progress JSX
  , editEmail :: AsyncWrapper.Progress JSX
  , editPhone :: AsyncWrapper.Progress JSX
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

data EditField = EditAddress | EditEmail | EditName | EditPhone
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
  | Zip (Maybe String)
  | CountryCode
instance validatableFieldAddressFormFields :: ValidatableField AddressFormFields where
  validateField field value _serverErrors = case field of
    StreetAddress -> validateEmptyField field "Adress krävs." value
    City          -> validateEmptyField field "Stad krävs." value
    -- Sweden also has five number zips and we don't look up further
    -- than that so
    Zip country   -> if country `Array.elem` [Just "FI", Just "AX", Just "SE"]
                       then validateFinnishZipCode field value
                       else validateZipCode field value
    CountryCode   -> validateEmptyField field "Land krävs." value

data EmailFormFields
  = Email
instance validatableFieldEmailFormFields :: ValidatableField EmailFormFields where
  validateField field value _serverErrors = case field of
    Email -> validateEmailAddress field value

data PhoneFormFields
  = Phone
instance validatableFieldPhoneFormFields :: ValidatableField PhoneFormFields where
  validateField field value _serverErrors = case field of
    Phone -> validatePhone field value

derive instance eqEmailFormFields :: Eq EmailFormFields

jsComponent :: React.Component Props
jsComponent = component

component :: React.Component Props
component = React.createComponent "Profile"

profile :: Props -> JSX
profile = make component
  { initialState:
      { name: { firstName: Nothing, lastName: Nothing }
      , email: Nothing
      , phone: Nothing
      , address: { zipCode: Nothing, countryCode: Nothing, streetAddress: Nothing, city: Nothing }
      , now: Nothing
      , changeDate: Nothing
      , editFields: Set.empty
      , editName: Ready
      , editEmail: Ready
      , editPhone: Ready
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
  now <- Now.nowDate
  self.setState _ { now = Just now }
  resetFields self EditEmail
  resetFields self EditAddress
  resetFields self EditName
  resetFields self EditPhone

render :: Self -> JSX
render self@{ props: { profile: user } } =
  DOM.div_ $
    [ profileName
    , profileAddress
    , guard (not $ null visiblePendingAddressChanges) $
        DOM.div
          { className: "profile--profile-row"
          , id: "profile--pending-address-change"
          , children:
              [ DescriptionList.descriptionList { definitions: visiblePendingAddressChanges } ]
          }
    , profileEmail
    , profilePhone
    , DOM.div
        { id: "profile--display"
        , children:
            [ DescriptionList.descriptionList
                { definitions:
                    [ { term: "Kundnummer:", description: [ DOM.text $ Cusno.toString user.cusno ] }
                    ]
                }
            ]
        }
    ]
  where
    visiblePendingAddressChanges = showPendingAddressChanges self
    profileEmail =
      AsyncWrapper.asyncWrapper
        { wrapperState: self.state.editEmail
        , readyView: profileEmailReady
        , editingView: \_ -> profileEmailEditing
        , loadingView: profileEmailLoading
        , successView: \_ -> profileEmailReady
        , errorView: editingError self EditEmail
        }
      where
        profileEmailReady = DOM.div
          { className: "profile--profile-row"
          , id: "profile--email"
          , children:
              [ currentEmail
              , changeEmailButton self
              ]
          }
        profileEmailEditing = DOM.div_
          [ DescriptionList.descriptionList
              { definitions:
                  [ { term: "E-postadress:"
                    , description: [ editEmail self ]
                    }
                  ]
              }
          ]
        profileEmailLoading spinner = DOM.div
          { className: "profile--profile-row"
          , children:
              [ currentEmail
              , spinner
              ]
          }
        currentEmail =
          DescriptionList.descriptionList
            { definitions:
                [ { term: "E-postadress:"
                  , description: [ DOM.text user.email ]
                  }
                ]
            }

    profilePhone =
      AsyncWrapper.asyncWrapper
        { wrapperState: self.state.editPhone
        , readyView: profilePhoneReady
        , editingView: \_ -> profilePhoneEditing
        , loadingView: profilePhoneLoading
        , successView: \_ -> profilePhoneReady
        , errorView: editingError self EditPhone
        }
      where
        profilePhoneReady = DOM.div
          { className: "profile--profile-row"
          , id: "profile--phone"
          , children:
              [ currentPhone
              , changePhoneButton self
              ]
          }
        profilePhoneEditing = DOM.div_
          [ DescriptionList.descriptionList
              { definitions:
                  [ { term: "Telefonnummer:"
                    , description: [ editPhone self ]
                    }
                  ]
              }
          ]
        profilePhoneLoading spinner = DOM.div
          { className: "profile--profile-row"
          , children:
              [ currentPhone
              , spinner
              ]
          }
        currentPhone =
          DescriptionList.descriptionList
            { definitions:
                [ { term: "Telefonnummer:"
                  , description: [ DOM.text $ fromMaybe "-" $ Nullable.toMaybe user.phone ]
                  }
                ]
            }

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
          , id: "profile--name"
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
            , id: "profile--address"
            , children:
                [ currentAddress
                  -- Don't allow to edit address if already pending for a change
                , case any (isUpcomingPendingChange self.state.now) pendingChanges of
                    false
                      | isNothing $ toMaybe user.address -> addAddressButton self
                      | otherwise -> changeAddressButton self
                    true -> deletePendingAddressChanges self $ length visiblePendingAddressChanges /= 1
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
                [ { term: "Permanent adress:"
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
               EditEmail   -> self.setState _ { editEmail   = AsyncWrapper.Ready }
               EditPhone   -> self.setState _ { editPhone   = AsyncWrapper.Ready }
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
        { term: "Adressändring:"
        , description: map (DOM.text <<< pendingAddressChangeText) upcomingChanges
        }
    _ -> mempty

editAddress :: Self -> JSX
editAddress self =
  DOM.form
    { className: "profile--edit-address"
    , children:
        [ Grid.row_
            [ Grid.row_ [ DOM.label
                            { className: "input-field--input-label"
                            , children: [ DOM.text "Giltig från" ]
                            }
                        ]
            , Grid.row_
                [ DatePicker.datePicker
                    { onChange: (_ >>= \newDate -> self.setState _ { changeDate = newDate })
                    , className: "profile--edit-address--date-picker"
                    , value: self.state.changeDate
                    , format: "d.M.yyyy"
                    , required: true
                    , minDate: Date.adjust (Days 1.0) =<< self.state.now
                    , maxDate: Nothing
                    , disabled: false
                    , locale: "sv-FI"
                    }
                ]
            ]
        , InputField.inputField
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
            , validationError: inputFieldErrorMessage $ validateField (Zip self.state.address.countryCode) self.state.address.zipCode []
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
        , CountryDropDown.countryDropDown CountryDropDown.limitedCountries false
            (\newCountryCode -> self.setState _ { address { countryCode = newCountryCode } })
            self.state.address.countryCode
        , CountryDropDown.countryChangeMessage
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
      <*> validateField (Zip form.countryCode) form.zipCode []
      <*> validateField City form.city []
      <*> validateField CountryCode form.countryCode []

    submitNewAddress :: ValidatedForm AddressFormFields Address -> Effect Unit
    submitNewAddress = validation
      (\_ -> Console.error "Could not submit address.")
      updateAddress

    updateAddress :: Address -> Effect Unit
    updateAddress { streetAddress: Just streetAddress
                  , zipCode:       Just zipCode
                  , countryCode:   Just countryCode
                  } = do
      self.setState _ { editAddress = Loading mempty }
      Aff.launchAff_ do
        newUser <- User.updateUser self.props.profile.uuid $ User.UpdateAddress { streetAddress, zipCode, countryCode, startDate: self.state.changeDate }
        case newUser of
          Right u -> liftEffect do
            self.props.onUpdate u
            self.setState _ { editAddress = Success Nothing }
            Tracking.changeAddress self.props.profile.cusno "success"
          Left err -> do
            liftEffect do
              self.props.logger.error $ Error.userError $ show err
              self.setState _ { editAddress = AsyncWrapper.Error "Adressändringen misslyckades." }
              Tracking.changeAddress self.props.profile.cusno "error: unexpected error when updating address"
    updateAddress _ = pure unit

editEmail :: Self -> JSX
editEmail self =
  DOM.form
    { className: "profile--edit-email"
    , children:
        [ InputField.inputField
            { type_: InputField.Email
            , name: "email"
            , "placeholder": self.props.profile.email
            , value: self.state.email
            , onChange: \newEmail -> case newEmail of
                Just n -> self.setState _ { email = Just n }
                _ -> pure unit
            , label: Just "E-postadress"
            , validationError: inputFieldErrorMessage $ validateField Email self.state.email []
            }
        , submitButton
        , DOM.div { className: "profile--submit-buttons", children: [ iconClose self EditEmail ] }
        ]
    , onSubmit: Events.handler preventDefault $ \_ -> submitNewEmail $ validateEmailForm self.state.email
    }
  where
    submitButton = iconSubmit $ isValid (validateEmailForm self.state.email)

    validateEmailForm :: Maybe String -> ValidatedForm EmailFormFields (Maybe String)
    validateEmailForm form =
      validateField Email form []

    submitNewEmail :: ValidatedForm EmailFormFields (Maybe String) -> Effect Unit
    submitNewEmail = validation
      (\_ -> Console.error "Could not submit email.")
      updateEmail

    updateEmail :: Maybe String -> Effect Unit
    updateEmail (Just email) = do
      self.setState _ { editEmail = Loading mempty }
      if email == self.props.profile.email then
        self.setState _ { editEmail = AsyncWrapper.Error "E-postadressen är den samma som den gamla." }
        else Aff.launchAff_ do
          newUser <- User.updateUser self.props.profile.uuid $ User.UpdateEmail { email: email }
          case newUser of
            Right u -> liftEffect do
              self.props.onUpdate u
              self.setState _ { editEmail = Success Nothing }
              Tracking.changeEmail self.props.profile.cusno "success"
            Left UniqueViolation -> do
              liftEffect do
                self.setState _ { editEmail = AsyncWrapper.Error "Den här e-postadressen används för ett annat konto. Vänligen ta kontakt med kundservice om du har frågor." }
            Left err -> do
              liftEffect do
                self.props.logger.error $ Error.userError $ show err
                self.setState _ { editEmail = AsyncWrapper.Error "Det gick inte att uppdatera e-postadressen. Vänligen ta kontakt med kundservice." }
                Tracking.changeEmail self.props.profile.cusno "error: unexpected error when updating email"
    updateEmail _ = pure unit

editPhone :: Self -> JSX
editPhone self =
  DOM.form
    { className: "profile--edit-phone"
    , children:
        [ InputField.inputField
            { type_: InputField.Text
            , name: "phone"
            , placeholder: "Telefonnummer"
            , value: self.state.phone
            , onChange: \newPhone -> self.setState _ { phone = newPhone }
            , label: Just "Telefonnummer"
            , validationError: inputFieldErrorMessage $ validateField Phone self.state.phone []
            }
        , submitButton
        , DOM.div { className: "profile--submit-buttons", children: [ iconClose self EditPhone ] }
        ]
    , onSubmit: Events.handler preventDefault $ \_ -> submitNewPhone $ validatePhoneForm self.state.phone
    }
  where
    submitButton = iconSubmit $ isValid (validatePhoneForm self.state.phone)

    validatePhoneForm :: Maybe String -> ValidatedForm PhoneFormFields (Maybe String)
    validatePhoneForm form =
      validateField Phone form []

    submitNewPhone :: ValidatedForm PhoneFormFields (Maybe String) -> Effect Unit
    submitNewPhone = validation
      (\_ -> Console.error "Could not submit phone.")
      updatePhone

    updatePhone :: Maybe String -> Effect Unit
    updatePhone (Just phone) = do
      self.setState _ { editPhone = Loading mempty }
      Aff.launchAff_ do
        newUser <- User.updateUser self.props.profile.uuid $ User.UpdatePhone { phone }
        case newUser of
          Right u -> liftEffect do
            self.props.onUpdate u
            self.setState _ { editEmail = Success Nothing }
            Tracking.changePhone self.props.profile.cusno "success"
          Left err -> liftEffect do
            self.props.logger.error $ Error.userError $ show err
            self.setState _ { editPhone = AsyncWrapper.Error "Det gick inte att updatera telefonnummer. Vänligen tak kontakt med kundservice." }
            Tracking.changePhone self.props.profile.cusno "error: unexpected error when updating phone"
    updatePhone _ = pure unit

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
      submitNewName = validation
        (\_ -> Console.error "Could not submit name.")
        updateName

      updateName :: Name -> Effect Unit
      updateName { firstName: Just fname, lastName: Just lname } = do
        self.setState _ { editName = Loading mempty }
        Aff.launchAff_ do
          newUser <- User.updateUser self.props.profile.uuid $ User.UpdateName { firstName: fname, lastName: lname }
          case newUser of
            Right u -> liftEffect do
              self.props.onUpdate u
              self.setState _ { editName = Success Nothing }
              Tracking.changeName self.props.profile.cusno "success"
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

deletePendingAddressChanges :: Self -> Boolean -> JSX
deletePendingAddressChanges self multiple =
  DOM.div
    { className: "profile--edit-attribute-button"
    , children:
        [ DOM.div
            { className: "profile--delete-pending-address-change-icon circle" }
        , DOM.span
            { className: "profile--edit-text"
            , children:
                [ DOM.u_ [ DOM.text $ if multiple then "Avbryt adressändringar" else "Avbryt adressändringen" ] ]
            }
        ]
    , onClick: handler_ do
       self.setState _ { editAddress = Loading mempty }
       Aff.launchAff_ do
         deleted <- User.updateUser self.props.profile.uuid $ User.DeletePendingAddressChanges
         case deleted of
           Right _ -> liftEffect do
             self.setState _
               { editAddress = Success Nothing }
             self.props.onUpdate $ self.props.profile { pendingAddressChanges = Nullable.null }
             Tracking.deletePendingAddressChanges self.props.profile.cusno "success"
           Left err -> liftEffect do
             self.props.logger.error $ Error.userError $ show err
             self.setState _
               { editAddress = AsyncWrapper.Error "Begäran misslyckades." }
             Tracking.deletePendingAddressChanges self.props.profile.cusno "error: unexpected error when updating address"
    }

changeEmailButton :: Self -> JSX
changeEmailButton self = changeAttributeButton self EditEmail

changeNameButton :: Self -> JSX
changeNameButton self = changeAttributeButton self EditName

changePhoneButton :: Self -> JSX
changePhoneButton self = changeAttributeButton self EditPhone

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
switchEditProgress self EditEmail progress = self.setState _ { editEmail = progress }
switchEditProgress self EditAddress progress = self.setState _ { editAddress = progress }
switchEditProgress self EditPhone progress = self.setState _ { editPhone = progress }

isUpcomingPendingChange :: Maybe Date -> PendingAddressChange -> Boolean
isUpcomingPendingChange Nothing _ = true
isUpcomingPendingChange (Just now) (PendingAddressChange { startDate }) = (date startDate > now)

pendingAddressChangeText :: PendingAddressChange -> String
pendingAddressChangeText (PendingAddressChange { address, startDate }) =
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
    , changeDate = Nothing
    }
resetFields self EditName =
  self.setState _ { name = { firstName: toMaybe self.props.profile.firstName
                           , lastName:  toMaybe self.props.profile.lastName
                           }
                  }
resetFields self EditEmail =
  self.setState _ { email = Just self.props.profile.email }

resetFields self EditPhone =
  self.setState _ { phone = Nullable.toMaybe self.props.profile.phone }

formatAddress :: User.DeliveryAddress -> String
formatAddress { temporaryName, streetAddress, zipcode, city } =
  (maybe "" (_ <> ", ") temporaryName) <>
  intercalate ", " [ fromMaybe "-" streetAddress, zipcode, fromMaybe "-" city ]

formatDateString :: DateTime -> String
formatDateString = formatDateDots <<< date
