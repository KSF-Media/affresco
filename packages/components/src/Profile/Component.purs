  module KSF.Profile.Component where

import Prelude

import Control.Alt ((<|>))
import Control.Monad.Error.Class (throwError)
import Data.Array (any, catMaybes, filter, intercalate, length, mapMaybe, null, (:))
import Data.Array as Array
import Data.Date as Date
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
import KSF.DescriptionList as DescriptionList
import KSF.Grid as Grid
import KSF.Helpers (formatDateDots)
import KSF.InputField as InputField
import KSF.JSError as Error
import KSF.Sentry as Sentry
import KSF.User (User, UserError(UniqueViolation))
import KSF.User as User
import KSF.User.Cusno as Cusno
import KSF.ValidatableForm (class ValidatableField, ValidatedForm, inputFieldErrorMessage, validateEmailAddress, validateEmptyField, validateField, validatePhone, validateFinnishZipCode, validateZipCode)
import React.Basic (JSX)
import React.Basic.DOM as DOM
import React.Basic.DOM.Events (capture_, preventDefault)
import React.Basic.Hooks (Component, useState, (/\))
import React.Basic.Hooks as React
import React.Basic.Events (handler_)
import React.Basic.Events as Events
import KSF.Tracking as Tracking

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
  , changeDate :: Maybe Date
  , editFields :: Set EditField
  , editName :: AsyncWrapper.Progress JSX
  , editEmail :: AsyncWrapper.Progress JSX
  , editPhone :: AsyncWrapper.Progress JSX
  , editAddress :: AsyncWrapper.Progress JSX
  }

type SetState = (State -> State) -> Effect Unit

type ResetField = EditField -> Effect Unit

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

component :: Component Props
component = do
  now <- Now.nowDate
  React.component "Profile" \props -> React.do
    let propAddress = toMaybe props.profile.address
        initialAddress :: Address
        initialAddress =
          { zipCode: toMaybe <<< _.zipCode =<< propAddress
          , countryCode: (_.countryCode <$> propAddress) <|> Just "FI"
          , streetAddress: _.streetAddress <$> propAddress
          , city: toMaybe <<< _.city =<< propAddress
          }
    state /\ setState <- useState $
      { name: { firstName: toMaybe props.profile.firstName
              , lastName: toMaybe props.profile.lastName }
      , email: Just props.profile.email
      , phone: toMaybe props.profile.phone
      , address: initialAddress
      , changeDate: Nothing
      , editFields: Set.empty
      , editName: Ready
      , editEmail: Ready
      , editPhone: Ready
      , editAddress: Ready
      }
    pure $ render props now (resetFields props initialAddress setState) state setState

addressArray :: User.Address -> Array String
addressArray { streetAddress, zipCode, city } =
  let takeJust = catMaybes <<< map Nullable.toMaybe
  in streetAddress : takeJust [ zipCode, city ]

render :: Props -> Date -> ResetField -> State -> SetState -> JSX
render props@{ profile: user } now resetField state setState =
  DOM.div_ $
    [ profileName
    , profileAddress
    , guard (not $ null visiblePendingAddressChanges) $
        DOM.div
          { className: "profile--profile-row"
          , id: "profile--pending-address-change"
          , children:
              [ DescriptionList.render { definitions: visiblePendingAddressChanges } ]
          }
    , profileEmail
    , profilePhone
    , DOM.div
        { id: "profile--display"
        , children:
            [ DescriptionList.render
                { definitions:
                    [ { term: "Kundnummer:", description: [ DOM.text $ Cusno.toString user.cusno ] }
                    ]
                }
            ]
        }
    ]
  where
    visiblePendingAddressChanges = showPendingAddressChanges props now
    profileEmail =
      AsyncWrapper.asyncWrapper
        { wrapperState: state.editEmail
        , readyView: profileEmailReady
        , editingView: \_ -> profileEmailEditing
        , loadingView: profileEmailLoading
        , successView: \_ -> profileEmailReady
        , errorView: editingError setState EditEmail
        }
      where
        profileEmailReady = DOM.div
          { className: "profile--profile-row"
          , id: "profile--email"
          , children:
              [ currentEmail
              , changeAttributeButton EditEmail setState
              ]
          }
        profileEmailEditing = DOM.div_
          [ DescriptionList.render
              { definitions:
                  [ { term: "E-postadress:"
                    , description: [ editEmail props state setState resetField ]
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
          DescriptionList.render
            { definitions:
                [ { term: "E-postadress:"
                  , description: [ DOM.text user.email ]
                  }
                ]
            }

    profilePhone =
      AsyncWrapper.asyncWrapper
        { wrapperState: state.editPhone
        , readyView: profilePhoneReady
        , editingView: \_ -> profilePhoneEditing
        , loadingView: profilePhoneLoading
        , successView: \_ -> profilePhoneReady
        , errorView: editingError setState EditPhone
        }
      where
        profilePhoneReady = DOM.div
          { className: "profile--profile-row"
          , id: "profile--phone"
          , children:
              [ currentPhone
              , changeAttributeButton EditPhone setState
              ]
          }
        profilePhoneEditing = DOM.div_
          [ DescriptionList.render
              { definitions:
                  [ { term: "Telefonnummer:"
                    , description: [ editPhone props state setState resetField ]
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
          DescriptionList.render
            { definitions:
                [ { term: "Telefonnummer:"
                  , description: [ DOM.text $ fromMaybe "-" $ Nullable.toMaybe user.phone ]
                  }
                ]
            }

    profileName =
      AsyncWrapper.asyncWrapper
        { wrapperState: state.editName
        , readyView: profileNameReady
        , editingView: \_ -> profileNameEditing
        , loadingView: profileNameLoading
        , successView: \_ -> profileNameReady
        , errorView: editingError setState EditName
        }
      where
        profileNameReady = DOM.div
          { className: "profile--profile-row"
          , id: "profile--name"
          , children:
              [ currentName
              , changeAttributeButton EditName setState
              ]
          }
        profileNameEditing = DOM.div_
          [ DescriptionList.render
              { definitions:
                  [ { term: "Namn:"
                    , description: [ editName props state setState resetField ]
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
          DescriptionList.render
            { definitions:
                [ { term: "Namn:"
                  , description: map DOM.text $ mapMaybe toMaybe [ user.firstName, user.lastName ]
                  }
                ]
            }

    profileAddress =
      AsyncWrapper.asyncWrapper
        { wrapperState: state.editAddress
        , readyView: profileAddressReady
        , editingView: \_ -> profileAddressEditing
        , loadingView: profileAddressLoading
        , successView: \_ -> profileAddressReady
        , errorView: editingError setState EditAddress
        }
      where
        profileAddressReady =
          DOM.div
            { className: "profile--profile-row"
            , id: "profile--address"
            , children:
                [ currentAddress
                  -- Don't allow to edit address if already pending for a change
                , case any (isUpcomingPendingChange now) pendingChanges of
                    false
                      | isNothing $ toMaybe user.address -> addAddressButton setState
                      | otherwise -> changeAttributeButton EditAddress setState
                    true -> deletePendingAddressChanges props setState $ length visiblePendingAddressChanges /= 1
                ]
            }
          where
            pendingChanges = fromMaybe [] $ toMaybe user.pendingAddressChanges

        profileAddressEditing = DOM.div_
          [ DescriptionList.render
              { definitions:
                  [ { term: "Permanent adress:"
                    , description: [ editAddress props state setState resetField now ]
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
          DescriptionList.render
            { definitions:
                [ { term: "Permanent adress:"
                  , description: map DOM.text $ fromMaybe [] $ addressArray <$> toMaybe user.address
                  }
                ]
            }

editingError :: SetState -> EditField -> String -> JSX
editingError setState fieldName errMessage =
   DOM.div
     { className: "profile--edit-error"
     , children:
         [ DOM.text $ errMessage <> " "
         , DOM.span
             { className: "profile--edit-try-again"
             , children: [ DOM.text "Försök igen" ]
             , onClick: handler_ $ case fieldName of
               EditAddress -> setState _ { editAddress = AsyncWrapper.Ready }
               EditName    -> setState _ { editName    = AsyncWrapper.Ready }
               EditEmail   -> setState _ { editEmail   = AsyncWrapper.Ready }
               EditPhone   -> setState _ { editPhone   = AsyncWrapper.Ready }
             }
         ]
     }

showPendingAddressChanges :: Props -> Date -> Array DescriptionList.Definition
showPendingAddressChanges props now =
  case toMaybe props.profile.pendingAddressChanges of
    Just pendingChanges
      -- In the pendingChanges array, we might have changes that have already happened.
      -- These should not be shown to the user.
      | upcomingChanges <- filter (isUpcomingPendingChange now) pendingChanges
      , not $ null upcomingChanges -> Array.singleton
        { term: "Adressändring:"
        , description: map (DOM.text <<< pendingAddressChangeText) upcomingChanges
        }
    _ -> mempty

editAddress :: Props -> State -> SetState -> ResetField -> Date -> JSX
editAddress props state setState resetField now =
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
                    { onChange: (_ >>= \newDate -> setState _ { changeDate = newDate })
                    , className: "profile--edit-address--date-picker"
                    , value: state.changeDate
                    , format: "d.M.yyyy"
                    , required: true
                    , minDate: Date.adjust (Days 1.0) now
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
            , value: state.address.streetAddress
            , onChange: \newStreetAddr -> setState _ { address { streetAddress = newStreetAddr } }
            , label: Just "Gatuadress"
            , validationError: inputFieldErrorMessage $ validateField StreetAddress state.address.streetAddress []
            }
        , InputField.inputField
            { type_: InputField.Text
            , name: "zipCode"
            , placeholder: "Postnummer"
            , value: state.address.zipCode
            , onChange: \newZip -> setState _ { address { zipCode = newZip } }
            , label: Just "Postnummer"
            , validationError: inputFieldErrorMessage $ validateField (Zip state.address.countryCode) state.address.zipCode []
            }
        , InputField.inputField
            { type_: InputField.Text
            , name: "city"
            , placeholder: "Stad"
            , value: state.address.city
            , onChange: \newCity -> setState _ { address { city = newCity } }
            , label: Just "Stad"
            , validationError: inputFieldErrorMessage $ validateField City state.address.city []
            }
        , CountryDropDown.countryDropDown CountryDropDown.limitedCountries false
            (\newCountryCode -> setState _ { address { countryCode = newCountryCode } })
            state.address.countryCode
        , CountryDropDown.countryChangeMessage
        , submitButton
        , DOM.div { className: "profile--submit-buttons", children: [ iconClose setState resetField EditAddress ] }
        ]
    , onSubmit: Events.handler preventDefault $ \_ -> submitNewAddress $ validateAddressForm state.address
    }
  where
    submitButton = iconSubmit $ isValid (validateAddressForm state.address)

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
      setState _ { editAddress = Loading mempty }
      Aff.launchAff_ do
        newUser <- User.updateUser props.profile.uuid $ User.UpdateAddress { streetAddress, zipCode, countryCode, startDate: state.changeDate }
        case newUser of
          Right u -> liftEffect do
            props.onUpdate u
            setState _ { editAddress = Success Nothing }
            Tracking.changeAddress props.profile.cusno "success"
          Left err -> do
            liftEffect do
              props.logger.error $ Error.userError $ show err
              setState _ { editAddress = AsyncWrapper.Error "Adressändringen misslyckades." }
              Tracking.changeAddress props.profile.cusno "error: unexpected error when updating address"
    updateAddress _ = pure unit

editEmail :: Props -> State -> SetState -> ResetField -> JSX
editEmail props state setState resetField =
  DOM.form
    { className: "profile--edit-email"
    , children:
        [ InputField.inputField
            { type_: InputField.Email
            , name: "email"
            , "placeholder": props.profile.email
            , value: state.email
            , onChange: \newEmail -> case newEmail of
                Just n -> setState _ { email = Just n }
                _ -> pure unit
            , label: Just "E-postadress"
            , validationError: inputFieldErrorMessage $ validateField Email state.email []
            }
        , submitButton
        , DOM.div { className: "profile--submit-buttons", children: [ iconClose setState resetField EditEmail ] }
        ]
    , onSubmit: Events.handler preventDefault $ \_ -> submitNewEmail $ validateEmailForm state.email
    }
  where
    submitButton = iconSubmit $ isValid (validateEmailForm state.email)

    validateEmailForm :: Maybe String -> ValidatedForm EmailFormFields (Maybe String)
    validateEmailForm form =
      validateField Email form []

    submitNewEmail :: ValidatedForm EmailFormFields (Maybe String) -> Effect Unit
    submitNewEmail = validation
      (\_ -> Console.error "Could not submit email.")
      updateEmail

    updateEmail :: Maybe String -> Effect Unit
    updateEmail (Just email) = do
      setState _ { editEmail = Loading mempty }
      if email == props.profile.email then
        setState _ { editEmail = AsyncWrapper.Error "E-postadressen är den samma som den gamla." }
        else Aff.launchAff_ do
          newUser <- User.updateUser props.profile.uuid $ User.UpdateEmail { email: email }
          case newUser of
            Right u -> liftEffect do
              props.onUpdate u
              setState _ { editEmail = Success Nothing }
              Tracking.changeEmail props.profile.cusno "success"
            Left UniqueViolation -> do
              liftEffect do
                setState _ { editEmail = AsyncWrapper.Error "Den här e-postadressen används för ett annat konto. Vänligen ta kontakt med kundservice om du har frågor." }
            Left err -> do
              liftEffect do
                props.logger.error $ Error.userError $ show err
                setState _ { editEmail = AsyncWrapper.Error "Det gick inte att uppdatera e-postadressen. Vänligen ta kontakt med kundservice." }
                Tracking.changeEmail props.profile.cusno "error: unexpected error when updating email"
    updateEmail _ = pure unit

editPhone :: Props -> State -> SetState -> ResetField -> JSX
editPhone props state setState resetField =
  DOM.form
    { className: "profile--edit-phone"
    , children:
        [ InputField.inputField
            { type_: InputField.Text
            , name: "phone"
            , placeholder: "Telefonnummer"
            , value: state.phone
            , onChange: \newPhone -> setState _ { phone = newPhone }
            , label: Just "Telefonnummer"
            , validationError: inputFieldErrorMessage $ validateField Phone state.phone []
            }
        , submitButton
        , DOM.div { className: "profile--submit-buttons", children: [ iconClose setState resetField EditPhone ] }
        ]
    , onSubmit: Events.handler preventDefault $ \_ -> submitNewPhone $ validatePhoneForm state.phone
    }
  where
    submitButton = iconSubmit $ isValid (validatePhoneForm state.phone)

    validatePhoneForm :: Maybe String -> ValidatedForm PhoneFormFields (Maybe String)
    validatePhoneForm form =
      validateField Phone form []

    submitNewPhone :: ValidatedForm PhoneFormFields (Maybe String) -> Effect Unit
    submitNewPhone = validation
      (\_ -> Console.error "Could not submit phone.")
      updatePhone

    updatePhone :: Maybe String -> Effect Unit
    updatePhone (Just phone) = do
      setState _ { editPhone = Loading mempty }
      Aff.launchAff_ do
        newUser <- User.updateUser props.profile.uuid $ User.UpdatePhone { phone }
        case newUser of
          Right u -> liftEffect do
            props.onUpdate u
            setState _ { editPhone = Success Nothing }
            Tracking.changePhone props.profile.cusno "success"
          Left err -> liftEffect do
            props.logger.error $ Error.userError $ show err
            setState _ { editPhone = AsyncWrapper.Error "Det gick inte att updatera telefonnummer. Vänligen tak kontakt med kundservice." }
            Tracking.changePhone props.profile.cusno "error: unexpected error when updating phone"
    updatePhone _ = pure unit

editName :: Props -> State -> SetState -> ResetField -> JSX
editName props state setState resetField =
  DOM.form
    { className: "profile--edit-name"
    , children:
        [ InputField.inputField
            { type_: InputField.Text
            , name: "firstName"
            , placeholder: "Förnamn"
            , value: state.name.firstName
            , onChange: \newFirstName -> setState _ { name { firstName = newFirstName } }
            , label: Just "Förnamn"
            , validationError: inputFieldErrorMessage $ validateField FirstName state.name.firstName []
            }
        , InputField.inputField
            { type_: InputField.Text
            , name: "lastName"
            , placeholder: "Efternamn"
            , value: state.name.lastName
            , onChange: \newLastName -> setState _ { name { lastName = newLastName } }
            , label: Just "Efternamn"
            , validationError: inputFieldErrorMessage $ validateField LastName state.name.lastName []
            }
        , submitButton
        , DOM.div { className: "profile--submit-buttons", children: [ iconClose setState resetField EditName ] }
        ]
    , onSubmit: Events.handler preventDefault $ \_ -> submitNewName $ validateNameForm state.name
    }
    where
      submitButton = iconSubmit $ isValid (validateNameForm state.name)

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
        setState _ { editName = Loading mempty }
        Aff.launchAff_ do
          newUser <- User.updateUser props.profile.uuid $ User.UpdateName { firstName: fname, lastName: lname }
          case newUser of
            Right u -> liftEffect do
              props.onUpdate u
              setState _ { editName = Success Nothing }
              Tracking.changeName props.profile.cusno "success"
            Left err -> do
              liftEffect do
                props.logger.error $ Error.userError $ show err
                setState _ { editName = AsyncWrapper.Error "Namnändringen misslyckades." }
                Tracking.changeName props.profile.cusno "error: unexpected error when updating name"
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

iconClose :: SetState -> ResetField -> EditField -> JSX
iconClose setState resetField field = DOM.div
  { className: "close-icon"
  , onClick: capture_ do
      resetField field
      setState \s -> s { editFields = Set.delete field s.editFields }
      switchEditProgress setState field Ready
  }

changeAttributeButton :: EditField -> SetState -> JSX
changeAttributeButton = flip (editButton "Ändra")

addAddressButton :: SetState -> JSX
addAddressButton setState = editButton "Lägg till adress" setState EditAddress

changeAddressButton :: SetState -> JSX
changeAddressButton = changeAttributeButton EditAddress

deletePendingAddressChanges :: Props -> SetState -> Boolean -> JSX
deletePendingAddressChanges props setState multiple =
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
       setState _ { editAddress = Loading mempty }
       Aff.launchAff_ do
         deleted <- User.updateUser props.profile.uuid $ User.DeletePendingAddressChanges
         case deleted of
           Right _ -> liftEffect do
             setState _
               { editAddress = Success Nothing }
             props.onUpdate $ props.profile { pendingAddressChanges = Nullable.null }
             Tracking.deletePendingAddressChanges props.profile.cusno "success"
           Left err -> liftEffect do
             props.logger.error $ Error.userError $ show err
             setState _
               { editAddress = AsyncWrapper.Error "Begäran misslyckades." }
             Tracking.deletePendingAddressChanges props.profile.cusno "error: unexpected error when updating address"
    }

editButton :: String -> SetState -> EditField -> JSX
editButton buttonText setState field =
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
      setState \s -> s { editFields = Set.insert field s.editFields }
      switchEditProgress setState field (Editing mempty)

switchEditProgress :: SetState -> EditField -> AsyncWrapper.Progress JSX -> Effect Unit
switchEditProgress setState EditName progress = setState _ { editName = progress }
switchEditProgress setState EditEmail progress = setState _ { editEmail = progress }
switchEditProgress setState EditAddress progress = setState _ { editAddress = progress }
switchEditProgress setState EditPhone progress = setState _ { editPhone = progress }

isUpcomingPendingChange :: Date -> User.PendingAddressChange -> Boolean
isUpcomingPendingChange now { startDate } =
  maybe true (_ > now) $ toDate startDate

pendingAddressChangeText :: User.PendingAddressChange -> String
pendingAddressChangeText { address, startDate } =
  let addressString = formatAddress address
      pendingPeriod = formatDateString startDate
  in addressString <> " (fr.o.m. " <> pendingPeriod <> ")"

resetFields :: Props -> Address -> SetState -> EditField -> Effect Unit
resetFields _ initialAddress setState EditAddress =
  setState _
    { address = initialAddress
    , changeDate = Nothing
    }
resetFields props _ setState EditName =
  setState _ { name = { firstName: toMaybe props.profile.firstName
                      , lastName:  toMaybe props.profile.lastName
                      }
             }
resetFields props _ setState EditEmail =
  setState _ { email = Just props.profile.email }

resetFields props _ setState EditPhone =
  setState _ { phone = Nullable.toMaybe props.profile.phone }

formatAddress :: User.DeliveryAddress -> String
formatAddress { temporaryName, streetAddress, zipcode, city } =
  (maybe "" (_ <> ", ") $ toMaybe temporaryName) <>
  intercalate ", " [ fromMaybe "-" $ toMaybe streetAddress, zipcode, fromMaybe "-" $ toMaybe city ]

formatDateString :: JSDate -> String
formatDateString startDate
  | Just startString <- formatDateDots <$> toDate startDate = startString
  | otherwise = mempty
