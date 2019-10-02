module KSF.Profile.Component where

import Prelude

import Control.Monad.Error.Class (throwError)
import Data.Array (catMaybes, filter, intercalate, mapMaybe, null, (:))
import Data.Array as Array
import Data.DateTime (DateTime)
import Data.Either (Either(..))
import Data.Formatter.DateTime (FormatterCommand(..), format)
import Data.JSDate (JSDate, toDateTime)
import Data.List (fromFoldable)
import Data.Maybe (Maybe(..), fromMaybe, isNothing, maybe)
import Data.Nullable (Nullable, toMaybe)
import Data.Nullable as Nullable
import Data.Set (Set)
import Data.Set as Set
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Class.Console as Console
import Effect.Exception (error)
import Effect.Now as Now
import KSF.AsyncWrapper (Progress(..))
import KSF.AsyncWrapper as AsyncWrapper
import KSF.DescriptionList.Component as DescriptionList
import KSF.Editable.Component (editable, ChangeType(..))
import KSF.FormInputField as FormField
import KSF.InputField.Component as Input
import KSF.User (User)
import KSF.User as User
import Persona as Persona
import React.Basic (make, JSX)
import React.Basic as React
import React.Basic.DOM as DOM
import React.Basic.DOM.Events (capture, capture_, preventDefault)
import React.Basic.Events as Events
import Web.DOM.Element (id)

type Self = React.Self Props State

type Props =
  { profile :: User
  , onUpdate :: User -> Effect Unit
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

addressArray :: Persona.Address -> Array String
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
        , successView: DOM.div_ []
        , errorView: \e -> DOM.text e
        }
      where
        profileNameReady = DOM.div
          { className: "profile--profile-row"
          , children:
              [ DescriptionList.descriptionList
                  { definitions:
                      [ { term: "Namn:"
                        , description: map DOM.text $ mapMaybe toMaybe [ user.firstName, user.lastName ]
                        }
                      ]
                  }
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

    profileAddress =
      AsyncWrapper.asyncWrapper
        { wrapperState: self.state.editAddress
        , readyView: profileAddressReady
        , editingView: \_ -> profileAddressEditing
        , successView: DOM.div_ []
        , errorView: \e -> DOM.text e
        }
      where
        profileAddressReady =
          DOM.div
            { className: "profile--profile-row"
            , children:
                [ DescriptionList.descriptionList
                    { definitions:
                        [ { term: "Adress:"
                          , description: map DOM.text $ fromMaybe [] $ addressArray <$> toMaybe user.address
                          }
                        ]
                    }
                , if isNothing $ toMaybe user.address
                  then addAddressButton self
                  else changeAddressButton self
                ]
            }
        profileAddressEditing = DOM.div_
          [ DescriptionList.descriptionList
              { definitions:
                  [ { term: "Adress:"
                    , description: [ editAddress self ]
                    }
                  ]
              }
          ]
   -- <> editAddress self
  -- where
  --   -- | I'm sorry
  --   fixNullable :: Nullable String -> String
  --   fixNullable a = fromMaybe "" $ Nullable.toMaybe a

  --   address = fromMaybe [] $ addressArray <$> Nullable.toMaybe user.address

  --   saveName :: (String -> Effect Unit) -> Array String -> Aff Unit
  --   saveName onError [firstName, lastName] = do
  --     newUser <- User.updateUser user.uuid $ User.UpdateName { firstName, lastName }
  --     case newUser of
  --       Right u -> liftEffect $ self.props.onUpdate u
  --       Left err -> do
  --         Console.error "Unexpected error when updating name."
  --         liftEffect $ onError "Namnändringen misslyckades."
  --         throwError $ error "Unexpected error when updating name."
  --   saveName onError args =
  --     Console.error $ "saveName: unexpected number of arguments: " <> show args

  --   saveAddress onError [ streetAddress, zipCode, _city ] = do
  --     let body = Persona.UpdateAddress { streetAddress, zipCode, countryCode }
  --         -- TODO: There should be a country select list in the UI
  --         -- Use country code found in current address until then.
  --         countryCode = fromMaybe "FI" $ (map _.countryCode <<< toMaybe) user.address
  --     newUser <- User.updateUser user.uuid body
  --     case newUser of
  --       Right u -> liftEffect $ self.props.onUpdate u
  --       Left err -> do
  --         Console.error "Unexpected error when updating address."
  --         _ <- liftEffect $ onError "Adressändringen misslyckades."
  --         throwError $ error "Unexpected error when updating address."
  --   saveAddress onError args =
  --     Console.error $ "saveName: Unexpected number of arguments: " <> show args

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
        [ FormField.formInputField
            { type_: "text"
            , name: "streetAddress"
            , placeholder: "Gatuadress"
            , value: self.state.address.streetAddress
            , onChange: \newStreetAddr -> self.setState _ { address { streetAddress = newStreetAddr } }
            , label: "Gatuadress"
            }
        , FormField.formInputField
            { type_: "text"
            , name: "zipCode"
            , placeholder: "Postnummer"
            , value: self.state.address.zipCode
            , onChange: \newZip -> self.setState _ { address { zipCode = newZip } }
            , label: "Postnummer"
            }
        , FormField.formInputField
            { type_: "text"
            , name: "city"
            , placeholder: "Stad"
            , value: self.state.address.city
            , onChange: \newCity -> self.setState _ { address { city = newCity } }
            , label: "Stad"
            }
        , DOM.div { className: "profile--submit-buttons", children: [ iconSubmit, iconClose self EditAddress ] }
        ]
    , onSubmit: Events.handler preventDefault $ \_ -> Console.log "SUBMIT"
    }

editName :: Self -> JSX
editName self =
    DOM.form
    { className: "profile--edit-name"
    , children:
        [ FormField.formInputField
            { type_: "text"
            , name: "firstName"
            , placeholder: "Förnamn"
            , value: self.state.name.firstName
            , onChange: \newFirstName -> self.setState _ { name { firstName = newFirstName } }
            , label: "Förnamn"
            }
        , FormField.formInputField
            { type_: "text"
            , name: "lastName"
            , placeholder: "Efternamn"
            , value: self.state.name.lastName
            , onChange: \newLastName -> self.setState _ { name { lastName = newLastName } }
            , label: "Efternamn"
            }
        , DOM.div { className: "profile--submit-buttons", children: [ iconSubmit, iconClose self EditName ] }
        ]
    , onSubmit: Events.handler preventDefault $ \_ -> Console.log "SUBMIT"
    }

iconSubmit :: JSX
iconSubmit = DOM.div
  { className: "editable--submit"
  , children:
      [ DOM.button
          { type: "submit"
          , children: [ DOM.text "Spara" ]
          , className: "button-green"
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

isUpcomingPendingChange :: Maybe DateTime -> Persona.PendingAddressChange -> Boolean
isUpcomingPendingChange Nothing _ = true
isUpcomingPendingChange (Just now) { startDate } =
  maybe true (_ > now) $ toDateTime startDate

pendingAddressChangeText :: Persona.PendingAddressChange -> String
pendingAddressChangeText { address, startDate, endDate } =
  let addressString = formatAddress address
      pendingPeriod = formatDateString startDate
  in addressString <> " (fr.o.m. " <> pendingPeriod <> ")"

resetFields :: Self -> EditField -> Effect Unit
resetFields self EditAddress =
  self.setState _
    { address =
        { streetAddress: _.streetAddress <$> toMaybe self.props.profile.address
        , countryCode: _.countryCode <$> toMaybe self.props.profile.address
        , zipCode: toMaybe <<< _.zipCode =<< toMaybe self.props.profile.address
        , city: toMaybe <<< _.city =<< toMaybe self.props.profile.address
        }
    }
resetFields self EditName =
  self.setState _ { name = { firstName: toMaybe self.props.profile.firstName
                           , lastName:  toMaybe self.props.profile.lastName
                           }
                  }

formatAddress :: Persona.DeliveryAddress -> String
formatAddress { streetAddress, zipcode, city } = intercalate ", " [ fromMaybe "-" $ toMaybe streetAddress, zipcode, fromMaybe "-" $ toMaybe city ]

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
