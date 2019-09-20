module KSF.Profile.Component where

import Prelude

import Control.Monad.Error.Class (throwError)
import Data.Array (catMaybes, filter, intercalate, null, (:))
import Data.Array as Array
import Data.DateTime (DateTime)
import Data.Either (Either(..))
import Data.Formatter.DateTime (FormatterCommand(..), format)
import Data.JSDate (JSDate, toDateTime)
import Data.List (fromFoldable)
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Nullable (Nullable, toMaybe)
import Data.Nullable as Nullable
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Class.Console as Console
import Effect.Exception (error)
import Effect.Now as Now
import KSF.DescriptionList.Component (Description(..))
import KSF.DescriptionList.Component as DescriptionList
import KSF.Editable.Component (editable, ChangeType(..))
import KSF.User (User)
import KSF.User as User
import Persona as Persona
import React.Basic (make, JSX)
import React.Basic as React

type Self = React.Self Props State

type Props =
  { profile :: User
  , onUpdate :: User -> Effect Unit
  }

type State =
  { name :: Maybe Name
  , address :: Maybe Address
  , now :: Maybe DateTime
  }

type Name =
  { firstName :: String
  , lastName :: String
  }

type Address =
  { zipCode :: String
  , countryCode :: String
  , streetAddress :: String
  }

jsComponent :: React.Component Props
jsComponent = component

component :: React.Component Props
component = React.createComponent "Profile"

profile :: Props -> JSX
profile = make component
  { initialState:
      { name: Nothing
      , address: Nothing
      , now: Nothing
      }
  , render
  , didMount
  }

addressArray :: Persona.Address -> Array String
addressArray { streetAddress, zipCode, city } = do
  let takeJust = catMaybes <<< map Nullable.toMaybe
  streetAddress : takeJust [ zipCode, city ]

didMount :: Self -> Effect Unit
didMount self = do
  now <- Now.nowDateTime
  self.setState _ { now = Just now }

render :: Self -> JSX
render self@{ props: { profile: user } } =
  React.element
    DescriptionList.component
      { definitions:
          [ { term: "Namn:"
            , description: Editable $ editable
                { values: [ fixNullable user.firstName, fixNullable user.lastName ]
                , onSave: saveName
                , changeType: ImmediateChange
                }
            }
          , { term: "Adress:"
            , description: Editable $ editable
                { values: address
                , onSave: saveAddress
                , changeType: PendingChange
                }
            }
          ]
          <> showPendingAddressChanges <>
          [ { term: "E-postadress:", description: Static [ user.email ] }
          , { term: "Kundnummer:", description: Static [ user.cusno ] }
          ]
      }
  where
    -- | I'm sorry
    fixNullable :: Nullable String -> String
    fixNullable a = fromMaybe "" $ Nullable.toMaybe a

    address = fromMaybe [] $ addressArray <$> Nullable.toMaybe user.address

    saveName :: (String -> Effect Unit) -> Array String -> Aff Unit
    saveName onError [firstName, lastName] = do
      newUser <- User.updateUser user.uuid $ User.UpdateName { firstName, lastName }
      case newUser of
        Right u -> liftEffect $ self.props.onUpdate u
        Left err -> do
          Console.error "Unexpected error when updating name."
          liftEffect $ onError "Namnändringen misslyckades."
          throwError $ error "Unexpected error when updating name."
    saveName onError args =
      Console.error $ "saveName: unexpected number of arguments: " <> show args

    saveAddress onError [ streetAddress, zipCode, _city ] = do
      let body = Persona.UpdateAddress { streetAddress, zipCode, countryCode }
          -- TODO: There should be a country select list in the UI
          -- Use country code found in current address until then.
          countryCode = fromMaybe "FI" $ (map _.countryCode <<< toMaybe) user.address
      newUser <- User.updateUser user.uuid body
      case newUser of
        Right u -> liftEffect $ self.props.onUpdate u
        Left err -> do
          Console.error "Unexpected error when updating address."
          liftEffect $ onError "Adressändringen misslyckades."
          throwError $ error "Unexpected error when updating address."
    saveAddress onError args =
      Console.error $ "saveName: Unexpected number of arguments: " <> show args

    showPendingAddressChanges =
      case toMaybe user.pendingAddressChanges of
        Just pendingChanges
          -- In the pendingChanges array, we might have changes that have already happened.
          -- These should not be shown to the user.
          | upcomingChanges <- filter (isUpcomingPendingChange self.state.now) pendingChanges
          , not $ null upcomingChanges -> Array.singleton
            { term: "Addressändrig:"
            , description: Static $ map pendingAddressChangeText upcomingChanges
            }
        _ -> mempty

isUpcomingPendingChange :: Maybe DateTime -> Persona.PendingAddressChange -> Boolean
isUpcomingPendingChange Nothing _ = true
isUpcomingPendingChange (Just now) { startDate } =
  maybe true (_ > now) $ toDateTime startDate

pendingAddressChangeText :: Persona.PendingAddressChange -> String
pendingAddressChangeText { address, startDate, endDate } =
  let addressString = formatAddress address
      pendingPeriod = formatDateString startDate
  in addressString <> " (fr.o.m. " <> pendingPeriod <> ")"

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
