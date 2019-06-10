module KSF.Profile.Component where

import Prelude

import Control.Monad.Error.Class (catchError, throwError)
import Data.Array (catMaybes, filter, intercalate, null, (:))
import Data.Array as Array
import Data.DateTime (DateTime)
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
import Effect.Now as Now
import KSF.DescriptionList.Component (Description(..))
import KSF.DescriptionList.Component as DescriptionList
import KSF.Editable.Component (editable, ChangeType(..))
import KSF.Login.Component as Login
import Persona as Persona
import React.Basic (make)
import React.Basic as React
import React.Basic.Extended (JSX, Style, requireStyle)

foreign import profileStyles :: Style

type Self = React.Self Props State

type Props =
  { profile :: Persona.User
  , onUpdate :: Persona.User -> Effect Unit
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
  requireStyle
    profileStyles
    $ React.element
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
    saveName onError arr = do
      loginResponse <- liftEffect $ Login.loadToken
      case loginResponse, arr of
        Just { token }, [firstName, lastName] -> do
          let body = Persona.UpdateName { firstName, lastName }
          newUser <- Persona.updateUser user.uuid token body `catchError` \err -> do
            Console.error "Unexpected error when updating name."
            liftEffect $ onError "Något gick fel."
            throwError err
          liftEffect $ self.props.onUpdate newUser
        -- TODO: this should also show an error message
        _, _ -> Console.error "Did not find token in local storage."

    saveAddress onError arr = do
      loginResponse <- liftEffect $ Login.loadToken
      case loginResponse, arr of
        Just { token }, [ streetAddress, zipCode, _city ] -> do
          let body = Persona.UpdateAddress { streetAddress, zipCode, countryCode }
              -- TODO: There should be a country select list in the UI
              countryCode = "FI"
          newUser <- Persona.updateUser user.uuid token body `catchError` \err -> do
            Console.error "Unexpected error when updating address."
            liftEffect $ onError "Adressändringen misslyckades."
            throwError err
          liftEffect $ self.props.onUpdate newUser
        -- TODO: this should also show an error message
        _, _ -> Console.error "Did not find token in local storage."

    showPendingAddressChanges =
      case toMaybe user.pendingAddressChanges of
        Just pendingChanges
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
formatAddress { streetAddress, zipcode, city } = intercalate ", " [ streetAddress, zipcode, fromMaybe "-" $ toMaybe city ]

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
