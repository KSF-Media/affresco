module KSF.User.Address where

import Prelude

import Control.Alternative ((<|>))
import Data.Array (filter, fromFoldable, head, sortWith)
import Data.Date (Date)
import Data.JSDate (toDate)
import Data.Nullable as Nullable
import Data.Maybe (Maybe, fromMaybe, maybe)
import KSF.User (User)

-- The API types all use Nullable.  Since we're munching through them
-- anyway let's have our own with Maybe.
type Address =
  { zipcode :: String
  , city :: Maybe String
  , streetAddress :: String
  , countryCode :: String
  }

-- Gives either customer's address or the one from pending address
-- change.
effectiveAddress :: User -> Date -> Maybe Address
effectiveAddress user date =
  fromAddressChange <|> currentAddress
  where
    -- User's pending address changes has only permanent address
    -- changes, so endDate can be ignored.
    fromAddressChange = map (fromDeliveryAddress <<< _.address) $
                        head $ sortWith (_.startDate) $ filter inEffect $ join $ fromFoldable $
                        Nullable.toMaybe user.pendingAddressChanges
    inEffect change = maybe false (date >= _) $ toDate change.startDate
    fromDeliveryAddress address =
      { zipcode: address.zipcode
      , city: Nullable.toMaybe address.city
      , streetAddress: fromMaybe "" $ Nullable.toMaybe address.streetAddress
      , countryCode: "FI"
      }
    fromUserAddress address =
      { streetAddress: address.streetAddress
      , zipcode: fromMaybe "" $ Nullable.toMaybe address.zipCode
      , city: Nullable.toMaybe address.city
      , countryCode: address.countryCode
      }
    currentAddress = fromUserAddress <$> Nullable.toMaybe user.address
