module KSF.Test where

import Prelude

import Data.Date (Date)
import Data.DateTime (DateTime(..), adjust)
import Data.Formatter.DateTime as Format
import Data.Int (toNumber)
import Data.Time.Duration (Days(..))
import Data.List as List
import Data.Maybe (maybe)
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Now as Now
import KSF.Helpers as Helpers
import KSF.Puppeteer as Chrome

getTimeStamp :: Effect String
getTimeStamp = formatDate <$> Now.nowDateTime

formatDate :: DateTime -> String
formatDate = Format.format $ List.fromFoldable
  [ Format.YearFull
  , Format.MonthTwoDigits
  , Format.DayOfMonthTwoDigits
  , Format.Hours24
  , Format.MinutesTwoDigits
  , Format.SecondsTwoDigits
  ]

-- For input to date component
formatDateSolid :: Date -> String
formatDateSolid date = Format.format formatter $ DateTime date Helpers.midnight
  where
    formatter = List.fromFoldable
      [ Format.DayOfMonthTwoDigits
      , Format.MonthTwoDigits
      , Format.YearFull
      ]

type TestCreditCard =
  { number :: String
  , year :: String
  }

getTestCard :: Int -> Effect TestCreditCard
getTestCard n = do
  now <- Now.nowDateTime
  let getYear x =
        let diff = Days $ 365.0 * toNumber x
        in maybe "30"
           (Format.format (List.fromFoldable [ Format.YearTwoDigits ])) $ adjust diff now
  pure $ case n of
    1 -> { number: "4539612884950332", year: getYear 10 }
    _ -> { number: "4925000000000004", year: getYear 8 }

typeCreditCard :: forall a. Chrome.HasFrame a => a -> TestCreditCard -> Aff Unit
typeCreditCard page card = do
  let creditCardField = Chrome.Selector "#cardNumber"
  Chrome.waitFor_ creditCardField page
  Chrome.type_ creditCardField card.number page
  Chrome.select (Chrome.Selector "#year") card.year page
  Chrome.type_ (Chrome.Selector "#securityCode") "666" page
  Chrome.click (Chrome.Selector "#okButton") page
