module KSF.Test where

import Prelude

import Data.Date (Date)
import Data.DateTime (DateTime(..))
import Data.Formatter.DateTime as Format
import Data.List as List
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Now as Now
import KSF.Helpers as Helpers
import Puppeteer as Chrome

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

typeCreditCard :: forall a. Chrome.HasFrame a => a -> Aff Unit
typeCreditCard page = do
  let creditCardNumber = "4925000000000004"
  let creditCardField = Chrome.Selector "#cardNumber"
  Chrome.waitFor_ creditCardField page
  Chrome.type_ creditCardField creditCardNumber page
  Chrome.select (Chrome.Selector "#year") "30" page -- Year 2030
  Chrome.type_ (Chrome.Selector "#securityCode") "666" page
  Chrome.click (Chrome.Selector "#okButton") page
