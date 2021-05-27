module MittKonto.Test where

import Prelude

import Puppeteer as Chrome
import Effect.Aff (Aff)

type Test = Chrome.Page -> Aff Unit

typeTestAddress :: String -> String -> Test
typeTestAddress sel addressFieldName page = do
  let streetAddressField = Chrome.Selector $ sel <> " input[name='" <> addressFieldName <> "']"
      zipcodeField = Chrome.Selector $ sel <> " input[name='zipCode']"
      cityField = Chrome.Selector $ sel <> " input[name='city']"
  Chrome.typeDelete_ streetAddressField 25 page
  Chrome.type_ streetAddressField "Genvägen 8" page
  Chrome.typeDelete_ zipcodeField 5 page
  Chrome.type_ zipcodeField "10650" page
  Chrome.typeDelete_ cityField 20 page
  Chrome.type_ cityField "Ekenäs" page

getFirstSubsno :: Chrome.Page -> Aff String
getFirstSubsno =
  Chrome.getData (Chrome.Selector ".subscription--container") "subsno"
