module MittKonto.Test.Profile where

import Prelude

import Data.Date (adjust, month, year)
import Data.Enum (fromEnum) 
import Data.Time.Duration (Days(..))
import Data.Maybe (maybe)
import Data.String as String
import Effect.Class (liftEffect)
import Effect.Now as Now
import KSF.Helpers as Helpers
import KSF.Test (getTimeStamp)
import MittKonto.Test (Test)
import Puppeteer as Chrome

testNameChange :: Test
testNameChange page = do
  Chrome.click (Chrome.Selector ".profile--profile-row:nth-child(1) .profile--edit-text") page
  let firstNameField = Chrome.Selector ".profile--edit-name input[name='firstName']"
      firstNameText = Chrome.Selector ".profile--profile-row#profile--name dl dd:nth-child(2)"
  Chrome.waitFor_ firstNameField page
  Chrome.typeDelete_ firstNameField 10 page
  Chrome.type_ firstNameField "Testeri" page
  Chrome.click (Chrome.Selector ".profile--edit-name button[type='submit']") page
  Chrome.waitFor_ firstNameText page
  Chrome.assertContent firstNameText "Testeri" page

testAddressChange :: Test
testAddressChange page = do
  -- This should be close enough to succeed
  changeDate <- liftEffect $ adjust (Days 40.0) <$> Now.nowDate
  
  let changeDateString = maybe "fail" (("01." <> _) <<< String.drop 3 <<< Helpers.formatDateDots)
                         changeDate
      -- Date component is rather picky about text input, this should
      -- be safe
      dateFieldText = maybe "fail" (\d -> "1." <> (show $ fromEnum $ month d) <> "." <>
                                          (show $ fromEnum $ year d)) changeDate
      editAddressLink = Chrome.Selector ".profile--profile-row:nth-child(2) .profile--edit-text"
      dateField = Chrome.Selector ".profile--edit-address .react-date-picker__inputGroup"
      streetAddressField = Chrome.Selector ".profile--edit-address input[name='streetAddress']"
      zipcodeField = Chrome.Selector ".profile--edit-address input[name='zipCode']"
      cityField = Chrome.Selector ".profile--edit-address input[name='city']"
  -- Start edit
  Chrome.click editAddressLink page
  Chrome.waitFor_ streetAddressField page
  Chrome.click dateField page
  Chrome.type_ dateField dateFieldText page
  Chrome.typeDelete_ streetAddressField 25 page
  Chrome.type_ streetAddressField "Genvägen 8" page
  Chrome.typeDelete_ zipcodeField 5 page
  Chrome.type_ zipcodeField "10650" page
  Chrome.typeDelete_ cityField 20 page
  Chrome.type_ cityField "Ekenäs" page
  -- Submit edit
  Chrome.click (Chrome.Selector ".profile--edit-address button[type='submit']") page
  Chrome.waitFor_ editAddressLink page
  -- Check edit
  Chrome.assertContent (Chrome.Selector "#profile--display dt:nth-child(1)") "Adressändring:" page
  let expectedChange = "GENVÄGEN 8, 10650, EKENÄS (fr.o.m. " <> changeDateString <> ")"
  Chrome.assertContent (Chrome.Selector "#profile--display dd:nth-child(2)") expectedChange page
  Chrome.assertContent editAddressLink "Avbryt adressändringen" page
  -- Cancel edit
  Chrome.click editAddressLink page
  Chrome.waitFor_ editAddressLink page
  Chrome.assertContent (Chrome.Selector "#profile--display dt:nth-child(1)") "Kundnummer:" page
  Chrome.assertContent editAddressLink "Ändra" page

testEmailChange :: Test
testEmailChange page = do
  dateTimeStr <- liftEffect getTimeStamp
  let email = "mittkonto+test." <> dateTimeStr <> "+change@ksfmedia.fi"
      editEmailLink = Chrome.Selector ".profile--profile-row:nth-child(3) .profile--edit-text"
      emailField = Chrome.Selector ".profile--edit-email input[name='email']"
  Chrome.click editEmailLink page
  Chrome.typeDelete_ emailField 50 page
  Chrome.type_ emailField email page
  Chrome.click (Chrome.Selector ".profile--edit-email button[type='submit']") page
  Chrome.waitFor_ editEmailLink page
  Chrome.assertContent (Chrome.Selector "#profile--email dd:nth-child(2)") email page
