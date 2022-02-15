module MittKonto.Test.Profile where

import Prelude

import Data.Date (adjust)
import Data.Time.Duration (Days(..))
import Data.Maybe (maybe)
import Effect.Class (liftEffect)
import Effect.Class.Console (log)
import Effect.Now as Now
import KSF.Helpers as Helpers
import KSF.Test (getTimeStamp, formatDateSolid)
import MittKonto.Test (Test, typeTestAddress)
import KSF.Puppeteer as Chrome

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
  changeDate <- liftEffect $ adjust (Days 3.0) <$> Now.nowDate
  let changeDateString = maybe "fail" Helpers.formatDateDots changeDate
      dateFieldText = maybe "fail" formatDateSolid changeDate
      editAddressLink = Chrome.Selector ".profile--profile-row:nth-child(2) .profile--edit-text"
      dateField = Chrome.Selector ".profile--edit-address .react-date-picker__inputGroup"
  -- Start edit
  Chrome.click editAddressLink page
  Chrome.waitFor_ dateField page
  Chrome.click dateField page
  Chrome.type_ dateField dateFieldText page
  typeTestAddress ".profile--edit-address" "streetAddress" page
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

-- Note that this test isn't idempotent.  If you're reusing an account
-- to develope tests, you may want to disable this.
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
  log $ "email changed to " <> show email
  Chrome.waitFor_ editEmailLink page
  Chrome.assertContent (Chrome.Selector "#profile--email dd:nth-child(2)") email page
