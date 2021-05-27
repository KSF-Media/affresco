module MittKonto.Test.Subscription where

import Prelude

import Data.Date (adjust)
import Data.Int (toNumber)
import Data.Time.Duration (Days(..), Milliseconds(..))
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Effect.Aff as Aff
import Effect.Class (liftEffect)
import Effect.Now as Now
import KSF.Helpers (formatDateDots)
import KSF.Test (formatDateSolid)
import MittKonto.Test (Test, getFirstSubsno, typeTestAddress)
import Puppeteer as Chrome

testPause :: Test
testPause page = do
  now <- liftEffect Now.nowDate
  Chrome.waitFor_ (Chrome.Selector ".subscription--container") page
  subsno <- getFirstSubsno page
  let pauseDisplayField = Chrome.Selector $ "#subscription-" <> subsno <> " .subscription--subscription-pause"
      pauseLink = Chrome.Selector $ "#subscription-" <> subsno <> " .subscription--pause-icon"
      unpauseLink = Chrome.Selector $ "#subscription-" <> subsno <> " .subscription--unpause-icon"
      testPauseForm startDays endDays = do
        let startDate = adjust (Days $ toNumber startDays) now
            endDate = adjust (Days $ toNumber endDays) now
            startDateFieldText = maybe "fail" formatDateSolid startDate
            endDateFieldText = maybe "fail" formatDateSolid endDate
            startDateField = Chrome.Selector $ "#pause-start--" <> subsno <> " .react-date-picker"
            endDateField = Chrome.Selector $ "#pause-end--" <> subsno <> " .react-date-picker"
            startDateText = maybe "fail" formatDateDots startDate
            endDateText = maybe "fail" formatDateDots endDate
        Chrome.waitFor_ startDateField page
        Chrome.assertNotFound pauseLink page
        Chrome.click startDateField page
        Chrome.type_ startDateField startDateFieldText page
        Chrome.click endDateField page
        Chrome.type_ endDateField endDateFieldText page
        Chrome.click (Chrome.Selector $ "#subscription-" <> subsno <> " form button[type='submit']") page
        -- Check that pause shows up correctly
        Chrome.waitFor_ unpauseLink page
        Chrome.assertContent pauseDisplayField ("ÄndraUppehåll: " <> startDateText <> " – " <> endDateText) page
        Chrome.assertNotFound (Chrome.Selector $ "#subscription-" <> subsno <> " .error-text") page

  -- Create pause
  Chrome.waitFor_ pauseLink page
  Aff.delay $ Milliseconds 500.0
  Chrome.click pauseLink page
  Aff.delay $ Milliseconds 500.0
  testPauseForm 3 14
  -- Edit pause
  Chrome.click (Chrome.Selector $ "#subscription-" <> subsno <>
                " .subscription--subscription-pause .edit-icon") page
  testPauseForm 5 16
  -- Remove pause
  Chrome.click unpauseLink page
  Chrome.waitFor_ pauseLink page
  Chrome.assertNotFound unpauseLink page
  Chrome.assertNotFound pauseDisplayField page

testTemporaryAddressChange :: Test
testTemporaryAddressChange page = do
  now <- liftEffect Now.nowDate
  Chrome.waitFor_ (Chrome.Selector ".subscription--container") page
  subsno <- getFirstSubsno page
  let changeLink = Chrome.Selector $ "#subscription-" <> subsno <> " .subscription--temporary-address-change-icon"
      stopChangeLink = Chrome.Selector $ "#subscription-" <> subsno <> " .subscription--delete-temporary-address-change-icon"
  Chrome.waitFor_ changeLink page
  Chrome.assertNotFound stopChangeLink page
  -- Create temporary address change
  Chrome.click changeLink page
  let startDate = adjust (Days 3.0) now
      startDateFieldText = maybe "fail" formatDateSolid startDate
      startDateText = maybe "fail" formatDateDots startDate
      editStartDate = adjust (Days 5.0) now
      editEndDate = adjust (Days 15.0) now
      editStartDateFieldText = maybe "fail" formatDateSolid editStartDate
      editEndDateFieldText = maybe "fail" formatDateSolid editEndDate
      editStartDateText = maybe "fail" formatDateDots editStartDate
      editEndDateText = maybe "fail" formatDateDots editEndDate
      startDateField = Chrome.Selector $ "#edit-start--" <> subsno <> " .react-date-picker"
      endDateField = Chrome.Selector $ "#edit-end--" <> subsno <> " .react-date-picker"
      indefiniteCheckbox = Chrome.Selector $ "#edit-indefinite--" <> subsno
      temporaryNameField = Chrome.Selector $ "#subscription-" <> subsno <> " input[name='temporaryName']"
      pendingAddressText = Chrome.Selector $ "#subscription-" <> subsno <> "-pending-address-change-1"
      check start end = do
        Chrome.waitFor_ stopChangeLink page
        Chrome.waitFor_ (Chrome.Selector $ "#subscription-" <> subsno <> " .actions-wrapper--success") page
        Chrome.assertNotFound (Chrome.Selector $ "#subscription-" <> subsno <> " .error-text") page
        Chrome.assertContent pendingAddressText
          ("ÄndraKSF, GENVÄGEN 8, 10650, EKENÄS (" <> start <> " – " <> fromMaybe "" end <> ")") page
  Chrome.waitFor_ startDateField page
  Chrome.click startDateField page
  Chrome.type_ startDateField startDateFieldText page
  Chrome.click indefiniteCheckbox page
  typeTestAddress ("#subscription-" <> subsno) "address" page
  Chrome.type_ temporaryNameField "KSF" page
  Chrome.click (Chrome.Selector $ "#subscription-" <> subsno <> " button[type='submit']") page
  -- Check that it shows correctly
  check startDateText Nothing
  -- Edit temporary pause
  Chrome.click (Chrome.Selector $ "#subscription-" <> subsno <>
                "-pending-address-change-1 .edit-icon") page
  Chrome.waitFor_ startDateField page
  Chrome.click (Chrome.Selector $ "#edit-start--" <> subsno <> " .react-date-picker__clear-button") page
  Chrome.click startDateField page
  Chrome.type_ startDateField editStartDateFieldText page
  Chrome.click indefiniteCheckbox page
  Chrome.click endDateField page
  Chrome.type_ endDateField editEndDateFieldText page
  Chrome.click (Chrome.Selector $ "#subscription-" <> subsno <> " button[type='submit']") page
  -- Check that edit shows correctly
  check editStartDateText (Just editEndDateText)
  -- Remove pause
  Chrome.click stopChangeLink page
  Chrome.waitFor_ changeLink page
  Chrome.waitFor_ (Chrome.Selector $ "#subscription-" <> subsno <> " .actions-wrapper--success") page
  Chrome.assertNotFound stopChangeLink page
  Chrome.assertNotFound (Chrome.Selector $ "#subscription-" <> subsno <> " .error-text") page
  Chrome.assertNotFound (Chrome.Selector $ "#subscription--" <> subsno <> "-edit-pending-address-change-1") page
