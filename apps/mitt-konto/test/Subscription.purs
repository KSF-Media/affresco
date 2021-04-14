module MittKonto.Test.Subscription where

import Prelude

import Data.Date (adjust)
import Data.Time.Duration (Days(..))
import Data.Maybe (maybe)
import Effect.Class (liftEffect)
import Effect.Now as Now
import KSF.Helpers (formatDateDots)
import KSF.Test (formatDateSolid)
import MittKonto.Test (Test)
import Puppeteer as Chrome

testPause :: Test
testPause page = do
  now <- liftEffect Now.nowDate
  Chrome.waitFor_ (Chrome.Selector ".subscription--container") page
  -- TODO: parametrize this.
  subsno <- Chrome.getContent (Chrome.Selector ".subscription--container dl dd:nth-child(4)") page
  let pauseLink = Chrome.Selector $ "#subscription-" <> subsno <> " .subscription--pause-icon"
      unpauseLink = Chrome.Selector $ "#subscription-" <> subsno <> " .subscription--unpause-icon"
  -- Create pause
  Chrome.waitFor_ pauseLink page
  Chrome.click pauseLink page
  let startDate = adjust (Days 3.0) now
      endDate = adjust (Days 14.0) now
      startDateFieldText = maybe "fail" formatDateSolid startDate
      endDateFieldText = maybe "fail" formatDateSolid endDate
      startDateField = Chrome.Selector $ "#pause-start--" <> subsno <> " .react-date-picker"
      endDateField = Chrome.Selector $ "#pause-end--" <> subsno <> " .react-date-picker"
      startDateText = maybe "fail" formatDateDots startDate
      endDateText = maybe "fail" formatDateDots endDate
      pauseDisplayField = Chrome.Selector $ "#subscription-" <> subsno <> " .subscription--edit-subscription-pause"
  Chrome.waitFor_ startDateField page
  Chrome.assertNotFound pauseLink page
  Chrome.click startDateField page
  Chrome.type_ startDateField startDateFieldText page
  Chrome.click endDateField page
  Chrome.type_ endDateField endDateFieldText page
  Chrome.click (Chrome.Selector $ "#subscription-" <> subsno <> " form button[type='submit']") page
  -- Check that pause shows up correctly
  Chrome.waitFor_ unpauseLink page
  Chrome.assertContent pauseDisplayField ("Uppehåll: " <> startDateText <> " – " <> endDateText) page
  Chrome.assertNotFound (Chrome.Selector $ "#subscription-" <> subsno <> " .error-text") page
  -- Remove pause
  Chrome.click unpauseLink page
  Chrome.waitFor_ pauseLink page
  Chrome.assertNotFound unpauseLink page
  Chrome.assertNotFound pauseDisplayField page
