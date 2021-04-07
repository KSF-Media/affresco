module MittKonto.Test where

import Prelude

import Puppeteer as Chrome
import Effect.Aff (Aff)

type Test = Chrome.Page -> Aff Unit
