module Mosaico.Test where

import Prelude

import Data.Newtype (over)
import Effect.Aff (Aff)
import Puppeteer as Chrome

type Test = Chrome.Page -> Aff Unit

sub :: String -> Chrome.Selector -> Chrome.Selector
sub specialize = over Chrome.Selector (_ <> specialize)

site :: String
site = "http://localhost:8080/"
