module Mosaico.Test where

import Prelude

import Data.Newtype (over)
import Effect.Aff (Aff)
import Effect.Class.Console (log) as Console
import Puppeteer as Chrome

type Test = Chrome.Page -> Aff Unit

log :: String -> Aff Unit
log msg = Console.log $ "> " <> msg

sub :: String -> Chrome.Selector -> Chrome.Selector
sub specialize = over Chrome.Selector (_ <> specialize)

site :: String
site = "http://localhost:8080/"

listArticle :: Chrome.Selector
listArticle = Chrome.Selector ".mosaico--article-list .mosaico--list-article"
