module Mosaico.Test where

import Prelude

import Data.Array (find, mapMaybe)
import Data.Enum (enumFromTo)
import Data.Newtype (over)
import Data.Traversable (traverse)
import Effect.Aff (Aff)
import Effect.Class.Console (log) as Console
import KSF.Puppeteer as Chrome

type Test = Chrome.Page -> Aff Unit

log :: String -> Aff Unit
log msg = Console.log $ "> " <> msg

sub :: String -> Chrome.Selector -> Chrome.Selector
sub specialize = over Chrome.Selector (_ <> specialize)

site :: String
site = "http://localhost:8080/"

listArticle :: Chrome.Selector
listArticle = Chrome.Selector ".mosaico--article-list article"

-- Mosaico's and Lettera's data might not match due to timing and
-- caching issues.  These functions are for testing that they have any
-- common elements, to test that they load and aren't grossly out of
-- date or have completely wrong data.
type TaggedContent =
  { i :: Int
  , content :: String
  }

tagListWithSelector :: Int -> (Int -> Aff String) -> Aff (Array TaggedContent)
tagListWithSelector n f =
  let getContent i = do
        content <- f i
        pure { i, content }
  in traverse getContent $ enumFromTo 1 n

type MatchedContent a =
  { i :: Int
  , content :: String
  , match :: a
  }

matchTagList :: forall a. Array TaggedContent -> Array a -> (String -> a -> Boolean) -> Array (MatchedContent a)
matchTagList tagged haystack f =
  let matcher { i, content } = (\match -> { i, content, match })
                               <$> find (f content) haystack
  in mapMaybe matcher tagged
