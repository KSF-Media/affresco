module Mosaico.Lists where
  
import Prelude

import Effect (Effect)
import Effect.Aff (Aff, throwError)
import Effect.Aff as Aff
import Effect.Class (liftEffect)
import Effect.Class.Console as Console
import Effect.Exception (error)
import KSF.Paper (Paper(..))
import Lettera as Lettera
import Lettera.Models (ArticleStub)
import React.Basic (JSX)
import React.Basic.DOM as DOM
import React.Basic.Hooks (Component, component, useEffect, useEffectOnce, useState, (/\))
import React.Basic.Hooks as React


type Self =
  { state :: State
  , setState :: (State -> State) -> Effect Unit
  , props :: Props
  }

type Props =
  { start :: Int
  , limit :: Int
  , caategory :: String
  , paper :: Paper
  , onlySubscribers :: Boolean
  }

type State =
  { articles :: Array ArticleStub
  }  

mostreadArticleComponent :: Component Props
mostreadArticleComponent = do
  component "MostReadArticles" \props -> React.do
    let initialState = { articles: [] }
    state /\ setstate <- useState initialState
    useEffect do
      Aff.launchAff_  do
        mostread <- Lettera.getMostRead 0 10 "" HBL true
        Console.log $ show mostread
        -- liftEffect $ setstate \s -> s { articles: mostread }

    pure mempty