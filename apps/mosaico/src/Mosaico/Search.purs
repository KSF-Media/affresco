module Mosaico.Search where

import Prelude

import Data.Maybe (Maybe(..), isNothing)
import Data.Monoid (guard)
import Effect (Effect)
import KSF.InputField as InputField
import React.Basic (JSX)
import React.Basic.DOM as DOM
import React.Basic.DOM.Events (capture_)
import React.Basic.Hooks (Component, component, useState', (/\))
import React.Basic.Hooks as React

type Props =
  { query :: Maybe String
  , doSearch :: String -> Effect Unit
  , searching :: Boolean
  , noResults :: Boolean
  }

searchComponent :: Component Props
searchComponent = do
  component "SearchComponent" \props -> React.do
    query /\ setQuery <- useState' props.query
    pure $ render query setQuery props

render :: Maybe String -> (Maybe String -> Effect Unit) -> Props -> JSX
render query setQuery { doSearch, searching, noResults } =
  DOM.div
    { className: "mosaico-search"
    , children:
        [ DOM.span
            { className: "mosaico-search__header"
            , children: [ DOM.text "Sök" ]
            }
        , DOM.form
            { className: "mosaico-search__form"
            , children:
                [ DOM.span
                    { className: "mosaico-search__input"
                    , children:
                        [ InputField.inputField
                            { type_: InputField.Text
                            , name: "q"
                            , placeholder: ""
                            , label: Nothing
                            , value: query
                            , onChange: setQuery
                            , validationError: Nothing
                            , disabled: searching
                            }
                        ]
                    }
                , DOM.span
                    { className: "mosaico-search__submit"
                    , children:
                        [ DOM.button
                            { type: "submit"
                            , children: [ DOM.text "Sök" ]
                            , className: "button-green"
                            , disabled: isNothing query || query == Just "" || searching
                            }
                        ]
                    }
                ]
            , onSubmit: capture_
                case query of
                  Nothing -> pure unit
                  Just "" -> pure unit
                  Just q -> doSearch q
            }
        , guard noResults $
            DOM.div
              { className: "mosaico-search__message"
              , children: [ DOM.text "Inga resultat" ]
              }
        ]
    }
