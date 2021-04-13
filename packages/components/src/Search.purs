module KSF.Search where

import Prelude

import Data.Array (mapMaybe, drop, take)
import Data.Array as Array
import Data.Either (Either(..))
import Data.Foldable (intercalate, length)
import Data.JSDate (toDate)
import Data.Maybe (Maybe(..), fromMaybe, isNothing, maybe)
import Data.Nullable (toMaybe)
import Data.String.Common as String
import Data.UUID (UUID, parseUUID, emptyUUID)
import Effect (Effect)
import Effect.Aff as Aff
import Effect.Class (liftEffect)
import KSF.Api.Subscription (toString) as Subsno
import KSF.AsyncWrapper as AsyncWrapper
import KSF.Helpers (formatDateDots)
import KSF.InputField as InputField
import KSF.User as User
import KSF.User.Cusno as Cusno
import React.Basic (JSX)
import React.Basic.Hooks (Component, component, useState', (/\))
import React.Basic.Hooks as React
import React.Basic.DOM as DOM
import React.Basic.DOM.Events (preventDefault)
import React.Basic.Events as Events

type Props =
  { setActiveUser :: UUID -> Effect Unit
  }

search :: Component Props
search = do
  component "Search" \ { setActiveUser } -> React.do
    query /\ setQuery <- useState' Nothing
    results /\ setResults <- useState' Nothing
    (searchWrapper :: AsyncWrapper.Progress JSX) /\ setSearchWrapper <- useState' AsyncWrapper.Ready
    let submitSearch = case query /\ (parseUUID =<< query) of
          Nothing /\ _ -> pure unit
          _ /\ Just uuid -> setActiveUser uuid
          Just q /\ _ -> do
            setSearchWrapper $ AsyncWrapper.Loading mempty
            Aff.launchAff_ do
              queryResult <- User.searchUsers q
              case queryResult of
                Right r -> liftEffect do
                  setResults $ Just r
                  setSearchWrapper $ AsyncWrapper.Success Nothing
                Left e -> liftEffect do
                  setResults Nothing
                  setSearchWrapper $ AsyncWrapper.Error e
    pure $ React.fragment
      [ DOM.div { className: "search--container"
                , children: [ searchQuery query setQuery submitSearch
                            , searchResults setActiveUser searchWrapper results
                            ]
                }
      ]
  where
    searchQuery query setQuery submitSearch =
      DOM.div
        { className: "search--query mitt-konto--container clearfix"
        , children:
            [ DOM.span
                { className: "mitt-konto--component-heading"
                , children: [ DOM.text "Sök"]
                }
            , DOM.form
                { className: "search--query-form mitt-konto--component-block-content"
                , children:
                    [ DOM.span
                        { className: "search--query-input"
                        , children:
                            [ InputField.inputField
                                { type_: InputField.Text
                                , name: "query"
                                , placeholder: "Sök"
                                , label: Nothing
                                , value: query
                                , onChange: setQuery
                                , validationError: Nothing
                                }
                            ]
                        }
                    , DOM.button
                        { type: "submit"
                        , children: [ DOM.text "Sök" ]
                        , className: "button-green search--query-submit"
                        , disabled: isNothing query || query == Just ""
                        }
                    ]
                , onSubmit: Events.handler preventDefault
                    $ \_ -> submitSearch
                }
            ]
        }

    searchResults setActiveUser searchWrapper results =
      AsyncWrapper.asyncWrapper
        { wrapperState: searchWrapper
        , readyView: searchInitial
        , editingView: \_ -> searchDone setActiveUser results
        , loadingView: searchLoading
        , successView: \_ -> searchDone setActiveUser results
        , errorView: searchError
        }

    searchInitial = DOM.div
      { className: "search--search-results mitt-konto--component-block-content"
      , children: [ DOM.text "Mata in e-post, namn (efternamn förnamn), cusno eller subsno (p:nnn)" ]
      }

    searchDone setActiveUser results = DOM.div
      { className: "search--search-results mitt-konto--component-block-content"
      , children: case results of
          Nothing -> [ DOM.text "ingen" ]
          Just x -> [ DOM.div
                        { className: "search--results-container"
                        , children:
                            [ DOM.table
                                { className: "search--results-table"
                                , children: [ DOM.thead_ [ headerRow ]
                                            , DOM.tbody_ $ join $ renderUser setActiveUser <$> x
                                            ]
                                }
                            ]
                        }
                    ]
      }

    searchLoading spinner = DOM.div
      { className: "search--search-results mitt-konto--component-block-content"
      , children:
          [ DOM.div
              { className: "search--spinner-container"
              , children: [ spinner ]
              }
          , DOM.div_ [ DOM.text "sök" ]
          ]
      }

    searchError msg = DOM.div
      { className: "search--search-results mitt-konto--component-block-content"
      , children:
          [ DOM.div_ [ DOM.text "Något gick fel" ]
          , DOM.div_ [ DOM.text msg ]
          ]
      }

    headerRow =
      DOM.tr_ [ DOM.th_ [ DOM.text "Cusno" ]
              , DOM.th_ [ DOM.text "Namn" ]
              , DOM.th_ [ DOM.text "E-post" ]
              , DOM.th_ [ DOM.text "Adress" ]
              , DOM.th { colSpan: 3
                       , children: [ DOM.text "Prenumerationer" ]
                       }
              ]

    renderUser setActiveUser user =
      [ DOM.tr
          { className: "search--item"
              <> if haveDetails then " selectable" else " unselectable"
          , onClick: Events.handler_ $ when haveDetails $ setActiveUser user.uuid
          , children:
              [ td [ DOM.text $ Cusno.toString user.cusno ]
              , td $ map DOM.text $ pure $ String.joinWith " " $
                  mapMaybe toMaybe [ user.firstName, user.lastName ]
              , td [ DOM.text user.email ]
              , td $ maybe
                  [ DOM.text "Vet ej" ]
                  (\address -> intercalate [DOM.br {}] $
                                 [ [ DOM.text address.streetAddress ]
                                 , [ DOM.text $ String.joinWith " " $
                                       mapMaybe toMaybe [ address.zipCode, address.city ] ]
                                 ] ) $
                  toMaybe user.address
              ] <>
              ( if Array.null user.subs
                  then [ DOM.td { colSpan: 3
                                , children: [ DOM.text "Ingen" ]
                                }
                       ]
                  else subscriptionRow =<< take 1 user.subs
              )
          }
      ] <> (DOM.tr_ <<< subscriptionRow <$> drop 1 user.subs)

      where
        haveDetails = user.uuid /= emptyUUID
        td children = DOM.td { rowSpan: rowSpan, children: children }
        rowSpan = if Array.null user.subs then 1 else length user.subs
        subscriptionRow sub =
          [ DOM.td
              { className: "search--result-subsno-column"
              , children: [ DOM.text $ Subsno.toString sub.subsno ]
              }
          , DOM.td_ $ [ DOM.text $ sub.package.name ]
          , DOM.td_ $ [ DOM.text $ fromMaybe "ogiltig" $ formatDateDots <$> (toDate sub.dates.start) ]
          ]
