module KSF.Search where

import Prelude

import Data.Array (mapMaybe, drop, take)
import Data.Array as Array
import Data.Either (Either(..))
import Data.Foldable (intercalate, length, foldMap, sum, sequence_)
import Data.JSDate (toDate)
import Data.Maybe (Maybe(..), fromMaybe, isNothing, maybe)
import Data.Nullable (toMaybe)
import Data.String.Common as String
import Data.UUID (UUID, parseUUID)
import Effect (Effect)
import Effect.Aff as Aff
import Effect.Class (liftEffect)
import KSF.Api.Search (FaroUser, JanrainUser, SearchResult)
import KSF.Api.Subscription (toString) as Subsno
import KSF.AsyncWrapper as AsyncWrapper
import KSF.Helpers (formatDateDots)
import KSF.InputField as InputField
import KSF.User as User
import KSF.User.Cusno (Cusno)
import KSF.User.Cusno as Cusno
import React.Basic (JSX)
import React.Basic.Hooks (Component, component, useState, useState', (/\))
import React.Basic.Hooks as React
import React.Basic.DOM as DOM
import React.Basic.DOM.Events (preventDefault)
import React.Basic.Events as Events

type Props =
  { setActiveUser :: UUID -> Effect Unit
  }

type SearchActions =
  { setActiveUser :: UUID -> Effect Unit
  , loadSubs      :: Cusno -> Effect Unit
  }

search :: Component Props
search = do
  component "Search" \ { setActiveUser } -> React.do
    query /\ setQuery <- useState' Nothing
    results /\ setResults <- useState Nothing
    (searchWrapper :: AsyncWrapper.Progress JSX) /\ setSearchWrapper <- useState' AsyncWrapper.Ready
    let submitSearch = case query /\ (parseUUID =<< query) of
          Nothing /\ _ -> pure unit
          _ /\ Just uuid -> setActiveUser uuid
          Just q /\ _ -> do
            setSearchWrapper $ AsyncWrapper.Loading mempty
            Aff.launchAff_ do
              queryResult <- User.searchUsers { query: q, faroLimit: 10 }
              case queryResult of
                Right r -> liftEffect do
                  setResults $ const $ Just r
                  setSearchWrapper $ AsyncWrapper.Success Nothing
                Left e -> liftEffect do
                  setResults $ const Nothing
                  setSearchWrapper $ AsyncWrapper.Error e
        loadSubs cusno = do
          setSearchWrapper $ AsyncWrapper.Loading mempty
          Aff.launchAff_ do
            queryResult <- User.searchUsers { query: Cusno.toString cusno, faroLimit: 1 }
            case queryResult of
              Right res -> liftEffect $ sequence_ $
                             map (\r -> setResults $ map (map (updateSubs cusno r))) $
                             Array.concatMap (_.faro) res
              Left _ -> pure unit
            liftEffect $ setSearchWrapper $ AsyncWrapper.Success Nothing
        actions =
          { setActiveUser
          , loadSubs
          }
    pure $ React.fragment
      [ DOM.div { className: "search--container"
                , children: [ searchQuery query setQuery submitSearch
                            , searchResults actions searchWrapper results
                            ]
                }
      ]
  where
    updateSubs :: Cusno -> FaroUser -> SearchResult -> SearchResult
    updateSubs cusno result state =
      state { faro = map (\x -> if x.cusno == cusno then result else x) state.faro }

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

    searchResults actions searchWrapper results =
      AsyncWrapper.asyncWrapper
        { wrapperState: searchWrapper
        , readyView: searchInitial
        , editingView: \_ -> searchDone actions results
        , loadingView: searchLoading
        , successView: \_ -> searchDone actions results
        , errorView: searchError
        }

    searchInitial = DOM.div
      { className: "search--search-results mitt-konto--component-block-content"
      , children: [ DOM.text "Mata in e-post, namn (efternamn förnamn), cusno eller subsno (p:nnn)" ]
      }

    searchDone actions results = DOM.div
      { className: "search--search-results mitt-konto--component-block-content"
      , children: case results of
          Nothing -> [ DOM.text "ingen" ]
          Just x -> [ DOM.div
                        { className: "search--results-container"
                        , children:
                            [ DOM.table
                                { className: "search--results-table"
                                , children: [  DOM.thead_ [ headerRow1 , headerRow2 ]
                                            ,  DOM.tbody_ $ join $ renderResult actions <$> x
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

    headerRow1 =
      DOM.tr
        { className: "search--item-identity"
        , children:
            [ DOM.th_ [ DOM.text "Aktion" ]
            , DOM.th_ [ DOM.text "" ]
            , DOM.th_ [ DOM.text "E-post" ]
            , DOM.th_ [ DOM.text "Förnamn" ]
            , DOM.th_ [ DOM.text "Efternamn" ]
            , DOM.th { colSpan: 4, children: [ DOM.text "Kundnummer" ] }
            ]
        }

    headerRow2 =
      DOM.tr_ [ DOM.th_ [ ]
              , DOM.th_ [ DOM.text "Cusno" ]
              , DOM.th_ [ DOM.text "E-post" ]
              , DOM.th { colSpan: 2, children: [ DOM.text "Namn" ] }
              , DOM.th_ [ DOM.text "Adress" ]
              , DOM.th { colSpan: 3
                       , children: [ DOM.text "Prenumerationer" ]
                       }
              ]

    renderResult { setActiveUser, loadSubs } { janrain, faro } =
      foldMap (renderJanrain setActiveUser faro) janrain <>
      (Array.concatMap (renderFaro loadSubs $ isNothing janrain) faro)

    renderJanrain :: (UUID -> Effect Unit) -> Array FaroUser -> JanrainUser -> Array JSX
    renderJanrain setActiveUser faroResults user = pure $
      DOM.tr
        { className: "search--item-identity"
        , children:
            [ DOM.td
                { rowSpan: 1+(sum $ map faroLength faroResults)
                , children:
                    [ DOM.button
                        { onClick: Events.handler_ $ setActiveUser user.uuid
                        , children: [ DOM.text "Visa konto som kund" ]
                        }
                    ]
                }
            , DOM.td_ [ ]
            , DOM.td_ [ DOM.text $ fromMaybe "vet ej" user.email ]
            , DOM.td_ [ DOM.text $ fromMaybe "vet ej" user.firstName ]
            , DOM.td_ [ DOM.text $ fromMaybe "vet ej" user.lastName ]
            , DOM.td
                { colSpan: 4
                , children:
                    [ intercalate (DOM.text " / ") $
                      (foldMap (Array.singleton <<< DOM.text) user.cusno) <>
                      (foldMap (map (DOM.i_ <<< pure <<< DOM.text)) user.otherCusnos)
                    ]
                }
            ]
        }
      where
        faroLength x = maybe 1 (\s -> if Array.null s then 1 else length s) x.subs

    renderFaro :: (Cusno -> Effect Unit) -> Boolean -> FaroUser -> Array JSX
    renderFaro loadSubs standalone user =
      [ DOM.tr
          { className: if standalone then "search--standalone-cusno" else "search--sub-cusno"
          , children:
              (if standalone then [ td [] ] else mempty) <>
              [ td [ DOM.text $ Cusno.toString user.cusno ]
              , td [ DOM.text $ fromMaybe "vet ej" user.email ]
              , DOM.td
                  { rowSpan: rowSpan
                  , colSpan: 2
                  , children: [ DOM.text user.name ]
                  }
              , td $ maybe
                  [ DOM.text "vet ej" ]
                  (\address -> intercalate [DOM.br {}] $
                                 [ [ DOM.text address.streetAddress ]
                                 , [ DOM.text $ String.joinWith " " $
                                       mapMaybe toMaybe [ address.zipCode, address.city ] ]
                                 ] ) $
                  user.address
              ] <>
              case user.subs of
                Nothing -> [ loadableSubs user.cusno ]
                Just subs -> if Array.null subs
                               then [ DOM.td { colSpan: 3
                                             , children: [ DOM.text "Ingen" ]
                                             }
                                    ]
                               else subscriptionRow =<< take 1 subs
          }
      ] <> maybe mempty (\s -> subtr <<< subscriptionRow <$> drop 1 s) user.subs
      where
        td children = DOM.td { rowSpan: rowSpan, children: children }
        rowSpan = maybe 1 (\s -> if Array.null s then 1 else length s) user.subs
        subtr children =  DOM.tr { className: "search--subrow", children: children }
        subscriptionRow sub =
          [ DOM.td
              { className: "search--result-subsno-column"
              , children: [ DOM.text $ Subsno.toString sub.subsno ]
              }
          , DOM.td_ $ [ DOM.text $ sub.package.name ]
          , DOM.td_ $ [ DOM.text $ fromMaybe "ogiltig" $ formatDateDots <$> (toDate sub.dates.start) ]
          ]
        loadableSubs cusno =
           DOM.td { colSpan: 3
                  , children:
                      [ DOM.i
                          { className: "selectable"
                          , children: [ DOM.text "Inte laddad" ]
                          , onClick: Events.handler_ $ loadSubs cusno
                          }
                      ]
                  }
