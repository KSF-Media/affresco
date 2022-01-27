module Mosaico.Header where

import Prelude

import Data.Array (head, splitAt)
import Data.Either (Either(..))
import Data.Foldable (foldMap)
import Data.Maybe (Maybe, maybe)
import Data.Newtype (unwrap)
import Data.Nullable (toMaybe)
import Data.String as String
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Foreign.Object as Object
import KSF.User (User)
import Lettera.Models (Categories, Category(..))
import Mosaico.Routes (MosaicoPage(..), routes)
import React.Basic (JSX)
import React.Basic.DOM as DOM
import React.Basic.DOM.Events (capture_)
import React.Basic.Events (EventHandler, handler_)
import Routing (match)
import Routing.PushState (PushStateInterface)
import Simple.JSON (E, read, write)

type Props =
  { router :: PushStateInterface
  , categoryStructure :: Array Category
  , catMap :: Categories
  , onCategoryClick :: Category -> EventHandler
  , onLogin :: Effect Unit
  , user :: Maybe User
  }

render :: Props -> JSX
render props =
  DOM.header
    { className: block
    , children:
        [ DOM.div
            { className: block <> "__left-links"
            , children:
                [ DOM.a_ [ DOM.text "KONTAKTA OSS" ]
                , DOM.text "|"
                , DOM.a
                    { children: [ DOM.text "E-TIDNINGEN" ]
                    , href: "/epaper"
                    , onClick: capture_ $ props.router.pushState (write {}) "/epaper"
                    }
                ]
            }
        , DOM.div
            { className: block <> "__right-links"
            , children:
                [ DOM.ul_
                    [ DOM.li_
                        [ DOM.a_ [ DOM.text "KUNDSERVICE" ]
                        ]
                    , DOM.li_
                        [ DOM.a
                            { className: block <> "__prenumerera-link"
                            , children: [ DOM.text "PRENUMERERA" ]
                            }
                        ]
                    ]
                ]
            }
        , DOM.div
            { className: block <> "__logo"
            , onClick: foldMap props.onCategoryClick frontpageCategory
            }
        , maybe
            (DOM.div
               { children: [ DOM.text "LOGGA IN" ]
               , onClick: handler_ props.onLogin
               , className: accountClass <> " " <> accountClass <> "--active"
               , _data: Object.fromFoldable [Tuple "login" "1"]
               }
            )
            (\firstName ->
                DOM.div
                  { className: accountClass
                  , children: [ DOM.text firstName ]
                  }
            ) $ toMaybe <<< _.firstName =<< props.user
        , DOM.nav
            { className: block <> "__center-links"
            , children: map mkCategory headerCategories
            }
        , DOM.div
            { className: block <> "__right-buttons"
            , children:
                [ searchButton
                , DOM.div
                    { className: iconButtonClass <> " " <> menuButtonClass
                    , children: [ DOM.div_ [ DOM.text "MENU" ]
                                , DOM.div { className: iconClass <> " " <> menuIconClass }
                                ]
                    , onClick: handler_ $
                        (\r -> do
                          locationState <- r.locationState
                          case match (routes props.catMap) locationState.pathname of
                            Right MenuPage -> do
                              let
                                eitherState :: E { previousPath :: String }
                                eitherState = read locationState.state
                              case eitherState of
                                Right state -> r.pushState (write { }) state.previousPath
                                Left _      -> pure unit
                            _              -> r.pushState (write { previousPath: locationState.pathname }) "/meny")
                          props.router

                    }
                ]
            }
        ]
    }
  where
    mkCategory category@(Category { label }) =
      DOM.a { href: "/" <> show label
            , onClick: props.onCategoryClick category
            , children: [ DOM.text $ String.toUpper $ unwrap label ]
            }

    { frontpageCategory, headerCategories } =
      let { after, before } = splitAt 1 props.categoryStructure
      in  { frontpageCategory: head before, headerCategories: after }

    searchButton :: JSX
    searchButton = DOM.a
                    { className: iconButtonClass <> " " <> searchButtonClass
                    , children: [ DOM.div_ [ DOM.text "SÖK" ]
                                , DOM.div { className: iconClass <> " " <> searchIconClass }
                                ]
                    , href: "/sök"
                    , onClick: capture_ $ props.router.pushState (write {}) "/sök"
                    }

    block = "mosaico-header"

    accountElement = "__account"
    accountClass = block <> accountElement

    searchModifier = "--search"
    menuModifier = "--menu"

    iconButtonElement = "__icon-button"
    iconButtonClass = block <> iconButtonElement
    searchButtonClass = iconButtonClass <> searchModifier
    menuButtonClass = iconButtonClass <> menuModifier

    iconElement = "__icon"
    iconClass = block <> iconElement
    searchIconClass = iconClass <> searchModifier
    menuIconClass = iconClass <> menuModifier

-- The characteristic line at the top of every KSF media's site
topLine :: JSX
topLine = DOM.hr { className: "mosaico-top-line" }

-- The separator between the header and the rest of the page
mainSeparator :: JSX
mainSeparator = DOM.hr { className: "mosaico-main-separator" }
