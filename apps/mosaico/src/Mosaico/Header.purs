module Mosaico.Header
  ( Props
  , component
  , render
  , mainSeparator
  , topLine
  ) where

import Prelude
import Data.Array (head, splitAt)
import Data.Either (Either(..))
import Data.Foldable (foldMap)
import Data.Int (ceil)
import Data.Maybe (Maybe, fromMaybe, maybe)
import Data.Newtype (unwrap)
import Data.Nullable (toMaybe)
import Data.String as String
import Data.Tuple (Tuple(..))
import Data.Tuple.Nested ((/\))
import Foreign.Object as Object
import KSF.Paper (toString)
import KSF.User (User)
import Lettera.Models (Categories, Category(..))
import Mosaico.Paper (mosaicoPaper)
import Mosaico.Routes (MosaicoPage(..), routes)
import React.Basic (JSX)
import React.Basic.DOM as DOM
import React.Basic.DOM.Events (capture_)
import React.Basic.Events (EventHandler, handler_)
import React.Basic.Hooks as React
import Routing (match)
import Routing.PushState (PushStateInterface)
import Simple.JSON (E, read, write)
import Web.Event.Event (EventType(..))
import Web.Event.EventTarget (addEventListener, eventListener, removeEventListener)
import Web.HTML (window)
import Web.HTML.Window (scroll, scrollY, toEventTarget)

type Props
  = { router :: PushStateInterface
    , categoryStructure :: Array Category
    , catMap :: Categories
    , onCategoryClick :: Category -> EventHandler
    , onLogin :: EventHandler
    , onProfile :: EventHandler
    , onStaticPageClick :: String -> EventHandler
    , user :: Maybe User
    }

component :: React.Component Props
component = do
  React.component "Header"
    $ \props -> React.do
        scrollPosition /\ setScrollPosition <- React.useState 0
        React.useEffect unit do
          w <- window
          listener <-
            eventListener
              ( \_ -> do
                  yPosition <- scrollY w
                  setScrollPosition (const $ ceil yPosition)
                  pure unit
              )
          addEventListener (EventType "scroll") listener false (toEventTarget w)
          addEventListener (EventType "pageshow") listener false (toEventTarget w)
          pure
            $ do
                _ <- removeEventListener (EventType "scroll") listener false (toEventTarget w)
                removeEventListener (EventType "pageshow") listener false (toEventTarget w)
        pure $ (render scrollPosition props)

render :: Int -> Props -> JSX
render scrollPosition props =
  DOM.header
    { className: "header-container" <> (if scrollPosition == 0 then "" else " static-header")
    , children:
        [ DOM.div
            { className: block
            , children:
                [ DOM.div
                    { className: block <> "__left-links"
                    , children:
                        [ DOM.a
                            { href: "/sida/kontakt"
                            , onClick: props.onStaticPageClick "kontakt"
                            , children: [ DOM.text "KONTAKTA OSS" ]
                            }
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
                                [ DOM.a
                                    { children: [ DOM.text "KUNDSERVICE" ]
                                    , href: "/sida/kundservice"
                                    , onClick: props.onStaticPageClick "kundservice"
                                    }
                                ]
                            , DOM.li_
                                [ DOM.a
                                    { className: block <> "__prenumerera-link"
                                    , children: [ DOM.text "PRENUMERERA" ]
                                    , href: "https://prenumerera.ksfmedia.fi/#/" <> String.toLower (toString mosaicoPaper)
                                    , target: "_blank"
                                    }
                                ]
                            ]
                        ]
                    }
                , DOM.a
                    { className: block <> "__logo"
                    , href: "/"
                    , onClick: foldMap props.onCategoryClick frontpageCategory
                    }
                , maybe
                    (DOM.div
                    { children:
                            [ DOM.span
                                { className: accountClass <> "-icon"
                                , children: [ DOM.span_ [] ]
                                }
                            , DOM.span
                                { className: "menu-label"
                                , children: [ DOM.text "LOGGA IN" ]
                                }
                            ]
                    , onClick: props.onLogin
                    , className: accountClass <> " " <> accountClass <> "--active"
                    , _data: Object.fromFoldable [Tuple "login" "1"]
                    }
                    )
                    (\name ->
                        DOM.a
                        { className: accountClass
                        , onClick: props.onProfile
                        , href: "/konto"
                        , children:
                            [ DOM.span
                                { className: accountClass <> "-icon"
                                , children: [ DOM.span_ [] ]
                                }
                            , DOM.span
                                { className: "menu-label"
                                , children: [ DOM.text name ]
                                }
                            ]
                        , _data: Object.fromFoldable [Tuple "loggedin" "1"]
                        }
                    ) $ (\user -> fromMaybe "INLOGGAD" $ toMaybe user.firstName) <$> props.user
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
                            , children: [ DOM.span { className: iconClass <> " " <> menuIconClass }
                                        , DOM.span
                                            { className: "menu-label"
                                            , children: [ DOM.text "MENY" ]
                                            }
                                        ]
                                    , onClick:
                                        handler_
                                        $ ( \r -> do
                                                locationState <- r.locationState
                                                case match (routes props.catMap) locationState.pathname of
                                                    Right MenuPage -> do
                                                        let
                                                          eitherState :: E { previousPath :: String }
                                                          eitherState = read locationState.state
                                                        case eitherState of
                                                          Right state -> r.pushState (write {}) state.previousPath
                                                          Left _ -> pure unit
                                                    _ -> do
                                                      void $ scroll 0 0 =<< window
                                                      r.pushState (write { previousPath: locationState.pathname }) "/meny"
                                            )
                                            props.router
                            }
                        ]
                    }
                ]
            }
        ]
    }
  where
    mkCategory category@(Category { label }) =
        DOM.a
        { href: "/" <> show label
        , onClick: props.onCategoryClick category
        , children: [ DOM.text $ String.toUpper $ unwrap label ]
        }

    { frontpageCategory, headerCategories } =
        let
        { after, before } = splitAt 1 props.categoryStructure
        in
        { frontpageCategory: head before, headerCategories: after }

    searchButton :: JSX
    searchButton = DOM.a
                    { className: iconButtonClass <> " " <> searchButtonClass
                    , children: [ DOM.span { className: iconClass <> " " <> searchIconClass }
                                , DOM.span
                                    { className: "menu-label"
                                    , children: [ DOM.text "SÖK" ]
                                    }
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
