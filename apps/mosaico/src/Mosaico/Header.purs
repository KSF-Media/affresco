module Mosaico.Header
  ( Props
  , component
  , render
  ) where

import Prelude
import Control.Bind ((>>=))
import Data.Array (head, splitAt)
import Data.Either (Either(..))
import Data.Foldable (foldMap)
import Data.Int (ceil)
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Newtype (unwrap)
import Data.Nullable (Nullable, null, toMaybe)
import Data.String as String
import Data.Tuple (Tuple(..))
import Data.Tuple.Nested ((/\))
import Foreign.Object as Object
import KSF.Paper (toString)
import KSF.Spinner (loadingSpinner)
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
import Web.DOM (Node) as DOM
import Web.DOM.Document (toNonElementParentNode)
import Web.DOM.NonElementParentNode (getElementById)
import Web.DOM.Element (clientTop)
import Web.HTML (window)
import Web.HTML.Window (scroll, scrollY, toEventTarget, document)
import Web.HTML.HTMLDocument (toDocument)
import Web.HTML.HTMLElement (HTMLElement, fromNode, fromElement, offsetTop)
import Effect.Uncurried (EffectFn1, runEffectFn1)

foreign import getBoundingClientRectTop :: EffectFn1 HTMLElement Number

type Props =
  { router :: PushStateInterface
  , categoryStructure :: Array Category
  , catMap :: Categories
  , onCategoryClick :: Category -> EventHandler
  , onLogin :: EventHandler
  , onProfile :: EventHandler
  , onStaticPageClick :: String -> EventHandler
  , user :: Maybe (Maybe User)
  }

component :: React.Component Props
component = do
  React.component "Header"
    $ \props -> React.do
        headerPosition /\ setHeaderPosition <- React.useState 0
        scrollPosition /\ setScrollPosition <- React.useState 0
        React.useEffect unit do
          w <- window
          listener <-
            eventListener
              ( \_ -> do
                  yPosition <- scrollY w
                  setScrollPosition (const $ ceil yPosition)
                  d <- document w
                  (getElementById "HBL" $ toNonElementParentNode $ toDocument d) >>= (\e -> pure $ e >>= fromElement)
                    >>= case _ of
                      Nothing -> pure unit
                      Just node -> do
                        rect <- (runEffectFn1 getBoundingClientRectTop) node
                        setHeaderPosition (const $ ceil rect)
                        pure unit
                  pure unit
              )
          addEventListener (EventType "scroll") listener false (toEventTarget w)
          addEventListener (EventType "pageshow") listener false (toEventTarget w)
          pure
            $ do
                _ <- removeEventListener (EventType "scroll") listener false (toEventTarget w)
                removeEventListener (EventType "pageshow") listener false (toEventTarget w)
        pure $ (render headerPosition 0 props)

render :: Int -> Int -> Props -> JSX
render headerTopPosition _ props =
  DOM.header
    { className: "mosaico-header header-container" <> (if headerTopPosition >= 0 then "" else " static-header")
    , children:
        [ innerContainer "top-line" topLine
        , DOM.div {className: "top-line-separator", children: []}
        , innerContainer "first-line" firstHeaderRow
        , DOM.div {className: "first-line-separator", children: []}
        , innerContainer "logo" logo -- shown for only medium/large sizes here
        , innerContainer "second-line" secondHeaderRow
        , DOM.div {className: "second-line-separator", children: []}
        , DOM.div {className: "header-bottom-border", children: []}
        ]
    }
  where
  innerContainer name child = DOM.div
    { className: "header-container-inner-" <> name
    , children: [ child ]
    }
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

  leftLinks =
    DOM.div
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

  rightLinks =
    DOM.div
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

  logo =
    DOM.div
    { className: block <> "__logo-container"
    , children: 
      [DOM.a
      { className: block <> "__logo"
      , href: "/"
      , onClick: foldMap props.onCategoryClick frontpageCategory
      }
    ]}

  firstHeaderRow =
    DOM.div
      { className: block <> "__first-line"
      , children:
          [ leftLinks
          , rightLinks
          ]
      }

  profileLink = renderLoginLink props.user

  navigationLinks =
    DOM.nav
      { className: block <> "__center-links"
      , children: map mkCategory headerCategories
      }

  handleMenuClick =
    handler_
      $
        ( \r -> do
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

  menuButton = DOM.div
    { className: iconButtonClass <> " " <> menuButtonClass
    , children:
        [ DOM.span { className: iconClass <> " " <> menuIconClass }
        , DOM.span
            { className: "menu-label"
            , children: [ DOM.text "MENY" ]
            }
        ]
    , onClick: handleMenuClick
    }

  rightButtons =
    DOM.div
      { className: block <> "__right-buttons"
      , children:
          [ searchButton
          , menuButton
          ]
      }

  secondHeaderRow =
    DOM.div
      { className: block <> "__second-line"
      , children:
          [ profileLink
          , navigationLinks
          , logo -- shown only for mobile sizes here
          , rightButtons
          ]
      }

  searchButton :: JSX
  searchButton =
    DOM.a
      { className: iconButtonClass <> " " <> searchButtonClass
      , children:
          [ DOM.span { className: iconClass <> " " <> searchIconClass }
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

  renderLoginLink Nothing =
    loadingSpinner
  renderLoginLink (Just Nothing) =
    DOM.div
        { children:
            [ DOM.span
                { className: accountClass <> "-icon"
                , children: [ DOM.span_ [] ]
                }
            , DOM.span
                { children: [ DOM.text "LOGGA IN" ]
                , onClick: props.onLogin
                }
            ]
        , onClick: props.onLogin
        , className: accountClass <> " " <> accountClass <> "--active"
        , _data: Object.fromFoldable [Tuple "login" "1"]
        }
  renderLoginLink (Just (Just user)) =
    let name = fromMaybe "INLOGGAD" $ toMaybe user.firstName
    in DOM.a
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

-- The characteristic line at the top of every KSF media's site
topLine :: JSX
topLine = DOM.hr { className: "mosaico-top-line" }

-- The separator between the header and the rest of the page
mainSeparator :: JSX
mainSeparator = DOM.hr { className: "mosaico-main-separator" }
