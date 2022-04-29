module Mosaico.Header.Menu where

import Prelude

import Data.Array (catMaybes, foldl, intersperse, snoc)
import Data.Foldable (foldMap)
import Data.Maybe (Maybe(..), fromMaybe, isNothing)
import Data.Monoid (guard)
import Data.Newtype (unwrap)
import Data.String (toUpper)
import Data.String.Common (trim)
import KSF.User (User)
import Lettera.Models (Category(..), CategoryLabel)
import React.Basic (JSX)
import React.Basic.Events (EventHandler)
import React.Basic.DOM as DOM
import React.Basic.DOM.Events (capture_)
import Routing.PushState (PushStateInterface)
import Simple.JSON (write)

type Props =
  { router :: PushStateInterface
  , categoryStructure :: Array Category
  , onCategoryClick :: Category -> EventHandler
  , user :: Maybe User
  , onLogin :: EventHandler
  , onLogout :: EventHandler
  }

data MenuLayoutElement = Section Section
                       | Separator (Maybe String)
                       -- ^ The String-typed parameter is the BEM modifier to apply to the separator

type MenuBlock = Array MenuLayoutElement

type MenuLayout = Array MenuBlock

type Section =
  { title :: String
  , subsections :: Array Subsection
  , url :: String
  , onClick :: EventHandler
  , iconClass :: Maybe String
  }

type Subsection =
  { title :: CategoryLabel
  , onClick :: EventHandler
  }

render :: Props -> JSX
render props@{ onLogin, onLogout } = DOM.div
  { className: menuClass
  , children: [ menuContent ]
  }
  where

    menuLayout :: MenuLayout
    menuLayout = [ upperBlock, separatorBlock, middleBlock, separatorBlock, bottomBlock ]

    upperBlock :: MenuBlock
    upperBlock = topSections

    middleBlock :: MenuBlock
    middleBlock = (intersperse mobileOnlySeparator) $ foldl mkSection [] props.categoryStructure

    bottomBlock :: MenuBlock
    bottomBlock = bottomSections

    separatorBlock :: MenuBlock
    separatorBlock = [ separator ]

    separator = Separator Nothing
    mobileOnlySeparator = Separator $ Just mobileOnlySeparatorClass

    topSections :: MenuBlock
    topSections = Section <$> catMaybes
                  [ Just
                    { title: "SÖK"
                    , subsections: []
                    , url: "/sök"
                    , onClick: capture_ $ props.router.pushState (write {}) "/sök"
                    , iconClass: Just "mosaico-menu__icon mosaico-menu__icon--search"
                    }
                  , Just
                    { title: "E-TIDNINGEN"
                    , subsections: []
                    , url: "/epaper"
                    , onClick: capture_ $ props.router.pushState (write {}) "/epaper"
                    , iconClass: Just "mosaico-menu__icon mosaico-menu__icon--epaper"
                    }
                  , Just
                    { title: "KUNDSERVICE"
                    , subsections: []
                    , url: "/sida/kundservice"
                    , onClick: capture_ $ props.router.pushState (write {}) "/sida/kundservice"
                    , iconClass: Just "mosaico-menu__icon mosaico-menu__icon--customer-service"
                    }
                  , props.user *>
                    Just
                    { title: "LOGGA UT"
                    , subsections: []
                    , url: ""
                    , onClick: onLogout
                    , iconClass: Just "mosaico-menu__icon mosaico-menu__icon--account"
                    }
                  , guard (isNothing props.user) $
                    Just
                    { title: "LOGGA IN"
                    , subsections: []
                    , url: ""
                    , onClick: onLogin
                    , iconClass: mempty
                    }
                  ]

    mkSection acc category@(Category c) =
      let mkSubsection subCategory@(Category { label }) =
            { title: label
            , onClick: props.onCategoryClick subCategory
            }
          section =
            Section $
              { title: toUpper $ unwrap c.label
              , subsections: map mkSubsection c.subCategories
              , url: "/" <> show c.label
              , onClick: props.onCategoryClick category
              , iconClass: mempty
              }
      in acc `snoc` section

    bottomSections :: MenuBlock
    bottomSections = Section <$>
                  [ { title: "KONTAKTA OSS"
                    , subsections: []
                    , url: "/sida/kontakt"
                    , onClick: capture_ $ props.router.pushState (write {}) "/sida/kontakt"
                    , iconClass: mempty
                    }
                  , { title: "ANNONSERA"
                    , subsections: []
                    , url: "https://www.ksfmedia.fi/"
                    , onClick: mempty
                    , iconClass: mempty
                    }
                  , { title: "JOBBA HOS OSS"
                    , subsections: []
                    , url: ""
                    , onClick: mempty
                    , iconClass: mempty
                    }
                  ]

    menuContent :: JSX
    menuContent = DOM.div
      { className: menuContentClass
      , children: [ renderMenuLayout menuLayout ]
      }
      where
        renderMenuLayout :: MenuLayout -> JSX
        renderMenuLayout layout = foldMap renderMenuBlock layout

        renderMenuBlock :: MenuBlock -> JSX
        renderMenuBlock block = DOM.div
          { className: blockClass
          , children: [ foldMap renderMenuLayoutElement block ]
          }

        renderMenuLayoutElement :: MenuLayoutElement -> JSX
        renderMenuLayoutElement (Section section) = renderSection section
        renderMenuLayoutElement (Separator modifier) = renderSeparator modifier

        renderSection :: Section -> JSX
        renderSection { subsections, title, url, onClick, iconClass } = DOM.div
          { className: unwords [ sectionClass ]
          , children: [ DOM.div
                          { className: sectionHeaderClass
                          , children:
                              [ DOM.div
                                  { className: sectionTitleClass
                                  , children:
                                      [ DOM.a
                                          { href: url
                                          , children: [ DOM.text title ]
                                          , onClick
                                          , className: fromMaybe mempty iconClass
                                          }
                                      ]
                                  }
                              ]
                          }
                      , DOM.div
                          { className: subsectionsClass
                          , children: renderSubsection <$> subsections
                          }
                      ]
          }

        renderSubsection :: Subsection -> JSX
        renderSubsection { title, onClick } = DOM.div
          { className: subsectionClass
          , children:
              [ DOM.a
                  { href: "/" <> show title
                  , children: [ DOM.text $ unwrap title ]
                  , onClick
                  }
              ]
          }

        renderSeparator :: Maybe String -> JSX
        renderSeparator modifierClass = DOM.hr { className: unwords [ separatorClass, fromMaybe mempty modifierClass ] }

    headerBlock = "mosaico-header"

    menuElement = "__menu"
    menuClass = headerBlock <> menuElement

    menuContentElement = "__menu-content"
    menuContentClass = headerBlock <> menuContentElement

    blockElement = "__block"
    blockClass = headerBlock <> blockElement

    sectionElement = "__section"
    sectionClass = headerBlock <> sectionElement

    sectionHeaderElement = "__section-header"
    sectionHeaderClass = headerBlock <> sectionHeaderElement

    sectionTitleElement = "__section-title"
    sectionTitleClass = headerBlock <> sectionTitleElement

    subsectionsElement = "__subsections"
    subsectionsClass = headerBlock <> subsectionsElement

    subsectionElement = "__subsection"
    subsectionClass = headerBlock <> subsectionElement

    separatorElement = "__separator"
    separatorClass = headerBlock <> separatorElement

    mobileOnlyModifier = "--mobile-only"
    mobileOnlySeparatorClass = separatorClass <> mobileOnlyModifier

unwords :: Array String -> String
unwords = trim <<< foldl (\a w -> a <> " " <> w) mempty
