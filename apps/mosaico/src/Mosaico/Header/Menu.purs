module Mosaico.Header.Menu where

import Prelude

import Data.Array (catMaybes, foldl, intersperse, snoc)
import Effect (Effect)
import Data.Foldable (foldMap)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Newtype (unwrap)
import Data.String as String
import Data.String (toUpper)
import Data.String.Common (trim)
import KSF.Paper (Paper(..), toString)
import KSF.Spinner (loadingSpinner)
import KSF.User (User)
import Lettera.Models (Category(..), CategoryLabel)
import Mosaico.Paper (mosaicoPaper)
import React.Basic (JSX)
import React.Basic.Events (EventHandler)
import React.Basic.DOM as DOM
import React.Basic.DOM.Events (capture_)

type Props =
  { changeRoute :: String -> Effect Unit
  , categoryStructure :: Array Category
  , onCategoryClick :: Category -> EventHandler
    -- Nothing for loading state, Just Nothing for no user
  , user :: Maybe (Maybe User)
  , onLogin :: EventHandler
  , onLogout :: EventHandler
  }

data MenuLayoutElement = Section Section
                       | Loading
                       | Separator (Maybe String)
                       -- ^ The String-typed parameter is the BEM modifier to apply to the separator

type MenuBlock = Array MenuLayoutElement

type MenuLayout = Array MenuBlock

type Section =
  { title :: String
  , subsections :: Array Subsection
  , url :: String
  , onClick :: EventHandler
  , addClass :: Maybe String
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
    upperBlock = topSections `snoc`
                 case props.user of
                   Nothing -> Loading
                   Just (Just _) -> Section
                                    { title: "LOGGA UT"
                                    , subsections: []
                                    , url: ""
                                    , onClick: onLogout
                                    , addClass: Just "mosaico-menu__icon mosaico-menu__icon--logout"
                                    }
                   Just Nothing ->  Section
                                    { title: "LOGGA IN"
                                    , subsections: []
                                    , url: ""
                                    , onClick: onLogin
                                    , addClass: Just "mosaico-menu__icon mosaico-menu__icon--login"
                                    }

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
                    { title: "PRENUMERERA"
                    , subsections: []
                    , url: "https://prenumerera.ksfmedia.fi/#/" <> String.toLower (toString mosaicoPaper)
                    , onClick: mempty
                    , addClass: Just "mosaico-menu__subscribe-link"
                    }
                  , Just
                    { title: "SÖK"
                    , subsections: []
                    , url: "/sök"
                    , onClick: capture_ $ props.changeRoute "/sök"
                    , addClass: Just "mosaico-menu__icon mosaico-menu__icon--search"
                    }
                  , Just
                    { title: "E-TIDNINGEN"
                    , subsections: []
                    , url: "/epaper"
                    , onClick: capture_ $ props.changeRoute "/epaper/"
                    , addClass: Just "mosaico-menu__icon mosaico-menu__icon--epaper"
                    }
                  , Just
                    { title: "KUNDSERVICE"
                    , subsections: []
                    , url: "/sida/kundservice"
                    , onClick: capture_ $ props.changeRoute "/sida/kundservice"
                    , addClass: Just "mosaico-menu__icon mosaico-menu__icon--customer-service"
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
              , addClass: Just "mosaico-menu__category-headline"
              }
      in acc `snoc` section

    bottomSections :: MenuBlock
    bottomSections = Section <$>
                  [ { title: "KONTAKTA OSS"
                    , subsections: []
                    , url: "/sida/kontakt"
                    , onClick: capture_ $ props.changeRoute "/sida/kontakt"
                    , addClass: mempty
                    }
                  , { title: "ANNONSERA"
                    , subsections: []
                    , url: "https://www.ksfmedia.fi/"
                    , onClick: mempty
                    , addClass: mempty
                    }
                  , { title: "JOBBA HOS OSS"
                    , subsections: []
                    , url: "https://www.ksfmedia.fi/jobba-hos-oss"
                    , onClick: mempty
                    , addClass: mempty
                    }
                  ] <> paperSpecificLinks mosaicoPaper

    paperSpecificLinks :: Paper -> Array Section
    paperSpecificLinks VN = vastranylandMenuLinks
    paperSpecificLinks _ = mempty

    vastranylandMenuLinks :: Array Section
    vastranylandMenuLinks =
      [ { title: "ANSLAGSTAVLAN"
        , subsections: []
        , url: "/sida/anslagstavlan"
        , onClick: capture_ $ props.changeRoute "/sida/anslagstavlan"
        , addClass: Just "mosaico-menu__link-headline"
        }
      , { title: "FISKECUPEN"
        , subsections: []
        , url: "/sida/fiskecupen"
        , onClick: capture_ $ props.changeRoute "/sida/fiskecupen"
        , addClass: Just "mosaico-menu__link-headline"

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
        renderMenuLayoutElement Loading = DOM.div
          { className: unwords [ sectionClass ]
          , children:
              [ DOM.div
                  { className : sectionHeaderClass
                  , children: [ loadingSpinner ]
                  }
              ]
          }
        renderMenuLayoutElement (Section section) = renderSection section
        renderMenuLayoutElement (Separator modifier) = renderSeparator modifier

        renderSection :: Section -> JSX
        renderSection { subsections, title, url, onClick, addClass } = DOM.div
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
                                          , className: fromMaybe mempty addClass
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
