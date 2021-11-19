module Mosaico.Header.Menu where

import Prelude

import Data.Array (foldl, intersperse, snoc)
import Data.Foldable (foldMap)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Monoid (guard)
import Data.Newtype (unwrap)
import Data.String (toUpper)
import Data.String.Common (trim)
import Data.Tuple (snd)
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Lettera.Models (Category(..), CategoryLabel)
import React.Basic (JSX)
import React.Basic.DOM as DOM
import React.Basic.DOM.Events (capture_)
import React.Basic.Hooks (Component, component)

type Self =
  { props :: Props
  }

type Props =
  { visible :: Boolean
  , categoryStructure :: Array Category
  , onCategoryClick :: CategoryLabel -> String -> Effect Unit
  }

data MenuLayoutElement = Section Section
                       | Separator (Maybe String)
                       -- ^ The String-typed parameter is the BEM modifier to apply to the separator

type MenuBlock = Array MenuLayoutElement

type MenuLayout = Array MenuBlock

type Section =
  { title :: String
  , modifier :: Maybe String
  , url :: String
  , subsections :: Array Subsection
  }

type Subsection =
  { title :: CategoryLabel
  , url :: String
  }

menuComponent :: Component Props
menuComponent = do
  component "Menu" \props -> React.do
    pure $ render { props }

render :: Self -> JSX
render { props } = DOM.div
  { className: menuClass <> guard props.visible (" " <> visibleMenuClass)
  , children: [ menuContent
              , DOM.div
                  { className: menuFooterClass
                  , children:
                      [ DOM.div
                          { className: footerCaptionClass
                          , children: [ DOM.text "ANDRA KSF-TIDNINGAR" ]
                          }
                      , logo vnLogoClass vnLogoImageClass "Västra Nyland"
                      , logo onLogoClass onLogoImageClass "Östnyland"
                      ]
                  }
              ]
  }
  where

    menuLayout :: MenuLayout
    menuLayout = [ upperBlock, separatorBlock, middleBlock, separatorBlock, bottomBlock ]

    upperBlock :: MenuBlock
    upperBlock = topSections

    middleBlock :: MenuBlock
    middleBlock = (intersperse mobileOnlySeparator) $ snd $ foldl mkSection (0 /\ []) props.categoryStructure

    bottomBlock :: MenuBlock
    bottomBlock = bottomSections

    separatorBlock :: MenuBlock
    separatorBlock = [ separator ]

    separator = Separator Nothing
    mobileOnlySeparator = Separator $ Just mobileOnlySeparatorClass

    topSections :: MenuBlock
    topSections = Section <$>
                  [ { title: "SÖK"
                    , modifier: Nothing
                    , url: ""
                    , subsections: []
                    }
                  , { title: "E-TIDNINGEN"
                    , modifier: Nothing
                    , url: ""
                    , subsections: []
                    }
                  , { title: "KUNDSERVICE"
                    , modifier: Nothing
                    , url: ""
                    , subsections: []
                    }
                  ]

    mkSection (n /\ acc) (Category c) =
      let mkSubsection (Category subc) =
            { title: subc.label, url: "/" <> show subc.label }
          section =
            Section $
              { title: toUpper $ unwrap c.label
              , modifier: Just $ "--section" <> show n
              , url: "/" <> show c.label
              , subsections: map mkSubsection c.subCategories
              }
      in ((n + 1) /\ acc `snoc` section)

    bottomSections :: MenuBlock
    bottomSections = Section <$>
                  [ { title: "KONTAKTA OSS"
                    , modifier: Nothing
                    , url: ""
                    , subsections: []
                    }
                  , { title: "ANNONSERA"
                    , modifier: Nothing
                    , url: ""
                    , subsections: []
                    }
                  , { title: "JOBBA HOS OSS"
                    , modifier: Nothing
                    , url: ""
                    , subsections: []
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
        renderSection { modifier, subsections, title } = DOM.div
          { className: unwords [ sectionClass, sectionClass <> fromMaybe mempty modifier ]
          , children: [ DOM.div
                          { className: sectionHeaderClass
                          , children:
                              [ DOM.div
                                 { className: sectionTitleClass
                                 , children: [ DOM.text title ]
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
        renderSubsection { title, url } = DOM.div
          { className: subsectionClass
          , children:
              [ DOM.a
                  { href: url
                  , children: [ DOM.text $ unwrap title ]
                  , onClick: capture_ $ props.onCategoryClick title url
                  }
              ]
          }

        renderSeparator :: Maybe String -> JSX
        renderSeparator modifierClass = DOM.hr { className: unwords [ separatorClass, fromMaybe mempty modifierClass ] }

    logo :: String -> String ->  String -> JSX
    logo modifierClass imageModifierClass caption = DOM.div
      { className: unwords [ logoClass, modifierClass ]
      , children:
          [ DOM.div
              { className: unwords [ logoImageClass, imageModifierClass ]
              }
          , DOM.div
              { className: unwords [ logoCaptionClass ]
              , children: [ DOM.text caption ]
              }
          ]
      }

    headerBlock = "mosaico-header"

    menuElement = "__menu"
    visibleModifier = "--visible"
    menuClass = headerBlock <> menuElement
    visibleMenuClass = menuClass <> visibleModifier

    menuContentElement = "__menu-content"
    menuContentClass = headerBlock <> menuContentElement

    menuFooterElement = "__menu-footer"
    menuFooterClass = headerBlock <> menuFooterElement

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

    footerCaptionElement = "__footer-caption"
    footerCaptionClass = headerBlock <> footerCaptionElement

    logoElement =  "__footer-logo"
    onLogoModifier = "--on"
    vnLogoModifier = "--vn"
    logoClass = headerBlock <> logoElement
    onLogoClass = logoClass <> onLogoModifier
    vnLogoClass = logoClass <> vnLogoModifier

    logoImageElement = "__footer-logo-image"
    logoImageClass = headerBlock <> logoImageElement
    onLogoImageClass = logoImageClass <> onLogoModifier
    vnLogoImageClass = logoImageClass <> vnLogoModifier

    logoCaptionElement = "__footer-logo-caption"
    logoCaptionClass = headerBlock <> logoCaptionElement

unwords :: Array String -> String
unwords = trim <<< foldl (\a w -> a <> " " <> w) mempty
