module Mosaico.Header.Menu where

import Prelude

import Data.Array (catMaybes, foldl, intersperse, snoc)
import Data.Foldable (foldMap)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Newtype (unwrap)
import Data.String (toUpper)
import Data.String.Common (trim)
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)
import KSF.User (User, logout)
import Lettera.Models (Category(..), CategoryLabel)
import React.Basic (JSX)
import React.Basic.Events (EventHandler, handler_)
import React.Basic.DOM as DOM

type Props =
  { categoryStructure :: Array Category
  , onCategoryClick :: CategoryLabel -> EventHandler
  , user :: Maybe User
  , onLogout :: Effect Unit
  }

data MenuLayoutElement = Section Section
                       | Separator (Maybe String)
                       -- ^ The String-typed parameter is the BEM modifier to apply to the separator

type MenuBlock = Array MenuLayoutElement

type MenuLayout = Array MenuBlock

type Section =
  { title :: String
  , subsections :: Array Subsection
  , onClick :: Maybe (Effect Unit)
  }

type Subsection =
  { title :: CategoryLabel
  }

render :: Props -> JSX
render props@{ onLogout } = DOM.div
  { className: menuClass
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
                    , onClick: Nothing
                    }
                  , Just
                    { title: "E-TIDNINGEN"
                    , subsections: []
                    , onClick: Nothing
                    }
                  , Just
                    { title: "KUNDSERVICE"
                    , subsections: []
                    , onClick: Nothing
                    }
                  , props.user *>
                    Just
                    { title: "LOGGA UT"
                    , subsections: []
                    , onClick: Just $ launchAff_ do
                      logout $ const $ pure unit
                      liftEffect onLogout
                    }
                  ]

    mkSection acc (Category c) =
      let mkSubsection (Category subc) =
            { title: subc.label }
          section =
            Section $
              { title: toUpper $ unwrap c.label
              , subsections: map mkSubsection c.subCategories
              , onClick: Nothing
              }
      in acc `snoc` section

    bottomSections :: MenuBlock
    bottomSections = Section <$>
                  [ { title: "KONTAKTA OSS"
                    , subsections: []
                    , onClick: Nothing
                    }
                  , { title: "ANNONSERA"
                    , subsections: []
                    , onClick: Nothing
                    }
                  , { title: "JOBBA HOS OSS"
                    , subsections: []
                    , onClick: Nothing
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
        renderSection section@{ subsections, title } = DOM.div
          { className: unwords [ sectionClass ]
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
          , onClick: foldMap handler_ section.onClick
          }

        renderSubsection :: Subsection -> JSX
        renderSubsection { title } = DOM.div
          { className: subsectionClass
          , children:
              [ DOM.a
                  { href: "/" <> show title
                  , children: [ DOM.text $ unwrap title ]
                  , onClick: props.onCategoryClick title
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
    menuClass = headerBlock <> menuElement

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
