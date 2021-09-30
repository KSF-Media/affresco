module Mosaico.Header.Menu where

import Data.Foldable
import Data.Monoid
import Data.Semigroup
import Prelude

import Data.Array (concat, cons, foldl, intersperse, length, singleton, snoc, toUnfoldable)
import Data.Foldable
import Data.Foldable as Foldable
import Data.List ((:))
import Data.List as List
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String.Common (trim)
import Data.Tuple (uncurry)
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import React.Basic (JSX)
import React.Basic.DOM as DOM
import React.Basic.Hooks (Component, component)

type Self =
  { props :: Props
  }

type Props =
  { visible :: Boolean
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
  { title :: String
  , url :: String
  }

menuComponent :: Component Props
menuComponent = do
  component "Menu" \props -> React.do
    pure $ render { props }

visibleMenuComponent :: Effect JSX
visibleMenuComponent = do
  component <- menuComponent
  pure $ component { visible: true }

render :: Self -> JSX
render { props: { visible } } = DOM.div
  { className: menuClass <>
      if visible then
        " " <> visibleMenuClass
      else
        mempty
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
    middleBlock = (intersperse mobileOnlySeparator) $ [ stardsidan, nyheter, opinion, kultur, sport ]

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

    stardsidan = Section $
                 { title: "STARTSIDAN"
                 , modifier: Just "--startsidan"
                 , url: ""
                 , subsections: []
                 }
    nyheter      = Section $
                   { title: "NYHETER"
                   , modifier: Just "--section1"
                   , url: ""
                   , subsections:
                       [ { title: "Finland"
                         , url: ""
                         }
                       , { title: "Huvustadsregionen"
                         , url: ""
                         }
                       , { title: "Norden och världen"
                         , url: ""
                         }
                       , { title: "Ekonomi"
                         , url: ""
                         }
                       , { title: "Miljö och natur"
                         , url: ""
                         }
                       ]
                 }
    opinion    = Section $
                 { title: "OPINION"
                 , modifier: Just "--section2"
                 , url: ""
                 , subsections:
                     [ { title: "Ledare"
                       , url: ""
                       }
                     , { title: "Kolumner"
                       , url: ""
                       }
                     , { title: "Debatt"
                       , url: ""
                       }
                     , { title: "Krönikor"
                       , url: ""
                       }
                     ]
                 }
    kultur    =  Section $
                 { title: "KULTUR"
                 , modifier: Just "--section2"
                 , url: ""
                 , subsections:
                     [ { title: "Litteratur"
                       , url: ""
                       }
                     , { title: "Musik"
                       , url: ""
                       }
                     , { title: "Scen"
                       , url: ""
                       }
                     , { title: "Konst"
                       , url: ""
                       }
                     , { title: "Film och TV"
                       , url: ""
                       }
                     ]
                 }
    sport     =  Section $
                 { title: "SPORT"
                 , modifier: Just "--section3"
                 , url: ""
                 , subsections:
                     [ { title: "Sport"
                       , url: ""
                       }
                     , { title: "Handboll"
                       , url: ""
                       }
                     , { title: "Ishockey"
                       , url: ""
                       }
                     , { title: "Fotboll"
                       , url: ""
                       }
                     , { title: "Motorsport"
                       , url: ""
                       }
                     , { title: "Friidrott"
                       , url: ""
                       }
                     , { title: "Skidsport"
                       , url: ""
                       }
                     ]
                 }

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
                              DOM.div
                                { className: sectionTitleClass
                                , children: [ DOM.text title ]
                                } `cons`
                                  if length subsections > 0 then
                                    [ DOM.div
                                        { className: sectionExpanderClass
                                        , children: [ ]
                                        }
                                    ]
                                  else
                                    []
                          }
                      , DOM.div
                          { className: subsectionsClass
                          , children: renderSubsection <$> subsections
                          }
                      ]
          }

        renderSubsection :: Subsection -> JSX
        renderSubsection { title } = DOM.div
          { className: subsectionClass
          , children: [ DOM.text title ]
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

    sectionExpanderElement = "__section-expander"
    sectionExpanderClass = headerBlock <> sectionExpanderElement

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
