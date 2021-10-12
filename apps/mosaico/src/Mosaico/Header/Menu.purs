module Mosaico.Header.Menu where

import Prelude

import Data.Array (concat, foldl, singleton, toUnfoldable)
import Data.String.Common (trim)
import Data.List ((:))
import Data.List as List
import Data.Tuple (uncurry)
import Data.Tuple.Nested ((/\))
import React.Basic (JSX)
import React.Basic.DOM as DOM
import React.Basic.Hooks (Component, component)

type Self =
  { props :: Props
  }

type Props =
  { visible :: Boolean
  }

type MenuLayout = Array MenuLayoutElement

data MenuLayoutElement = SectionElement Section
                       | SeparatorElement String
                       -- ^ The String-typed parameter is the BEM modifier to apply to the separator

type Section =
  { title :: String
  , modifier :: String
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

render :: Self -> JSX
render { props: { visible } } = DOM.div
  { className: menuClass <>
      if visible then
        " " <> visibleMenuClass
      else
        mempty
  , children: [ renderMenuLayout
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
    menuLayout =  concat $ (((<$>) SectionElement) <$> [ topSections, middleSections, bottomSections ]) `merge` ((singleton <<< SeparatorElement) <$> [ "--top", "--center", "--bottom" ])

    topSections = [ { title: "E-TIDNINGEN"
                    , modifier: "--e-tidningen"
                    , url: ""
                    , subsections: []
                    }
                  , { title: "KUNDSERVICE"
                    , modifier: "--kundservice"
                    , url: ""
                    , subsections: []
                    }
                  , { title: "ANNONSERA"
                    , modifier: "--annonsera"
                    , url: ""
                    , subsections: []
                    }
                  , { title: "OTHER IMPORTANT"
                    , modifier: "--other-important"
                    , url: ""
                    , subsections: []
                    }
                  ]

    middleSections = [ { title: "STARTSIDAN"
                       , modifier: "--startsidan"
                       , url: ""
                       , subsections: []
                       }
                     , { title: "SECTION 1"
                       , modifier: "--section1"
                       , url: ""
                       , subsections:
                           [ { title: "Consectetur"
                             , url: ""
                             }
                           , { title: "Ultrices"
                             , url: ""
                             }
                           , { title: "Tempor"
                             , url: ""
                             }
                           , { title: "Mollis"
                             , url: ""
                             }
                           , { title: "Consectetur"
                             , url: ""
                             }
                           , { title: "Lorem ipsum"
                             , url: ""
                             }
                           ]
                       }
                     , { title: "SECTION 2"
                       , modifier: "--section2"
                       , url: ""
                       , subsections:
                           [ { title: "Lorem"
                             , url: ""
                             }
                           , { title: "Pellentesque"
                             , url: ""
                             }
                           , { title: "Sollicitudin"
                             , url: ""
                             }
                           , { title: "Ultrices"
                             , url: ""
                             }
                           , { title: "Consectetur"
                             , url: ""
                             }
                           ]
                         }
                     , { title: "SECTION 3"
                       , modifier: "--section3"
                       , url: ""
                       , subsections:
                           [ { title: "Consectetur"
                             , url: ""
                             }
                           , { title: "Mollis"
                             , url: ""
                             }
                           , { title: "Tempor"
                             , url: ""
                             }
                           , { title: "Ultrices"
                             , url: ""
                             }
                           ]
                         }
                     , { title: "SECTION 4"
                       , modifier: "--section4"
                       , url: ""
                       , subsections:
                           [ { title: "Lorem"
                             , url: ""
                             }
                           , { title: "Pellentesque"
                             , url: ""
                             }
                           , { title: "Tempor"
                             , url: ""
                             }
                           , { title: "Consectetur"
                             , url: ""
                             }
                           , { title: "Elit"
                             , url: ""
                             }
                           ]
                         }
                     ]

    bottomSections = [ { title: "KONTAKT"
                       , modifier: "--kontakt"
                       , url: ""
                       , subsections:
                           [ { title: "Lorem"
                             , url: ""
                             }
                           , { title: "Pellentesque"
                             , url: ""
                             }
                           , { title: "Aliquet"
                             , url: ""
                             }
                           , { title: "Ultrices"
                             , url: ""
                             }
                           , { title: "Consectetur"
                             , url: ""
                             }
                           ]
                        }
                     , { title: "ANNONSERA"
                       , modifier: "--annonsera2"
                       , url: ""
                       , subsections:
                           [ { title: "Consectetur"
                             , url: ""
                             }
                           , { title: "Ultrices"
                             , url: ""
                             }
                           , { title: "Tempor"
                             , url: ""
                             }
                           ]
                        }
                     , { title: "KUNDSERVICE"
                       , modifier: "--kundservice2"
                       , url: ""
                       , subsections:
                           [ { title: "Lorem"
                             , url: ""
                             }
                           , { title: "Pellentesque"
                             , url: ""
                             }
                           , { title: "Sollicitudin"
                             , url: ""
                             }
                           , { title: "Ultrices"
                             , url: ""
                             }
                           ]
                        }
                     , { title: "OTHER"
                       , modifier: "--other"
                       , url: ""
                       , subsections:
                           [ { title: "Lorem"
                             , url: ""
                             }
                           , { title: "Sollicitudin"
                             , url: ""
                             }
                           , { title: "Ultrices"
                             , url: ""
                             }
                           ]
                        }
                     ]

    renderMenuLayout :: JSX
    renderMenuLayout = DOM.div
      { className: menuContentClass
      , children: renderMenuLayoutElement <$> menuLayout
      }
      where
        renderMenuLayoutElement :: MenuLayoutElement -> JSX
        renderMenuLayoutElement (SectionElement section) = renderSection section
        renderMenuLayoutElement (SeparatorElement modifier) = renderSeparator modifier

        renderSection :: Section -> JSX
        renderSection { modifier, subsections, title } = DOM.div
          { className: unwords [ sectionClass, sectionClass <> modifier ]
          , children: [ DOM.div { className: sectionTitleClass
                                , children: [ DOM.text title ]
                                }
                      ] <> (renderSubsection <$> subsections)
          }

        renderSubsection :: Subsection -> JSX
        renderSubsection { title } = DOM.div
          { className: subsectionClass
          , children: [ DOM.text title ]
          }

        renderSeparator :: String -> JSX
        renderSeparator modifier = DOM.hr { className: unwords [ separatorClass, separatorClass <> modifier ] }

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

    searchElement = "__search"
    _searchClass = headerBlock <> searchElement

    menuContentElement = "__menu-content"
    menuContentClass = headerBlock <> menuContentElement

    menuFooterElement = "__menu-footer"
    menuFooterClass = headerBlock <> menuFooterElement

    sectionElement = "__section"
    sectionClass = headerBlock <> sectionElement

    sectionTitleElement = "__section-title"
    sectionTitleClass = headerBlock <> sectionTitleElement

    subsectionElement = "__subsection"
    subsectionClass = headerBlock <> subsectionElement

    separatorElement = "__separator"
    separatorClass = headerBlock <> separatorElement

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

merge :: forall a. Array a -> Array a -> Array a
merge a b =
  List.toUnfoldable <<< uncurry mergeList $ toUnfoldable a /\ toUnfoldable b
  where
    mergeList :: forall b. List.List b -> List.List b -> List.List b
    mergeList xs List.Nil = xs
    mergeList List.Nil ys = ys
    mergeList (List.Cons x xs) (List.Cons y ys) = x : y : mergeList xs ys

unwords :: Array String -> String
unwords = trim <<< foldl (\a w -> a <> " " <> w) mempty
