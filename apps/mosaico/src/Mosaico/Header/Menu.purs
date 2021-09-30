module Mosaico.Header.Menu where

import Prelude

import Data.Array (concat, cons, foldl, intersperse, length, singleton, snoc, toUnfoldable)
import Data.Foldable
import Data.Foldable as Foldable
import Data.List ((:))
import Data.List as List
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Monoid
import Data.Semigroup
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

data MenuLayout a = Element a
                  | Block (Array (MenuLayout a))

instance semigroupMenuLayout :: (Semigroup a) => Semigroup (MenuLayout a) where
  append (Block as) (Block bs) = Block $ as <> bs
  append e (Block es) = Block $ e `cons` es
  append (Block es) e  = Block $  es `snoc` e
  append a b = Block [ a, b ]

instance foldableMenuLayout :: Foldable MenuLayout where
  foldMap f (Block as) = foldMap (foldMap f) as
  foldMap f (Element a) = f a
  foldr f = foldrDefault f
  foldl f = foldlDefault f

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
    menuLayout :: MenuLayout MenuLayoutElement
    menuLayout =  Block <<< (intersperse separator) $ [ topSections, stardsidan, nyheter, opinion, kultur, sport, bottomSections ]
    separator = Element $ Separator Nothing

    topSections :: MenuLayout MenuLayoutElement
    topSections = Block $ Element <<< Section <$>
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

    stardsidan = Element <<< Section $
                 { title: "STARTSIDAN"
                 , modifier: Just "--startsidan"
                 , url: ""
                 , subsections: []
                 }
    nyheter      = Element <<< Section $
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
    opinion    = Element <<< Section $
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
    kultur    =  Element <<< Section $
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
    sport     =  Element <<< Section $
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

    bottomSections :: MenuLayout MenuLayoutElement
    bottomSections = Block $ Element <<< Section <$>
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
      , children: [ Foldable.foldMap renderMenuLayoutElement menuLayout ]
      }
      where
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
        renderSeparator modifier = DOM.hr { className: unwords [ separatorClass, separatorClass <> (fromMaybe mempty modifier) ] }

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
