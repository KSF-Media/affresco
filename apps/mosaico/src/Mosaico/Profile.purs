module Mosaico.Profile where

import Prelude

import Data.Maybe (Maybe(..), maybe)
import Data.Nullable (toMaybe)
import KSF.User (User)
import React.Basic (JSX, fragment)
import React.Basic.DOM as DOM
import React.Basic.Events (EventHandler)

type Props =
  { user :: Maybe User
  , onLogin :: EventHandler
  , onLogout :: EventHandler
  , onStaticPageClick :: String -> EventHandler
  }

render :: Props -> JSX
render props@{ onLogin, onLogout, onStaticPageClick } =
  DOM.div
    { className: "mosaico-profile"
    , children:
        [ DOM.div
            { className: "mosaico-profile--header"
            , children: [ DOM.text "MIN PROFIL" ]
            }
        , maybe renderMissing renderUser props.user
        , DOM.div
            { className: "mosaico-profile--help"
            , children:
                [ DOM.text "Behöver du hjälp? "
                , DOM.a
                    { href: "/sida/kundservice"
                    , onClick: onStaticPageClick "kundservice"
                    , children: [ DOM.text "Kontakta kundservice" ]
                    }
                , DOM.text "."
                ]
            }
        ]
    }
  where
    renderMissing = DOM.div
      { className: "mosaico-profile--actions"
      , children:
          [ DOM.a
              { children: [ DOM.span { className: "glyphicon glyphicon-log-in" }
                          , DOM.text "Logga in"
                          ]
              , onClick: onLogin
              }
          ]
      }

    renderUser user = fragment
      [ DOM.div
          { className: "mosaico-profile--data"
          , children:
              [ DOM.div { className: "mosaico-profile--name", children: [ name ] }
              , DOM.div_ [ DOM.text user.email ]
              , DOM.p_ [ DOM.text "I Mitt konto sköter du de flesta av dina prenumerationsärenden. Här gör du smidigast till exempel adressändringar och uppehåll." ]
              ]
          }
      , DOM.div
          { className: "mosaico-profile--actions"
          , children:
              [ DOM.div_
                  [ DOM.a
                      { href: "https://konto.ksfmedia.fi/"
                      , children: [ DOM.span { className: "glyphicon glyphicon-edit" }
                                  , DOM.text "Gå till Mitt konto" ]
                      }
                  ]
              -- TODO: Open Google consent popup
              , DOM.div_
                  [ DOM.a
                      { children: [ DOM.span { className: "glyphicon glyphicon-edit" }
                                  , DOM.text "Hantera dataskydd" ]
                      }
                  ]
              , DOM.div_
                  [ DOM.a
                      { children: [ DOM.span { className: "glyphicon glyphicon-log-out" }
                                  , DOM.text "Logga ut"
                                  ]
                      , onClick: onLogout
                      }
                  ]
              ]
          }
      ]
      where
        name =
          case { firstName: toMaybe user.firstName, lastName: toMaybe user.lastName } of
            { firstName: Nothing, lastName: Nothing } -> DOM.i_ [ DOM.text "Inget namn registrerat" ]
            { firstName: Nothing, lastName: Just n } -> DOM.text n
            { firstName: Just n, lastName: Nothing } -> DOM.text n
            { firstName: Just fname, lastName: Just lname } -> DOM.text $ fname <> " " <> lname
