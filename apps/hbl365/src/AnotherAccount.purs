module HBL365.AnotherAccount where

import Prelude

import Effect.Aff as Aff
import KSF.User as User
import React.Basic (JSX)
import React.Basic.DOM as DOM
import React.Basic.DOM.Events (preventDefault)
import React.Basic.Events (handler)
import Web.HTML (window)
import Web.HTML.Location (setHref)
import Web.HTML.Window (location)

anotherAccount :: JSX
anotherAccount =
  DOM.div_
    [ DOM.text "Köp för ett annat konto? "
    , DOM.a
        { href: "/"
        , children: [ DOM.text "Logga ut." ]
        , onClick: handler preventDefault $ const $
            Aff.launchAff_ $ User.logout $ const $
            window >>= location >>= setHref "/"
        }
    ]
