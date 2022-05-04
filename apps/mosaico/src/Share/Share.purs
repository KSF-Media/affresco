module Mosaico.Share
  ( articleShareButtons
  ) where

import Prelude
import Data.Nullable (Nullable, toMaybe)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Uncurried (EffectFn1, runEffectFn1)
import React.Basic (JSX)
import React.Basic.DOM as DOM
import React.Basic.Events (handler_)
import React.Basic.DOM.Events (capture_)

foreign import encodeURIComponent :: String -> String

type NativeShareProps
  = { title :: String
    , url :: String
    }

foreign import nativeShare :: Nullable (EffectFn1 NativeShareProps Unit)

makeFacebookUrl :: String -> String -> String
makeFacebookUrl title url = "http://www.facebook.com/sharer.php?title=" <> title <> "&u=" <> url

makeTwitterUrl :: String -> String -> String
makeTwitterUrl title url = "https://twitter.com/intent/tweet?text=" <> title <> "&url=" <> url

makeLinkedinUrl :: String -> String -> String
makeLinkedinUrl title url = "http://www.linkedin.com/shareArticle?mini=true&title=" <> title <> "&url=" <> url

makeMailUrl :: String -> String -> String
makeMailUrl title url = "mailto:?subject=" <> title <> "&body=" <> url

shareUrl :: (String -> String -> String) -> String -> String -> String
shareUrl urlTemplate titleStr urlStr = urlTemplate title url
  where
    title = encodeURIComponent titleStr
    url = encodeURIComponent urlStr

mkShareIcon :: Maybe (Unit -> Effect Unit) -> String -> String -> JSX
mkShareIcon onClick href someName =
  DOM.li_
    [ DOM.a
        { href
        , children: [ DOM.span {} ]
        , onClick:
            ( case onClick of
                Just e -> capture_ $ e unit
                Nothing -> handler_ $ pure unit
            )
        , className: "mosaico-article__some--" <> someName
        }
    ]

articleShareButtons :: String -> String -> JSX
articleShareButtons title currentUrl =
  DOM.ul
    { className: "mosaico-article__some"
    , children: if currentUrl /= "" then shareIcons else mempty
    }
  where
  shareIcons =
    [ mkShareIcon Nothing (shareUrl makeFacebookUrl title currentUrl) "facebook"
    , mkShareIcon Nothing (shareUrl makeTwitterUrl title currentUrl) "twitter"
    , mkShareIcon Nothing (shareUrl makeLinkedinUrl title currentUrl) "linkedin"
    , mkShareIcon Nothing (shareUrl makeMailUrl title currentUrl) "mail"
    , case (toMaybe nativeShare) of
        Nothing -> mempty
        Just doShare -> nativeShareIcon $ runEffectFn1 doShare
    ]

  nativeShareIcon doShare = mkShareIcon (Just $ \_ -> doShare { title, url: currentUrl }) "#" "native"
