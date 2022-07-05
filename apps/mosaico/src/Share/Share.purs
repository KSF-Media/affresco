module Mosaico.Share
  ( articleShareButtons
  ) where

import Prelude
import Data.Foldable (foldMap)
import Data.Nullable (Nullable, toMaybe)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Uncurried (EffectFn1, runEffectFn1)
import React.Basic (JSX)
import React.Basic.DOM as DOM
import React.Basic.Events (handler_)
import React.Basic.DOM.Events (capture_)

foreign import encodeURIComponent_ :: String -> String

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
    title = encodeURIComponent_ titleStr
    url = encodeURIComponent_ urlStr

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

articleShareButtons :: String -> Maybe String -> JSX
articleShareButtons title maybeShareUrl =
  DOM.ul
    { className: "mosaico-article__some"
    , children: foldMap shareIcons maybeShareUrl
    }
  where
  shareIcons currentUrl =
    [ mkShareIcon Nothing (shareUrl makeFacebookUrl title currentUrl) "facebook"
    , mkShareIcon Nothing (shareUrl makeTwitterUrl title currentUrl) "twitter"
    , mkShareIcon Nothing (shareUrl makeLinkedinUrl title currentUrl) "linkedin"
    , mkShareIcon Nothing (shareUrl makeMailUrl title currentUrl) "mail"
    , case (toMaybe nativeShare) of
        Nothing -> mempty
        Just doShare -> nativeShareIcon currentUrl $ runEffectFn1 doShare
    ]

  nativeShareIcon url doShare = mkShareIcon (Just $ \_ -> doShare { title, url }) "#" "native"
