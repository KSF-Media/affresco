module Mosaico.Article.Advertorial.Standard where

import Data.Maybe (Maybe (..))
import           Lettera.Models                    (Article, Image)
import           Mosaico.Article.Advertorial.Basic as Basic
import           Mosaico.Article.Image             as Image
import           React.Basic.Hooks                 (JSX)

type Props =
  { article :: Article
  , currentUrl :: String
  }

render :: (Image.Props -> JSX) -> Props -> JSX
render imageComponent { article, currentUrl } =
  Basic.render imageComponent
    { article
    , imageProps: Just defaultImageProps
    , advertorialClassName: Just "advertorial-standard"
    , currentUrl
    }

defaultImageProps :: Image -> Image.Props
defaultImageProps =
   { clickable: true
   , main: true
   , params: Nothing
   , image: _
   , fullWidth: true
   }
