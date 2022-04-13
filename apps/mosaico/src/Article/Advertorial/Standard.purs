module Mosaico.Article.Advertorial.Standard where

import Data.Maybe (Maybe (..))
import           Lettera.Models                    (Article, Image)
import           Mosaico.Article.Advertorial.Basic as Basic
import           Mosaico.Article.Image             as Image
import           React.Basic.Hooks                 (JSX)

type Props = { article :: Article }

render :: (Image.Props -> JSX) -> Props -> JSX
render imageComponent { article } =
  Basic.render imageComponent
    { article
    , imageProps: Just defaultImageProps
    , advertorialClassName: Just "advertorial-standard"
    }

defaultImageProps :: Image -> Image.Props
defaultImageProps =
   { clickable: true
   , main: true
   , params: Just "&width=960&height=540&q=90"
   , image: _
   , fullWidth: true
   }
