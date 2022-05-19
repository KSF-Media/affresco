module Mosaico.Article.Advertorial.Standard where

import Prelude

import Data.Maybe (Maybe (..))
import Lettera.Models (Article, Image)
import Mosaico.Article.Advertorial.Basic as Basic
import Mosaico.Article.Box as Box
import Mosaico.Article.Image as Image
import React.Basic (JSX)
import React.Basic.Hooks (Component)
import React.Basic.Hooks as React

type Props = { article :: Article }

component :: Component Props
component = do
  imageComponent <- Image.component
  boxComponent <- Box.component
  React.component "Standard" $ \props -> React.do
    pure $ render imageComponent boxComponent props

render :: (Image.Props -> JSX) -> (Box.Props -> JSX) -> Props -> JSX
render imageComponent boxComponent { article } =
  Basic.render imageComponent boxComponent
    { article
    , imageProps: Just defaultImageProps
    , advertorialClassName: Just "advertorial-standard"
    }

defaultImageProps :: Image -> Image.Props
defaultImageProps =
   { clickable: true
   , main: true
   , params: Nothing
   , image: _
   , fullWidth: true
   }
