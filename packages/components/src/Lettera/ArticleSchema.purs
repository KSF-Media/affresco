module Lettera.ArticleSchema
  ( renderAsJsonLd
  ) where

import Prelude
import Data.Maybe (Maybe(..), maybe)
import Data.Argonaut.Core (Json)
import Data.Argonaut.Encode.Class (encodeJson)
import Lettera.Models (Article)
import KSF.LocalDateTime (formatLocalDateTime)

type JsonLdArticle
  = { "@context" :: String
    , "@type" :: String
    , "headline" :: String
    , "image" :: Array String
    , "datePublished" :: Maybe String
    , "dateModified" :: Maybe String
    , "author" ::
        Array
          { "@type" :: String
          , "name" :: String
          , "url" :: Maybe String
          }
    }

articleToJsonLd :: Article -> JsonLdArticle
articleToJsonLd article =
  { "@context": "https://schema.org"
  , "@type": "NewsArticle"
  , "headline": article.title
  , "image": maybe [] (\img -> [ img.url ]) article.mainImage
  , "datePublished": map formatLocalDateTime article.publishingTime
  , "dateModified": map formatLocalDateTime article.updateTime
  , "author":
      map
        ( \author ->
            { "@type": "Person"
            , "name": author.byline
            , "url": Nothing -- Todo: link to a profile page or something here
            }
        )
        article.authors
  }

renderAsJsonLd :: Article -> Json
renderAsJsonLd article = encodeJson $ articleToJsonLd article
