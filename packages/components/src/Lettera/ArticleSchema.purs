module Lettera.ArticleSchema
  ( renderAsJsonLd
  ) where

import Prelude
import Data.Maybe (Maybe(..), maybe)
import Data.Argonaut.Core (Json)
import Data.Argonaut.Encode.Class (encodeJson)
import Lettera.Models (Article)
import KSF.LocalDateTime (formatLocalDateTime)

-- https://developers.google.com/search/docs/appearance/structured-data/paywalled-content
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
    , "isAccessibleForFree" :: String
    , "hasPart" :: Maybe
          { "@type" :: String
          , "isAccessibleForFree" :: String
          , "cssSelector" :: String
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
  , isAccessibleForFree: if article.premium then "False" else "True"
  , hasPart: if article.premium
             then Just { "@type": "WebPageElement"
                       , "isAccessibleForFree": "False"
                       , "cssSelector": ".premium-only"
                       }
             else Nothing
  }

renderAsJsonLd :: Article -> Json
renderAsJsonLd article = encodeJson $ articleToJsonLd article
