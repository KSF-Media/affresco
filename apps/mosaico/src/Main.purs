module Main where

import Prelude

import Data.Maybe (Maybe(..), fromMaybe)
import Data.UUID (UUID)
import Data.UUID as UUID
import Data.List (List)
import Data.Either (Either)
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Aff as Aff
import Effect.Class.Console as Console
import Lettera as Lettera
import Lettera.Models (Article, BodyElement(..))
import Mosaico.Article as Article
import Node.HTTP as HTTP
import Payload.ContentType as ContentType
import Payload.Headers as Headers
import Payload.Server.Handlers (File)
import Payload.Server.Handlers as Handlers
import Payload.ResponseTypes (Failure, Response(..), ResponseBody(..))
import Payload.Server as Payload
import Payload.Server.Guards as Guards
import Payload.Server.Response (class EncodeResponse)
import Payload.Spec (type (:), GET, Guards, Spec(Spec), Nil)
import React.Basic (JSX)
import React.Basic.DOM as DOM
import React.Basic.DOM.Server (renderToString) as DOM

-- NOTE: We need to require dotenv in JS
foreign import requireDotenv :: Unit

newtype TextHtml = TextHtml String

type Credentials = { userId :: UUID, token :: String }

instance encodeResponsePlainHtml :: EncodeResponse TextHtml where
  encodeResponse r@(Response res) = do
    let (TextHtml b) = res.body
    pure $
      Response
        { status: res.status
        , headers: Headers.setIfNotDefined "content-type" ContentType.html res.headers
        , body: StringBody b
        }

spec ::
  Spec
    { routes ::
         { getArticle ::
              GET "/artikel/<uuid>"
                { response :: TextHtml
                , params :: { uuid :: String }
                , guards :: Guards ("credentials" : Nil)
                }
         , getMostRead ::
              GET "/mostread"
                { response :: TextHtml }
         , assets ::
              GET "/assets/<..path>"
            { params :: { path :: List String }
            , response :: File
            }
         }
    , guards :: { credentials :: Maybe Credentials }
    }
spec = Spec

main :: Effect Unit
main = do
  let handlers = { getArticle, getMostRead, assets }
      guards = { credentials: getCredentials }
  Aff.launchAff_ $ Payload.startGuarded_ spec { handlers, guards }

assets :: { params :: { path :: List String } } -> Aff (Either Failure File)
assets { params: {path} } = Handlers.directory "dist" path 

getArticle :: { params :: { uuid :: String }, guards :: { credentials :: Maybe Credentials } } -> Aff TextHtml
getArticle r@{ params: { uuid } } = do
  case r.guards.credentials of
    -- TODO: Pass credentials to Lettera
    Just _  -> Console.log "YES CREDS!"
    Nothing -> Console.log "NO CREDS!"
  article <- Lettera.getArticle (fromMaybe UUID.emptyUUID $ UUID.parseUUID uuid)
  pure $ TextHtml $ mosaicoString article

getCredentials :: HTTP.Request -> Aff (Maybe Credentials)
getCredentials req = do
  headers <- Guards.headers req
  let tokens = do
        token  <- Headers.lookup "Authorization" headers
        userId <- UUID.parseUUID =<< Headers.lookup "Auth-User" headers
        pure { token, userId }
  pure tokens

getMostRead :: {} -> Aff TextHtml
getMostRead _ = do
  mostReadArticles <- Lettera.getMostRead
  pure $ TextHtml $ DOM.renderToString $ mostRead mostReadArticles
  where
    mostRead articles =
      DOM.ul
        { className: "most-read-yo"
        , children: map mkListItem articles
        }

    mkListItem a =
      DOM.li
        { children:
          [ DOM.a
            { href: "/artikel/" <> UUID.toString a.uuid
            , children: [ DOM.text a.title ]
            }
          ]
        }

mosaicoString :: Article -> String
mosaicoString = DOM.renderToString <<< mosaico

mosaico :: Article -> JSX
mosaico a =
  DOM.html
    { lang: "sv"
    , children:
      [ DOM.head
        { children:
          [ DOM.meta { charSet: "UTF-8" }
          , DOM.meta
            { name: "viewport"
            , content: "width=device-width, initial-scale=1.0"
            }
          , DOM.link
            { rel: "stylesheet"
            , href: "/assets/mosaico.css"
            }
          ]
        }
      , DOM.body
        { children:
          [ DOM.div
            { className: "mosaico"
            , children:
              [ DOM.text "THIS IS AN ARTICLE"
              , Article.article
                { article: a
                , brand: "hbl"
                }
              ]
            }
          ]
        }
      ]
    }
