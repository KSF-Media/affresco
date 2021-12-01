module Main where

import Prelude

import Control.Alt ((<|>))
import Data.Argonaut.Core as JSON
import Data.Argonaut.Core (stringify)
import Data.Argonaut.Encode (encodeJson)
import Data.Array (cons, find, foldl, null)
import Data.Array.NonEmpty as NonEmptyArray
import Data.Either (Either(..))
import Data.Foldable (fold, foldM, foldMap)
import Data.HashMap as HashMap
import Data.List (List, intercalate)
import Data.List as List
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Newtype (unwrap)
import Data.String (Pattern(..), Replacement(..), split, replace)
import Data.String.Regex (Regex)
import Data.String.Regex (match, regex) as Regex
import Data.String.Regex.Flags (ignoreCase) as Regex
import Data.Tuple (Tuple(..))
import Data.Tuple.Nested ((/\))
import Data.UUID as UUID
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Aff as Aff
import Effect.Exception (throw)
import Effect.Class (liftEffect)
import Effect.Uncurried (EffectFn2, runEffectFn2)
import Foreign.Object as Object
import JSURI as URI
import KSF.Api (Token(..), UserAuth, parseToken)
import KSF.Paper (Paper(..))
import Lettera as Lettera
import Lettera.Models (ArticleStub, Category(..), CategoryLabel(..), DraftParams, FullArticle, encodeStringifyArticle, encodeStringifyArticleStubs, fromFullArticle, isDraftArticle, isPreviewArticle, notFoundArticle, tagToURIComponent, uriComponentToTag)
import Mosaico.Article as Article
import Mosaico.Frontpage as Frontpage
import MosaicoServer (MainContent(..))
import MosaicoServer as MosaicoServer
import Node.Encoding (Encoding(..))
import Node.FS.Sync as FS
import Node.HTTP as HTTP
import Payload.ContentType as ContentType
import Payload.Headers as Headers
import Payload.ResponseTypes (Failure(..), Response(..), ResponseBody(..))
import Payload.Server as Payload
import Payload.Server.Guards as Guards
import Payload.Server.Handlers (File)
import Payload.Server.Handlers as Handlers
import Payload.Server.Response (class EncodeResponse)
import Payload.Server.Response as Response
import Payload.Server.Status as Status
import Payload.Spec (type (:), DELETE, GET, POST, Guards, Spec(Spec), Nil)
import React.Basic (fragment) as DOM
import React.Basic.DOM (div, meta) as DOM
import React.Basic.DOM.Server (renderToStaticMarkup, renderToString) as DOM

foreign import appendMosaicoImpl :: EffectFn2 String String String
appendMosaico :: String -> String -> Effect String
appendMosaico content htmlTemplate = runEffectFn2 appendMosaicoImpl content htmlTemplate

foreign import appendHeadImpl :: EffectFn2 String String String
appendHead :: String -> String -> Effect String
appendHead = runEffectFn2 appendHeadImpl

foreign import serverPort :: Int

newtype TextHtml = TextHtml String
instance encodeResponsePlainHtml :: EncodeResponse TextHtml where
  encodeResponse (Response res) = do
    let (TextHtml b) = res.body
    pure $
      Response
        { status: res.status
        , headers: Headers.setIfNotDefined "content-type" ContentType.html res.headers
        , body: StringBody b
        }

type Env =
  { htmlTemplate :: String
  , categoryStructure :: Array Category
  , categoryRegex :: Regex
  , staticPages :: HashMap.HashMap String String
  }

indexHtmlFileLocation :: String
indexHtmlFileLocation = "./dist/client/index.html"

spec ::
  Spec
    { routes ::
         { getDraftArticle ::
              GET "/artikel/draft/<aptomaId>/?dp-time=<time>&publicationId=<publication>&user=<user>&hash=<hash>"
                { response :: ResponseBody
                , params :: { aptomaId :: String }
                , query :: DraftParams
                }
         , getArticle ::
              GET "/artikel/<uuidOrSlug>"
                { response :: ResponseBody
                , params :: { uuidOrSlug :: String }
                , guards :: Guards ("credentials" : Nil)
                }
         , assets ::
              GET "/assets/<..path>"
                { params :: { path :: List String }
                , response :: File
                }
         , tagList ::
              GET "/tagg/<tag>"
                { response :: ResponseBody
                , params :: { tag :: String }
                , guards :: Guards ("credentials" : Nil)
                }
         , frontpage ::
              GET "/"
                { response :: TextHtml
                , guards :: Guards ("credentials" : Nil)
                }
         , staticPage ::
              GET "/sida/<pageName>"
                { response :: ResponseBody
                , params :: { pageName :: String }
                }
          , categoryPage ::
              GET "/<categoryName>"
                { response :: ResponseBody
                , params :: { categoryName :: String }
                , guards :: Guards ("category" : Nil)
                }
          , setAuthCookie ::
              POST "/api/authCookie"
                { response :: ResponseBody
                , guards :: Guards ("credentials" : Nil)
                }
          , deleteAuthCookie ::
              DELETE "/api/authCookie"
                { response :: ResponseBody
                }
          , notFound ::
              GET "/<..path>"
                { response :: ResponseBody
                , params :: { path :: List String}
                }
         }
    , guards ::
         { credentials :: Maybe UserAuth
         , category :: Category
         }
    }
spec = Spec

main :: Effect Unit
main = do
  staticPages  <- do
      staticPageNames <- FS.readdir "./static/"
      let makeMap acc staticPageFileName = do
            pageContent <- FS.readTextFile UTF8 $ "./static/" <> staticPageFileName
            let staticPageName = replace (Pattern ".html") (Replacement "") staticPageFileName
            pure $ HashMap.insert staticPageName pageContent acc
      foldM makeMap HashMap.empty staticPageNames
  htmlTemplate <- FS.readTextFile UTF8 indexHtmlFileLocation
  Aff.launchAff_ do
    categoryStructure <- Lettera.getCategoryStructure HBL
    -- This is used for matching a category label from a route, such as "/nyheter" or "/norden-och-världen"
    categoryRegex <- case Regex.regex "^\\/([\\w|ä|ö|å|-]+)\\b" Regex.ignoreCase of
      Right r -> pure r
      Left _  -> liftEffect $ throw "I have a very safe regex to parse, yet somehow I didn't know how to parse it. Fix it please. Exploding now, goodbye."
    let env = { htmlTemplate, categoryStructure, categoryRegex, staticPages }
        handlers =
          { getDraftArticle: getDraftArticle env
          , getArticle: getArticle env
          , assets
          , frontpage: frontpage env
          , tagList: tagList env
          , staticPage: staticPage env
          , categoryPage: categoryPage env
          , setAuthCookie: setAuthCookie
          , deleteAuthCookie: deleteAuthCookie
          , notFound: notFound env Nothing
          }
        guards = { credentials: getCredentials, category: parseCategory env }
    Payload.startGuarded (Payload.defaultOpts { port = 8080 }) spec { handlers, guards }

getDraftArticle
  :: Env
  -> { params :: { aptomaId :: String }, query :: DraftParams }
  -> Aff (Response ResponseBody)
getDraftArticle env { params: {aptomaId}, query } = do
  article <- Lettera.getDraftArticle aptomaId query
  renderArticle env Nothing article mempty

getArticle
  :: Env
  -> { params :: { uuidOrSlug :: String }, guards :: { credentials :: Maybe UserAuth } }
  -> Aff (Response ResponseBody)
getArticle env r@{ params: { uuidOrSlug }, guards: { credentials } }
  | Just uuid <- UUID.parseUUID uuidOrSlug = do
      article <- Lettera.getArticle uuid r.guards.credentials
      mostReadArticles <- Lettera.getMostRead 0 10 "" HBL true
      setCredentials credentials
        <$> renderArticle env (Just uuidOrSlug) article mostReadArticles
  | otherwise = do
    article <- Lettera.getArticleWithSlug uuidOrSlug r.guards.credentials
    case article of
      Right a -> do
        pure $ Response
          { status: Status.found
          , body: EmptyBody
          , headers: Headers.fromFoldable [ Tuple "Location" $ "/artikel/" <> (_.uuid $ fromFullArticle a)]
          }
      Left _ -> do
        mostReadArticles <- Lettera.getMostRead 0 10 "" HBL true
        let maybeMostRead = if null mostReadArticles then Nothing else Just mostReadArticles
        notFound env maybeMostRead { params: {path: List.fromFoldable ["artikel", uuidOrSlug]} }

renderArticle
  :: Env
  -> Maybe String
  -> Either String FullArticle
  -> Array ArticleStub
  -> Aff (Response ResponseBody)
renderArticle env uuid article mostReadArticles = do
  mosaico <- liftEffect MosaicoServer.app
  case article of
    Right a -> do
      let articleJSX =
            Article.render
              { brand: "hbl"
              , article: Right a
              , onLogin: pure unit
              , onPaywallEvent: pure unit
              , onTagClick: const mempty
              }
          mosaicoString = DOM.renderToString
                          $ mosaico
                            { mainContent: ArticleContent articleJSX
                            , mostReadArticles
                            , categoryStructure: env.categoryStructure
                            }

      html <- liftEffect do
        let windowVars =
              [ "article"           /\ (encodeStringifyArticle $ fromFullArticle a)
              , "isPreview"         /\ (show $ isPreviewArticle a)
              , "mostReadArticles"  /\ (encodeStringifyArticleStubs mostReadArticles)
              , "isDraft"           /\ (show $ isDraftArticle a)
              , "categoryStructure" /\ (JSON.stringify $ encodeJson env.categoryStructure)
              ]
            metaTags =
              let a' = fromFullArticle a
              in DOM.renderToStaticMarkup $
                  DOM.fragment
                    [ DOM.meta { property: "og:type", content: "article" }
                    , DOM.meta { property: "og:title", content: a'.title }
                    , DOM.meta { property: "og:description", content: fold a'.preamble }
                    , DOM.meta { property: "og:image", content: foldMap _.url a'.mainImage }
                    ]
        appendMosaico mosaicoString env.htmlTemplate >>= appendHead (mkWindowVariables windowVars) >>= appendHead metaTags

      pure $ Response.ok $ StringBody html
    Left _ ->
      let maybeMostRead = if null mostReadArticles then Nothing else Just mostReadArticles
      in notFound env maybeMostRead { params: {path: foldMap (List.fromFoldable <<< (_ `cons` ["artikel"])) uuid} }

assets :: { params :: { path :: List String } } -> Aff (Either Failure File)
assets { params: { path } } = Handlers.directory "dist/client" path

frontpage :: Env -> { guards :: { credentials :: Maybe UserAuth } } -> Aff TextHtml
frontpage env _ = do
  articles <- Lettera.getFrontpage HBL Nothing
  mostReadArticles <- Lettera.getMostRead 0 10 "" HBL true
  mosaico <- liftEffect MosaicoServer.app
  frontpageComponent <- liftEffect Frontpage.frontpageComponent
  let mosaicoString =
        DOM.renderToString
        $ mosaico
          { mainContent:
              FrontpageContent
              $ frontpageComponent
                  { frontpageArticles: articles
                  , onArticleClick: const $ pure unit
                  , onTagClick: const mempty
                  }
          , mostReadArticles
          , categoryStructure: env.categoryStructure
          }
  html <- liftEffect do
            let windowVars =
                  [ "frontpageFeed"     /\ mkArticleFeed Nothing "categoryfeed" articles
                  , "mostReadArticles"  /\ encodeStringifyArticleStubs mostReadArticles
                  , "categoryStructure" /\ (JSON.stringify $ encodeJson env.categoryStructure)
                  ]
            appendMosaico mosaicoString env.htmlTemplate >>= appendHead (mkWindowVariables windowVars)
  pure $ TextHtml html

mkArticleFeed :: Maybe String -> String -> Array ArticleStub -> String
mkArticleFeed feedPage feedType feedContent =
  stringify $ encodeJson { feedPage, feedType, feedContent: encodeStringifyArticleStubs feedContent }

tagList :: Env -> { params :: { tag :: String }, guards :: { credentials :: Maybe UserAuth } } -> Aff (Response ResponseBody)
tagList env { params: { tag } } = do
  let tag' = uriComponentToTag tag
  articles <- Lettera.getByTag 0 20 tag' HBL
  mostReadArticles <- Lettera.getMostRead 0 10 "" HBL true
  mosaico <- liftEffect MosaicoServer.app
  if null articles
    then notFound env (Just mostReadArticles) { params: { path: List.fromFoldable [ "tagg", tag ] } }
    else do
    frontpageComponent <- liftEffect Frontpage.frontpageComponent
    let mosaicoString =
          DOM.renderToString
          $ mosaico
            { mainContent:
                TagListContent
                $ frontpageComponent
                  { frontpageArticles: articles
                  , onArticleClick: const $ pure unit
                  , onTagClick: const mempty
                  }
            , categoryStructure: env.categoryStructure
            , mostReadArticles
            }
    html <- liftEffect do
              let windowVars =
                    [ "frontpageFeed"     /\ mkArticleFeed (Just $ tagToURIComponent tag') "tagfeed" articles
                    , "mostReadArticles"  /\ encodeStringifyArticleStubs mostReadArticles
                    , "categoryStructure" /\ (JSON.stringify $ encodeJson env.categoryStructure)
                    ]
              appendMosaico mosaicoString env.htmlTemplate >>= appendHead (mkWindowVariables windowVars)
    pure $ Response.ok $ StringBody html

staticPage :: Env -> { params :: { pageName :: String }} -> Aff (Response ResponseBody)
staticPage env { params: { pageName } } = do
  mostReadArticles <- Lettera.getMostRead 0 10 "" HBL true
  case HashMap.lookup pageName env.staticPages of
    Just staticPageContent -> do
      mosaico <- liftEffect MosaicoServer.app
      let staticPageJsx =
            DOM.div { className: "mosaico--static-page"
                    , dangerouslySetInnerHTML: { __html: staticPageContent }
                    }
      let mosaicoString =
            DOM.renderToString
            $ mosaico
              { mainContent: StaticPageContent staticPageJsx
              , mostReadArticles
              , categoryStructure: env.categoryStructure
              }
      html <- liftEffect do
        let staticPageString = JSON.stringify $ JSON.fromString $ DOM.renderToString staticPageJsx
            staticPageObj = Object.singleton "pageName" pageName
                            # Object.insert "pageContent" staticPageString
                            # encodeJson
                            # JSON.stringify
            windowVars =
              [ "staticPageContent" /\ staticPageObj
              , "categoryStructure" /\ (JSON.stringify $ encodeJson env.categoryStructure)
              ]
        appendMosaico mosaicoString env.htmlTemplate
          >>= appendHead (mkWindowVariables windowVars)

      pure $ Response.ok $ StringBody html
    Nothing ->
      let maybeMostRead = if null mostReadArticles then Nothing else Just mostReadArticles
      in notFound env maybeMostRead { params: {path: List.fromFoldable ["sida", pageName]} }

categoryPage :: Env -> { params :: { categoryName :: String }, guards :: { category :: Category } } -> Aff (Response ResponseBody)
categoryPage env { params: { categoryName } } = do
  mosaico <- liftEffect MosaicoServer.app
  frontpageComponent <- liftEffect Frontpage.frontpageComponent
  articles <- Lettera.getFrontpage HBL (Just categoryName)
  mostReadArticles <- Lettera.getMostRead 0 10 "" HBL true
  let mosaicoString = DOM.renderToString
                          $ mosaico
                            { mainContent: FrontpageContent $ frontpageComponent
                                { frontpageArticles: articles
                                , onArticleClick: const $ pure unit
                                , onTagClick: const mempty
                                }
                            , mostReadArticles
                            , categoryStructure: env.categoryStructure
                            }
  html <- liftEffect do
            let windowVars =
                  [ "frontpageFeed"     /\ mkArticleFeed (Just categoryName) "categoryfeed" articles
                  , "mostReadArticles"  /\ encodeStringifyArticleStubs mostReadArticles
                  , "categoryStructure" /\ (JSON.stringify $ encodeJson env.categoryStructure)
                  ]
            appendMosaico mosaicoString env.htmlTemplate >>= appendHead (mkWindowVariables windowVars)
  pure $ Response.ok $ StringBody html

notFound :: Env -> Maybe (Array ArticleStub) -> { params :: { path :: List String } } -> Aff (Response ResponseBody)
notFound env _ _ = do
  let articleJSX =
        Article.render
          { brand: "hbl"
          , article: Right notFoundArticle
          , onLogin: pure unit
          , onPaywallEvent: pure unit
          , onTagClick: const mempty
          }

  mosaico <- liftEffect MosaicoServer.app
  let mosaicoString = DOM.renderToString $ mosaico { mainContent: ArticleContent articleJSX, mostReadArticles: [], categoryStructure: env.categoryStructure }
  html <- liftEffect $ do
    let windowVars =
          [ "article" /\ (encodeStringifyArticle $ fromFullArticle notFoundArticle)
          , "categoryStructure" /\ (JSON.stringify $ encodeJson env.categoryStructure)
          ]
    appendMosaico mosaicoString env.htmlTemplate >>= appendHead (mkWindowVariables windowVars)
  pure $ Response.notFound $ StringBody $ html

setAuthCookie :: { guards :: { credentials :: Maybe UserAuth } } -> Aff (Response ResponseBody)
setAuthCookie { guards: { credentials } } =
  setCredentials credentials <$> pure (Response.ok EmptyBody)

deleteAuthCookie :: {} -> Aff (Response ResponseBody)
deleteAuthCookie {} =
  pure $ Response
    { status: Status.ok
    , body: EmptyBody
    , headers: Headers.fromFoldable [ Tuple "Set-Cookie" "CombinedAuth=; Secure; SameSite=Strict; HttpOnly; Path=/; Max-age=0" ]
    }

-- Payload 0.4.0 can't do multiple Set-Cookie headers
setCredentials :: forall a. Maybe UserAuth -> Response a -> Response a
setCredentials Nothing response = response
setCredentials (Just { authToken, userId }) (Response response@{ headers }) = Response
  response { headers = Headers.set "Set-Cookie" cookies headers }
  where
    cookies = "CombinedAuth=" <> (\(Token t) -> t) authToken <> "|" <> UUID.toString userId <>
              "; Secure; SameSite=Strict; HttpOnly; Path=/; Max-Age=31536000"

getCredentials :: HTTP.Request -> Aff (Maybe UserAuth)
getCredentials req = do
  headers <- Guards.headers req
  cookies <- Guards.cookies req
  pure $
    (do
        authToken <- parseToken =<< Headers.lookup "Authorization" headers
        userId <- UUID.parseUUID =<< Headers.lookup "AuthUser" headers
        pure { authToken, userId }) <|>
    (do
        combined <- Map.lookup "CombinedAuth" cookies
        authToken /\ userId <- case split (Pattern "|") combined of
          [a, u] -> do
            userId <- UUID.parseUUID u
            -- Cookie token value is without prefix
            Just $ (Token a) /\ userId
          _ -> Nothing
        pure { authToken, userId })

parseCategory :: Env -> HTTP.Request -> Aff (Either Failure Category)
parseCategory { categoryRegex, categoryStructure } req = do
  let url = HTTP.requestURL req
      urlDecoded = fromMaybe url $ URI.decodeURIComponent url
      categoryRoute = CategoryLabel $ fold $ NonEmptyArray.last =<< Regex.match categoryRegex urlDecoded
      -- Flatten out categories from the category structure
      categories = foldl (\acc (Category c) -> acc <> [Category c] <> c.subCategories) [] categoryStructure
  case find ((_ == categoryRoute) <<< _.label <<< unwrap) categories of
    Just c -> pure $ Right c
    _ -> pure $ Left (Forward "Did not match category")

mkWindowVariables :: Array (Tuple String String) -> String
mkWindowVariables vars =
  let jsVars = map (\(name /\ value) -> "window." <> name <> "=" <> value <> ";") vars
  in "<script>" <> intercalate "" jsVars <> "</script>"
