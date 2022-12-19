module Ospedale.Page.Edit where

import Prelude

import Control.Alt ((<|>))
import Data.Array (filter, head)
import Data.Either (either)
import Data.Foldable (find, fold, foldMap)
import Data.Maybe (Maybe(..), fromMaybe, isJust, maybe)
import Data.Monoid (guard)
import Data.Newtype (unwrap)
import Data.String (Pattern(..), joinWith, null, split, trim)
import Data.Tuple (Tuple(..), fst, snd)
import Effect (Effect)
import Effect.Aff as Aff
import Effect.Class (liftEffect)
import Effect.Now as Now
import KSF.LocalDateTime (LocalDateTime, formatLocalDateTime)
import KSF.InputField as InputField
import KSF.InputField.Checkbox as Checkbox
import KSF.Paper (Paper)
import KSF.Paper as Paper
import Lettera as Lettera
import Lettera.Models (Article, ArticleType(..), BodyElement(..), Tag(..), articleTypes, fromUTCTime)
import React.Basic.DOM as DOM
import React.Basic.DOM.Events (capture_, targetValue)
import React.Basic.Events (handler)
import React.Basic.Hooks (Component, JSX, useState', (/\))
import React.Basic.Hooks as React
import Web.HTML as Web
import Web.HTML.Window (confirm)

type Props =
  { paper :: Paper
  , article :: Maybe Article
  , closeArticle :: Maybe Article -> Effect Unit
  }

component :: Component Props
component = do
  window <- Web.window
  React.component "Editor" $ \ { paper, article: inputArticle, closeArticle } -> React.do
    success /\ setSuccess <- useState' false
    modified /\ setModified <- useState' false
    let modifying :: forall a. (a -> Effect Unit) -> a -> Effect Unit
        modifying = (\f x -> do
                        setModified true
                        f x)

    title /\ setTitle <- (map <<< map) modifying $ useState' $ fromMaybe "" $ _.title <$> inputArticle
    listTitle /\ setListTitle <- (map <<< map) modifying $ useState' $ _.listTitle =<< inputArticle
    body /\ setBody <- (map <<< map) modifying $ useState' $ fromMaybe "" $ (joinWith "\n\n" <<< map buildBody <<< _.body) <$> inputArticle
    preamble /\ setPreamble <- (map <<< map) modifying $ useState' $ maybe "" (joinWith "\n\n" <<< _.preamble) inputArticle
    publish /\ setPublish <- (map <<< map) modifying $ useState' $ isJust $ _.publishingTime =<< inputArticle
    articleType /\ setArticleType <- (map <<< map) modifying $ useState' $ Tuple NyhetStor "NyhetStor"
    author /\ setAuthor <- (map <<< map) modifying $ useState' $ maybe "" _.byline $ head <<< _.authors =<< inputArticle
    tags /\ setTags <- (map <<< map) modifying $ useState' $ maybe "" (joinWith ", " <<< map unwrap <<< _.tags) inputArticle
    categories /\ setCategories <- (map <<< map) modifying $ useState' $ maybe "" (joinWith ", " <<< _.categories) inputArticle
    savedArticle /\ setSavedArticle <-
      (map <<< map)
      (\f x -> do
          setSuccess true
          setModified false
          f x) $ useState' Nothing
    error /\ setError <- useState' Nothing

    let submit = do
          now <- Now.nowDateTime
          nowLocal <- fromUTCTime now
          let publishingTime :: Maybe LocalDateTime
              publishingTime =
                if not publish then Nothing
                else (_.publishingTime =<< inputArticle) <|> nowLocal
              updateTime =
                if publish && isJust inputArticle then nowLocal else Nothing
          let article =
                { uuid: maybe "00000000-0000-0000-0000-000000000000" _.uuid inputArticle
                , title
                , listTitle
                , mainImage: Nothing
                , preamble: map trim $ split (Pattern "\n\n") preamble
                , authors: if null $ trim author then []
                           else [{ byline: trim author, image: Nothing, email: Nothing }]
                , publishingTime
                , updateTime
                , tags: map Tag $ filter (not <<< null) $ map trim $ split (Pattern ",") tags
                , categories: filter (not <<< null) $ map trim $ split (Pattern ",") categories
                , articleType: fst articleType
                , publishingTimeUtc: Nothing
                , body: map (Html <<< trim) $ split (Pattern "\n\n") body
                , charLength: 0
                , premium: false
                , removeAds: false
                , live: false
                , externalScripts: Nothing
                , articleTypeDetails: Nothing
                  -- Lettera expects this to be non-null, even though
                  -- it only generates them.
                , shareUrl: Just ""
                , analyticsSection: Nothing
                , analyticsCategory: Nothing
                , paper
                }
          -- Don't reset savedArticle if set already
          setError Nothing
          Aff.launchAff_ $ liftEffect <<< either (setError <<< Just) (setSavedArticle <<< Just) =<<
            ((if isJust inputArticle
              then Lettera.updateFallbackArticle
              else Lettera.createFallbackArticle) article)
    pure $ fold
      [ DOM.div_ [ DOM.text $ "Tidning: " <> Paper.paperName paper ]
      , InputField.inputField
          { type_: InputField.Text
          , name: "title"
          , value: Just title
          , placeholder: ""
          , onChange: setTitle <<< fromMaybe ""
          , validationError: Nothing
          , label: Just "Rubrik"
          }
      , InputField.inputField
          { type_: InputField.Text
          , name: "listTitle"
          , value: listTitle
          , placeholder: ""
          , onChange: setListTitle
          , validationError: Nothing
          , label: Just "Rubrik 2"
          }
      , articleTypeDropDown articleType setArticleType
      , DOM.div
          { className: "ospedale--row"
          , children:
              [ Checkbox.inputCheckbox
                  { type_: Checkbox.Checkbox
                  , name: "published"
                  , checked: publish
                  , onChange: setPublish
                  , label: Just "Publicerat"
                  }
              , InputField.inputField
                  { type_: InputField.Text
                  , name: "published"
                  , value: if publish
                           then formatLocalDateTime <$> (_.publishingTime =<< inputArticle) else Nothing
                  , placeholder: ""
                  , onChange: const mempty
                  , label: Just "published"
                  , validationError: Nothing
                  , disabled: true
                  }
              , InputField.inputField
                  { type_: InputField.Text
                  , name: "updated"
                  , value: if publish
                           then formatLocalDateTime <$> (_.updateTime =<< inputArticle) else Nothing
                  , placeholder: ""
                  , onChange: const mempty
                  , label: Just "uppdaterat"
                  , validationError: Nothing
                  , disabled: true
                  }
              ]
          }
      , InputField.inputField
          { type_: InputField.Text
          , name: "author"
          , value: Just author
          , placeholder: ""
          , onChange: setAuthor <<< fromMaybe ""
          , validationError: Nothing
          , label: Just "author"
          }
      , DOM.div
          { className: "ospedale--row"
          , children:
              [ InputField.inputField
                  { type_: InputField.Text
                  , name: "categories"
                  , value: Just categories
                  , placeholder: ""
                  , onChange: setCategories <<< fromMaybe ""
                  , validationError: Nothing
                  , label: Just "Kategori"
                  }
              , InputField.inputField
                  { type_: InputField.Text
                  , name: "tags"
                  , value: Just tags
                  , placeholder: ""
                  , onChange: setTags <<< fromMaybe ""
                  , validationError: Nothing
                  , label: Just "Taggar"
                  }
              ]
          }
      , DOM.div
          { className: "ospedale--textarea"
          , children:
              [ DOM.label
                  { children: [ DOM.text "Ingress" ]
                  }
              , DOM.textarea
                  { name: "preamble"
                  , value: preamble
                  , onChange: handler targetValue $ setPreamble <<< fromMaybe ""
                  }
              ]
          }
      , DOM.div
          { className: "ospedale--textarea"
          , children:
              [ DOM.label
                  { children: [ DOM.text "Brödtext" ]
                  }
              , DOM.textarea
                  { name: "body"
                  , value: body
                  , onChange: handler targetValue $ setBody <<< fromMaybe ""
                  }
              ]
          }
      , DOM.div
          { className: "ospedale--buttons"
          , children:
              [ DOM.button
                  { children: [ DOM.text "Skicka" ]
                  , onClick: capture_ submit
                  }
              , DOM.button
                  { children: [ DOM.text "Stänga" ]
                  , onClick: capture_ $ if not modified then closeArticle savedArticle else do
                       confirmed <- confirm "Artikeln har ändrats efter att ha sparats. Vänligen bekräfta stängning." window
                       when confirmed $ closeArticle savedArticle
                  }
              ]
          }
      , flip foldMap error $ \errorText ->
          DOM.div
            { className: "error-text"
            , children: [ DOM.text errorText ]
            }
      , guard success $
          DOM.div
            { children: [ DOM.text $ "Artikeln sparat" <>
                          (if isJust (_.publishingTime <$> savedArticle)
                           then " och publicerat" else "") <>
                          (if modified then ". Artikeln har ändrats efter att ha sparats." else "")
                        ]
            }
      ]
  where
    -- Assume only HTML elements
    buildBody (Html html) = html
    buildBody _ = ""

type TagArticle = Tuple ArticleType String

articleTypeDropDown :: TagArticle -> (TagArticle -> Effect Unit) -> JSX
articleTypeDropDown articleType setArticleType =
  DOM.div
    { className: "input-field--container"
    , children:
        [ InputField.inputLabel { label: "Artikel Typ", nameFor: "articleType" }
        , DOM.select
            { children: map createOption articleTypes
            , onChange: handler targetValue (onChange <<< fromMaybe "")
            , value: snd articleType
            }
        ]
    }
  where
    createOption typ =
      DOM.option
        { value: snd typ
        , children: [ DOM.text $ snd typ ]
        }
    onChange value =
      foldMap setArticleType $ find ((_ == value) <<< snd) articleTypes
