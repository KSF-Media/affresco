module Ospedale.Page.List where

import Prelude

import Data.Array as Array
import Data.Array (cons, length)
import Data.Either (Either(..), either)
import Data.Foldable (fold, foldMap)
import Data.Maybe (Maybe(..), fromMaybe, isNothing, maybe)
import Data.UUID (UUID)
import Effect (Effect)
import Effect.Aff as Aff
import Effect.Class (liftEffect)
import KSF.InputField (inputLabel)
import KSF.LocalDateTime (formatLocalDateTime)
import KSF.Paper as Paper
import KSF.Paper (Paper(..))
import KSF.Spinner as Spinner
import Lettera.Fallback as Lettera
import Lettera.Models (Article)
import Ospedale.Page.Edit as Page.Edit
import Ospedale.Page.Error (renderError)
import React.Basic.DOM as DOM
import React.Basic.DOM.Events (capture_, targetValue)
import React.Basic.Events (handler, handler_)
import React.Basic.Hooks (Component, useEffectOnce, useState, useState', (/\)) -- )
import React.Basic.Hooks as React

type Props =
  { newArticle :: Effect Unit
    -- Only sets URL
  , openArticle :: Article -> Effect Unit
    -- Only sets logo in nav bar
  , closeArticle :: Effect Unit
  , setNavPaper :: Paper -> Effect Unit
  , initialArticle :: Maybe UUID
  , relogin :: Effect Unit
  -- If Nothing user needs to log in again.
  , token :: Maybe String
  }

component :: Component Props
component = do
  editComponent <- Page.Edit.component
  React.component "List" $ \ props@{ newArticle, openArticle, setNavPaper, initialArticle, relogin, token } -> React.do
    let onSetPaper f p@(Just paper) = do
          setNavPaper paper
          f p
        onSetPaper f p = f p
    paper /\ setPaper <- (map <<< map) onSetPaper $ useState' Nothing
    error /\ setError <- useState' Nothing
    loading /\ setLoading <- useState' Nothing
    articles /\ setArticles <- useState Nothing
    -- Nothing: bare list, Just Nothing: edit new, Just Just: edit old
    let onOpen :: ((Maybe (Maybe Article)) -> Effect Unit) -> Maybe (Maybe Article) -> Effect Unit
        onOpen f Nothing = do
          props.closeArticle
          f Nothing
        onOpen f a@(Just Nothing) = do
          newArticle
          f a
        onOpen f a@(Just (Just article)) = do
          openArticle article
          setPaper $ Just article.paper
          f a
    openedArticle /\ (setOpenedArticle :: Maybe (Maybe Article) -> Effect Unit) <- (map <<< map) onOpen $ useState' Nothing

    useEffectOnce do
      flip foldMap ({t:_, uuid:_} <$> token <*> initialArticle) $ \{t, uuid} -> do
        Aff.launchAff_ $ Spinner.withSpinner setLoading do
          liftEffect <<< either
            (setError <<< Just)
            (setOpenedArticle <<< Just <<< Just) =<< Lettera.getSingleFallbackArticle t uuid
      pure $ pure unit

    let loadArticles p = do
          setArticles $ const Nothing
          setError Nothing
          flip foldMap token $ \t -> Aff.launchAff_ do
            response <- Lettera.getFallbackArticles t 0 50 p
            liftEffect $ case response of
              Right a -> setArticles $ const $ Just a
              Left err -> setError $ Just err
            pure unit
        paperSelect str = do
          let maybePaper = case str >>= Paper.fromString of
                Just HBL -> Just HBL
                Just ON -> Just ON
                Just VN -> Just VN
                _ -> Nothing
          flip foldMap maybePaper $ \p -> do
            setPaper $ Just p
            loadArticles p
        loadMore = flip foldMap ({t:_, p:_} <$> token <*> paper) $ \{t, p} -> do
          flip (maybe (pure unit)) articles $ \xs -> do
            let len = length xs
            Aff.launchAff_ do
              moreArticles <- Lettera.getFallbackArticles t len 50 p
              liftEffect $ case moreArticles of
                Left err -> setError $ Just err
                Right more -> setArticles $ map (_ <> more)

        closeArticle updatedArticle = do
          setOpenedArticle Nothing
          if isNothing articles then foldMap loadArticles paper else
            -- Update existing or prepend if not found
            flip foldMap updatedArticle $ \article -> setArticles <<< map $ \oldArticles ->
            fromMaybe (Array.cons article oldArticles) $
            Array.findIndex ((_ == article.uuid) <<< _.uuid) oldArticles >>=
            (\idx -> Array.updateAt idx article oldArticles)

    let renderArticleItem article =
          DOM.div
            { className: "ospedale--article-item"
            , children:
                [ DOM.span
                    { className: "time ospedale--publishingTime"
                    , children: [ DOM.text $ foldMap formatLocalDateTime article.publishingTime ]
                    }
                , DOM.span
                    { className: "time ospedale--updateTime"
                    , children: [ DOM.text $ foldMap formatLocalDateTime article.updateTime ]
                    }
                , DOM.span
                    { className: "ospedale--title"
                    , children: [ DOM.text $ fromMaybe article.title article.listTitle ]
                    }
                , DOM.span
                    { className: "ospedale--action"
                    , children:
                        [ DOM.button
                            { children: [ DOM.text "Öppna" ]
                            , onClick: handler_ do
                                 setOpenedArticle $ Just $ Just article
                            }
                        ]
                    }
                ]
            }

    pure $ case {loading, paper, openedArticle} of
      {loading: Just _} -> Spinner.loadingSpinner
      {paper: Just p, openedArticle: Just article} -> editComponent
        { paper: p
        , article
        , closeArticle
        , relogin
        , token
        }
      _ -> fold
        [ DOM.div
            { className: "ksf-paper-selector"
            , children:
                [ inputLabel { label: "Tidning", nameFor: "paper" }
                , DOM.select
                    { children:
                      DOM.option
                        { disabled: true
                        , value: ""
                        }
                      `cons` map paperOption [ HBL, VN, ON ]
                    , onChange: handler targetValue paperSelect
                    , value: maybe "" Paper.toString paper
                    }
                ]
            }
        , foldMap (renderError relogin "Authentication error. Need to relogin.") error
        , fold $ paper *> Just
            (DOM.div
              { className: ""
              , children:
                  [ DOM.button
                      { children: [ DOM.text "Ny artikel" ]
                      , onClick: capture_ $ setOpenedArticle $ Just Nothing
                      }
                  ]
              }
            )
        , case articles of
              Nothing ->
                case error of
                  Just _ -> mempty
                  _ -> case paper of
                    Nothing -> DOM.div_ [ DOM.text "Välj tidning" ]
                    Just _ -> Spinner.loadingSpinner
              Just xs ->
                DOM.div
                  { className: "ospedale-article-list"
                  , children: map renderArticleItem xs
                  } <>
                DOM.button
                  { children: [ DOM.text "Ladda mer" ]
                  , onClick: handler_ loadMore
                  }
        ]
  where
    paperOption paper =
      DOM.option
        { children: [ DOM.text $ Paper.paperName paper ]
        , value: Paper.toString paper
        }
