module Mosaico.Frontpage.Events where

import Prelude

import Data.Maybe (Maybe(..), maybe)
import Effect (Effect)
import React.Basic.Events (EventHandler)
import React.Basic.DOM.Events (capture, target)
import Web.DOM (Node)
import Web.DOM.Element as Element
import Web.DOM.Node as Node

onFrontpageClick :: (String -> Effect Unit) -> EventHandler
onFrontpageClick setRoute = capture target $ \tgt -> do
  let findAnchor :: Node -> Effect (Maybe String)
      findAnchor node = do
        let continue = maybe (pure Nothing) findAnchor =<< Node.parentNode node
        case Node.nodeName node of
          "A" ->
            maybe continue (pure <<< Just) =<<
            maybe (pure Nothing) (Element.getAttribute "href") (Element.fromNode node)
          _ ->
            continue
  maybeHref <- maybe (pure Nothing) findAnchor $ Node.fromEventTarget tgt
  case maybeHref of
    Just href -> setRoute href
    _ -> pure unit
