module KSF.HtmlRenderer.Models where

import Prelude

import Data.Array (any)
import Data.Function.Uncurried (Fn1, Fn2, Fn3, runFn1, runFn2, runFn3)
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Nullable (Nullable, toMaybe, toNullable)
import Effect (Effect)
import Effect.Uncurried (EffectFn2, EffectFn3, mkEffectFn2, mkEffectFn3)
import React.Basic (JSX)

-- | Represents a node object returned by html-to-react
foreign import data Node :: Type

-- Getters
foreign import getRawImpl          :: Fn1 Node (Nullable String)
foreign import getDataImpl         :: Fn1 Node (Nullable String)
foreign import getTypeImpl         :: Fn1 Node (Nullable String)
foreign import getNameImpl         :: Fn1 Node (Nullable String)
foreign import getStringAttribImpl :: Fn2 String Node (Nullable String)
foreign import getChildrenImpl     :: Fn1 Node (Nullable (Array Node))

getRaw :: Node -> Maybe String
getRaw n = toMaybe $ runFn1 getRawImpl n

getData :: Node -> Maybe String
getData n = toMaybe $ runFn1 getDataImpl n

getType :: Node -> Maybe String
getType n = toMaybe $ runFn1 getTypeImpl n

getName :: Node -> Maybe String
getName n = toMaybe $ runFn1 getNameImpl n

getStringAttrib :: String -> Node -> Maybe String
getStringAttrib name node = toMaybe $ runFn2 getStringAttribImpl name node

getChildren :: Node -> Maybe (Array Node)
getChildren n = toMaybe $ runFn1 getChildrenImpl n

foreign import setStringAttribImpl :: Fn3 String String Node Node
foreign import removeAttribImpl :: Fn2 String Node Node

setStringAttrib :: String -> String -> Node -> Node
setStringAttrib = runFn3 setStringAttribImpl

removeAttrib :: String -> Node -> Node
removeAttrib = runFn2 removeAttribImpl

-- | Utility to turn a hook into a generic one
class ToGenericHook h where
  toGenericHook :: h -> GenericHook

-- | A representation of a hook, can be used to group different
--   types of hooks in the same collection
--   See: https://stackoverflow.com/questions/53270182/similar-record-types-in-a-list-array-in-purescript
newtype HookRep = HookRep (forall a. (forall h. ToGenericHook h => h -> a) -> a)

toHookRep :: forall h. ToGenericHook h => h -> HookRep
toHookRep h = HookRep \f -> f h

-- | This type of hook is to replace the children of
--   targeted nodes
newtype ReplacingHook = ReplacingHook ReplacingHookRecord

type ReplacingHookRecord =
  { shouldProcessNode :: Node -> Boolean
  , processNode       :: Node -> Array Node -> Int -> Effect JSX
  }

replacingHook :: ReplacingHookRecord -> HookRep
replacingHook = toHookRep <<< ReplacingHook

instance replacingHookToGenericHook :: ToGenericHook ReplacingHook where
  toGenericHook (ReplacingHook { shouldProcessNode, processNode }) =
    { replaceChildren: true
    , shouldPreprocessNode: Nothing
    , shouldProcessNode: Just shouldProcessNode
    , preprocessNode: Nothing
    , processNode: Just processNode
    }

-- | This type of hook is just to modify targeted nodes,
--   without replacing their children
newtype ModifyingHook = ModifyingHook ModifyingHookRecord

type ModifyingHookRecord =
  { shouldProcessNode :: Node -> Boolean
  , processNode       :: Node -> Array Node -> Effect Node
  }

modifyingHook :: ModifyingHookRecord -> HookRep
modifyingHook = toHookRep <<< ModifyingHook

instance modifyingHookToGenericHook :: ToGenericHook ModifyingHook where
  toGenericHook (ModifyingHook { shouldProcessNode, processNode }) =
    { replaceChildren: false
    , shouldPreprocessNode: Just shouldProcessNode
    , shouldProcessNode: Nothing
    , preprocessNode: Just processNode
    , processNode: Nothing
    }

type GenericHook =
  { replaceChildren      :: Boolean
  , shouldPreprocessNode :: Maybe (Node -> Boolean)
  , shouldProcessNode    :: Maybe (Node -> Boolean)
  , preprocessNode       :: Maybe (Node -> Array Node -> Effect Node)
  , processNode          :: Maybe (Node -> Array Node -> Int -> Effect JSX)
  }

type JSGenericHook =
  { replaceChildren      :: Boolean
  , shouldPreprocessNode :: Nullable (Node -> Boolean)
  , shouldProcessNode    :: Node -> Boolean
  , preprocessNode       :: Nullable (EffectFn2 Node (Array Node) Node)
  , processNode          :: Nullable (EffectFn3 Node (Array Node) Int JSX)
  }

toJSGenericHook :: GenericHook -> JSGenericHook
toJSGenericHook h =
  { replaceChildren:      h.replaceChildren
  , shouldPreprocessNode: toNullable h.shouldPreprocessNode
  , shouldProcessNode:    fromMaybe (const false) h.shouldProcessNode
  , preprocessNode:       toNullable $ mkEffectFn2 <$> h.preprocessNode
  , processNode:          toNullable $ mkEffectFn3 <$> h.processNode
  }

-- | Performs a Depth-First Search (DFS) on the given node and returns `true`
--   once a children that satisfies the given predicate is found or `false`
--   once all reachable nodes have been visited and none of them satisfied the
--   predicate.
dfs :: (Node -> Boolean) -> Node -> Boolean
dfs prop node = prop node || maybe false (any (dfs prop)) (getChildren node)
