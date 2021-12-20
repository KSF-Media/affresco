module KSF.HtmlRenderer.Models where

import Data.Maybe (Maybe(..))
import Data.Nullable (toMaybe, toNullable)
import React.Basic (JSX)

-- | Utility to turn a hook into a generic one
class ToGenericHook h where
  toGenericHook :: h -> GenericHook

-- | This type of hook is to replace the children of 
--   targeted nodes
newtype ReplacingHook = ReplacingHook
  { shouldProcessNode :: Node -> Boolean
  , processNode       :: Node -> Array Node -> String -> JSX
  }

instance replacingHookToGenericHook :: ToGenericHook ReplacingHook where
  toGenericHook (ReplacingHook { shouldProcessNode, processNode }) = 
    { replaceChildren: true
    , shouldProcessNode
    , processNode: Nothing
    , processNodeWithReplacement: Just processNode
    }

-- | This type of hook is just to modify targeted nodes,
--   without replacing their children
newtype ModifyingHook = ModifyingHook
  { shouldProcessNode :: Node -> Boolean
  , processNode       :: Node -> Array Node -> JSX
  }

instance modifyingHookToGenericHook :: ToGenericHook ModifyingHook where
  toGenericHook (ModifyingHook { shouldProcessNode, processNode }) = 
    { replaceChildren: false
    , shouldProcessNode
    , processNode: Just processNode
    , processNodeWithReplacement: Nothing
    }

type GenericHook =
  { replaceChildren            :: Boolean
  , shouldProcessNode          :: Node -> Boolean
  , processNode                :: Maybe (Node -> Array Node -> JSX)
  , processNodeWithReplacement :: Maybe (Node -> Array Node -> String -> JSX)
  }

newtype Node = Node
  { raw      :: String
  , data     :: String
  , type     :: String
  , name     :: Maybe String
  , attribs  :: Maybe Attributes
  , children :: Maybe (Array Node)
  }


newtype Attributes = Attributes
  { accept            :: Maybe String
  , "accept-charset"  :: Maybe String
  , accesskey         :: Maybe String
  , action            :: Maybe String
  , allowfullscreen   :: Maybe String
  , allowtransparency :: Maybe String
  , alt               :: Maybe String
  , async             :: Maybe String
  , autocomplete      :: Maybe String
  , autofocus         :: Maybe String
  , autoplay          :: Maybe String
  , capture           :: Maybe String
  , cellpadding       :: Maybe String
  , cellspacing       :: Maybe String
  , challenge         :: Maybe String
  , charset           :: Maybe String
  , checked           :: Maybe String
  , cite              :: Maybe String
  , class             :: Maybe String
  , colspan           :: Maybe String
  , cols              :: Maybe String
  , content           :: Maybe String
  , contenteditable   :: Maybe String
  , contextmenu       :: Maybe String
  , controls          :: Maybe String
  , coords            :: Maybe String
  , crossorigin       :: Maybe String
  , data              :: Maybe String
  , datetime          :: Maybe String
  , default           :: Maybe String
  , defer             :: Maybe String
  , dir               :: Maybe String
  , disabled          :: Maybe String
  , download          :: Maybe String
  , draggable         :: Maybe String
  , enctype           :: Maybe String
  , form              :: Maybe String
  , formaction        :: Maybe String
  , formenctype       :: Maybe String
  , formmethod        :: Maybe String
  , formnovalidate    :: Maybe String
  , formtarget        :: Maybe String
  , frameborder       :: Maybe String
  , headers           :: Maybe String
  , height            :: Maybe String
  , hidden            :: Maybe String
  , high              :: Maybe String
  , href              :: Maybe String
  , hreflang          :: Maybe String
  , for               :: Maybe String
  , "http-equiv"      :: Maybe String
  , icon              :: Maybe String
  , id                :: Maybe String
  , inputmode         :: Maybe String
  , integrity         :: Maybe String
  , is                :: Maybe String
  , keyparams         :: Maybe String
  , keytype           :: Maybe String
  , kind              :: Maybe String
  , label             :: Maybe String
  , lang              :: Maybe String
  , list              :: Maybe String
  , loop              :: Maybe String
  , low               :: Maybe String
  , manifest          :: Maybe String
  , marginheight      :: Maybe String
  , marginwidth       :: Maybe String
  , max               :: Maybe String
  , maxlength         :: Maybe String
  , media             :: Maybe String
  , mediagroup        :: Maybe String
  , method            :: Maybe String
  , min               :: Maybe String
  , minlength         :: Maybe String
  , multiple          :: Maybe String
  , muted             :: Maybe String
  , name              :: Maybe String
  , novalidate        :: Maybe String
  , nonce             :: Maybe String
  , open              :: Maybe String
  , optimum           :: Maybe String
  , pattern           :: Maybe String
  , placeholder       :: Maybe String
  , poster            :: Maybe String
  , preload           :: Maybe String
  , profile           :: Maybe String
  , radiogroup        :: Maybe String
  , readonly          :: Maybe String
  , rel               :: Maybe String
  , required          :: Maybe String
  , reversed          :: Maybe String
  , role              :: Maybe String
  , rowspan           :: Maybe String
  , rows              :: Maybe String
  , sandbox           :: Maybe String
  , scope             :: Maybe String
  , scoped            :: Maybe String
  , scrolling         :: Maybe String
  , seamless          :: Maybe String
  , selected          :: Maybe String
  , shape             :: Maybe String
  , size              :: Maybe String
  , sizes             :: Maybe String
  , span              :: Maybe String
  , spellcheck        :: Maybe String
  , src               :: Maybe String
  , srcdoc            :: Maybe String
  , srclang           :: Maybe String
  , srcset            :: Maybe String
  , start             :: Maybe String
  , step              :: Maybe String
  , style             :: Maybe String
  , summary           :: Maybe String
  , tabindex          :: Maybe String
  , target            :: Maybe String
  , title             :: Maybe String
  , type              :: Maybe String
  , usemap            :: Maybe String
  , value             :: Maybe String
  , width             :: Maybe String
  , wmode             :: Maybe String
  , wrap              :: Maybe String
  , onclick           :: Maybe String 
  }


toJSRecord (Attributes as) =
  { accept:             toNullable as.accept
  , "accept-charset":   toNullable as."accept-charset"
  , accesskey:          toNullable as.accesskey
  , action:             toNullable as.action
  , allowfullscreen:    toNullable as.allowfullscreen
  , allowtransparency:  toNullable as.allowtransparency
  , alt:                toNullable as.alt
  , async:              toNullable as.async
  , autocomplete:       toNullable as.autocomplete
  , autofocus:          toNullable as.autofocus
  , autoplay:           toNullable as.autoplay
  , capture:            toNullable as.capture
  , cellpadding:        toNullable as.cellpadding
  , cellspacing:        toNullable as.cellspacing
  , challenge:          toNullable as.challenge
  , charset:            toNullable as.charset
  , checked:            toNullable as.checked
  , cite:               toNullable as.cite
  , class:              toNullable as.class
  , colspan:            toNullable as.colspan
  , cols:               toNullable as.cols
  , content:            toNullable as.content
  , contenteditable:    toNullable as.contenteditable
  , contextmenu:        toNullable as.contextmenu
  , controls:           toNullable as.controls
  , coords:             toNullable as.coords
  , crossorigin:        toNullable as.crossorigin
  , data:               toNullable as.data
  , datetime:           toNullable as.datetime
  , default:            toNullable as.default
  , defer:              toNullable as.defer
  , dir:                toNullable as.dir
  , disabled:           toNullable as.disabled
  , download:           toNullable as.download
  , draggable:          toNullable as.draggable
  , enctype:            toNullable as.enctype
  , form:               toNullable as.form
  , formaction:         toNullable as.formaction
  , formenctype:        toNullable as.formenctype
  , formmethod:         toNullable as.formmethod
  , formnovalidate:     toNullable as.formnovalidate
  , formtarget:         toNullable as.formtarget
  , frameborder:        toNullable as.frameborder
  , headers:            toNullable as.headers
  , height:             toNullable as.height
  , hidden:             toNullable as.hidden
  , high:               toNullable as.high
  , href:               toNullable as.href
  , hreflang:           toNullable as.hreflang
  , for:                toNullable as.for
  , "http-equiv":       toNullable as."http-equiv"
  , icon:               toNullable as.icon
  , id:                 toNullable as.id
  , inputmode:          toNullable as.inputmode
  , integrity:          toNullable as.integrity
  , is:                 toNullable as.is
  , keyparams:          toNullable as.keyparams
  , keytype:            toNullable as.keytype
  , kind:               toNullable as.kind
  , label:              toNullable as.label
  , lang:               toNullable as.lang
  , list:               toNullable as.list
  , loop:               toNullable as.loop
  , low:                toNullable as.low
  , manifest:           toNullable as.manifest
  , marginheight:       toNullable as.marginheight
  , marginwidth:        toNullable as.marginwidth
  , max:                toNullable as.max
  , maxlength:          toNullable as.maxlength
  , media:              toNullable as.media
  , mediagroup:         toNullable as.mediagroup
  , method:             toNullable as.method
  , min:                toNullable as.min
  , minlength:          toNullable as.minlength
  , multiple:           toNullable as.multiple
  , muted:              toNullable as.muted
  , name:               toNullable as.name
  , novalidate:         toNullable as.novalidate
  , nonce:              toNullable as.nonce
  , open:               toNullable as.open
  , optimum:            toNullable as.optimum
  , pattern:            toNullable as.pattern
  , placeholder:        toNullable as.placeholder
  , poster:             toNullable as.poster
  , preload:            toNullable as.preload
  , profile:            toNullable as.profile
  , radiogroup:         toNullable as.radiogroup
  , readonly:           toNullable as.readonly
  , rel:                toNullable as.rel
  , required:           toNullable as.required
  , reversed:           toNullable as.reversed
  , role:               toNullable as.role
  , rowspan:            toNullable as.rowspan
  , rows:               toNullable as.rows
  , sandbox:            toNullable as.sandbox
  , scope:              toNullable as.scope
  , scoped:             toNullable as.scoped
  , scrolling:          toNullable as.scrolling
  , seamless:           toNullable as.seamless
  , selected:           toNullable as.selected
  , shape:              toNullable as.shape
  , size:               toNullable as.size
  , sizes:              toNullable as.sizes
  , span:               toNullable as.span
  , spellcheck:         toNullable as.spellcheck
  , src:                toNullable as.src
  , srcdoc:             toNullable as.srcdoc
  , srclang:            toNullable as.srclang
  , srcset:             toNullable as.srcset
  , start:              toNullable as.start
  , step:               toNullable as.step
  , style:              toNullable as.style
  , summary:            toNullable as.summary
  , tabindex:           toNullable as.tabindex
  , target:             toNullable as.target
  , title:              toNullable as.title
  , type:               toNullable as.type
  , usemap:             toNullable as.usemap
  , value:              toNullable as.value
  , width:              toNullable as.width
  , wmode:              toNullable as.wmode
  , wrap:               toNullable as.wrap
  , onclick:            toNullable as.onclick
  }

fromJSRecord as =
  { accept:             toMaybe as.accept
  , "accept-charset":   toMaybe as."accept-charset"
  , accesskey:          toMaybe as.accesskey
  , action:             toMaybe as.action
  , allowfullscreen:    toMaybe as.allowfullscreen
  , allowtransparency:  toMaybe as.allowtransparency
  , alt:                toMaybe as.alt
  , async:              toMaybe as.async
  , autocomplete:       toMaybe as.autocomplete
  , autofocus:          toMaybe as.autofocus
  , autoplay:           toMaybe as.autoplay
  , capture:            toMaybe as.capture
  , cellpadding:        toMaybe as.cellpadding
  , cellspacing:        toMaybe as.cellspacing
  , challenge:          toMaybe as.challenge
  , charset:            toMaybe as.charset
  , checked:            toMaybe as.checked
  , cite:               toMaybe as.cite
  , class:              toMaybe as.class
  , colspan:            toMaybe as.colspan
  , cols:               toMaybe as.cols
  , content:            toMaybe as.content
  , contenteditable:    toMaybe as.contenteditable
  , contextmenu:        toMaybe as.contextmenu
  , controls:           toMaybe as.controls
  , coords:             toMaybe as.coords
  , crossorigin:        toMaybe as.crossorigin
  , data:               toMaybe as.data
  , datetime:           toMaybe as.datetime
  , default:            toMaybe as.default
  , defer:              toMaybe as.defer
  , dir:                toMaybe as.dir
  , disabled:           toMaybe as.disabled
  , download:           toMaybe as.download
  , draggable:          toMaybe as.draggable
  , enctype:            toMaybe as.enctype
  , form:               toMaybe as.form
  , formaction:         toMaybe as.formaction
  , formenctype:        toMaybe as.formenctype
  , formmethod:         toMaybe as.formmethod
  , formnovalidate:     toMaybe as.formnovalidate
  , formtarget:         toMaybe as.formtarget
  , frameborder:        toMaybe as.frameborder
  , headers:            toMaybe as.headers
  , height:             toMaybe as.height
  , hidden:             toMaybe as.hidden
  , high:               toMaybe as.high
  , href:               toMaybe as.href
  , hreflang:           toMaybe as.hreflang
  , for:                toMaybe as.for
  , "http-equiv":       toMaybe as."http-equiv"
  , icon:               toMaybe as.icon
  , id:                 toMaybe as.id
  , inputmode:          toMaybe as.inputmode
  , integrity:          toMaybe as.integrity
  , is:                 toMaybe as.is
  , keyparams:          toMaybe as.keyparams
  , keytype:            toMaybe as.keytype
  , kind:               toMaybe as.kind
  , label:              toMaybe as.label
  , lang:               toMaybe as.lang
  , list:               toMaybe as.list
  , loop:               toMaybe as.loop
  , low:                toMaybe as.low
  , manifest:           toMaybe as.manifest
  , marginheight:       toMaybe as.marginheight
  , marginwidth:        toMaybe as.marginwidth
  , max:                toMaybe as.max
  , maxlength:          toMaybe as.maxlength
  , media:              toMaybe as.media
  , mediagroup:         toMaybe as.mediagroup
  , method:             toMaybe as.method
  , min:                toMaybe as.min
  , minlength:          toMaybe as.minlength
  , multiple:           toMaybe as.multiple
  , muted:              toMaybe as.muted
  , name:               toMaybe as.name
  , novalidate:         toMaybe as.novalidate
  , nonce:              toMaybe as.nonce
  , open:               toMaybe as.open
  , optimum:            toMaybe as.optimum
  , pattern:            toMaybe as.pattern
  , placeholder:        toMaybe as.placeholder
  , poster:             toMaybe as.poster
  , preload:            toMaybe as.preload
  , profile:            toMaybe as.profile
  , radiogroup:         toMaybe as.radiogroup
  , readonly:           toMaybe as.readonly
  , rel:                toMaybe as.rel
  , required:           toMaybe as.required
  , reversed:           toMaybe as.reversed
  , role:               toMaybe as.role
  , rowspan:            toMaybe as.rowspan
  , rows:               toMaybe as.rows
  , sandbox:            toMaybe as.sandbox
  , scope:              toMaybe as.scope
  , scoped:             toMaybe as.scoped
  , scrolling:          toMaybe as.scrolling
  , seamless:           toMaybe as.seamless
  , selected:           toMaybe as.selected
  , shape:              toMaybe as.shape
  , size:               toMaybe as.size
  , sizes:              toMaybe as.sizes
  , span:               toMaybe as.span
  , spellcheck:         toMaybe as.spellcheck
  , src:                toMaybe as.src
  , srcdoc:             toMaybe as.srcdoc
  , srclang:            toMaybe as.srclang
  , srcset:             toMaybe as.srcset
  , start:              toMaybe as.start
  , step:               toMaybe as.step
  , style:              toMaybe as.style
  , summary:            toMaybe as.summary
  , tabindex:           toMaybe as.tabindex
  , target:             toMaybe as.target
  , title:              toMaybe as.title
  , type:               toMaybe as.type
  , usemap:             toMaybe as.usemap
  , value:              toMaybe as.value
  , width:              toMaybe as.width
  , wmode:              toMaybe as.wmode
  , wrap:               toMaybe as.wrap
  , onclick:            toMaybe as.onclick
  }