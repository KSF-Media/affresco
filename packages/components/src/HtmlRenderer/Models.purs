module KSF.HtmlRenderer.Models where

import Data.Maybe (Maybe)
import React.Basic (JSX)

type Hook = 
  { replaceChildren   :: Boolean
  , shouldProcessNode :: Node -> Boolean
  , processNode       :: Node -> Array Node -> String -> JSX
  }

newtype Node = Node
  { raw      :: String
  , data     :: String
  , type     :: String
  , name     :: Maybe String
  , attribs  :: Maybe Attributes
  , children :: Maybe (Array Node)
  }

type Attributes =
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