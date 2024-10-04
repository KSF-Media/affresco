module MittKonto.Routes where

import Prelude hiding ((/))

import Bottega.Models (CreditCardId, CreditCardRegisterNumber)
import Data.Either (note)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap, wrap)
import Data.Profunctor (dimap)
import KSF.Api.Subscription (Subsno)
import KSF.Api.Subscription (fromString, toString) as Subsno
import Routing.Duplex (RouteDuplex', as, end, int, param, prefix, prop, record, root, segment, suffix)
import Routing.Duplex.Generic as G
import Type.Proxy (Proxy(..))

type CreditCardCallbackParams =
  { registerNumber :: CreditCardRegisterNumber
  , registerCardId :: CreditCardId
  , transactionId :: String
  , responseCode :: String
  }

data MittKontoRoute
  = CreditCardUpdate Subsno
  | CreditCardCallback Subsno CreditCardCallbackParams
  | MittKonto

derive instance genericRoute :: Generic MittKontoRoute _

-- Refines a codec of Strings to Subsnos
subsno :: RouteDuplex' String -> RouteDuplex' Subsno
subsno = as Subsno.toString (note "no parse as Subsno" <<< Subsno.fromString)

registerNumber :: RouteDuplex' String -> RouteDuplex' CreditCardRegisterNumber
registerNumber = dimap unwrap wrap

registerCardId :: RouteDuplex' String -> RouteDuplex' CreditCardId
registerCardId = dimap unwrap wrap <<< int

routes :: RouteDuplex' MittKontoRoute
routes = root $ G.sum
  { "CreditCardUpdate": "betalkort" `prefix` subsno segment `suffix` "uppdatera"
  , "CreditCardCallback":
      (G.product
       (subsno $ param "subsno")
       (record
        # prop (Proxy :: _ "registerNumber") (registerNumber $ param "order")
        # prop (Proxy :: _ "registerCardId") (registerCardId $ param "cardId")
        # prop (Proxy :: _ "transactionId") (param "transactionId")
        # prop (Proxy :: _ "responseCode") (param "responseCode")))
  , "MittKonto": end G.noArgs
  }

needsLogin :: MittKontoRoute -> Boolean
needsLogin (CreditCardCallback _ _) = false
needsLogin _ = true

creditCardCallbackParams :: MittKontoRoute -> Maybe CreditCardCallbackParams
creditCardCallbackParams (CreditCardCallback _ x) = Just x
creditCardCallbackParams _ = Nothing
