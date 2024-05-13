module MittKonto.Routes where

import Prelude hiding ((/))

import Bottega.Models.CreditCard (CreditCardId)
import Data.Generic.Rep (class Generic)
import Data.Newtype (unwrap, wrap)
import Data.Profunctor (dimap)
import Routing.Duplex (RouteDuplex', end, int, prefix, root, segment, suffix)
import Routing.Duplex.Generic as G

data MittKontoRoute
  = CreditCardUpdate CreditCardId
  | MittKonto

derive instance genericRoute :: Generic MittKontoRoute _

-- Refines a codec of Strings to CreditCardId
creditCardId :: RouteDuplex' String -> RouteDuplex' CreditCardId
creditCardId = dimap unwrap wrap <<< int

routes :: RouteDuplex' MittKontoRoute
routes = root $ G.sum
  { "CreditCardUpdate": "betalkort" `prefix` creditCardId segment `suffix` "uppdatera"
  , "MittKonto": end G.noArgs
  }
