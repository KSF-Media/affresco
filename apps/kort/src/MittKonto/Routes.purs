module MittKonto.Routes where

import Prelude hiding ((/))

import Bottega.Models.CreditCard (CreditCardId)
import Data.Either (note)
import Data.Generic.Rep (class Generic)
import Data.Newtype (unwrap, wrap)
import Data.Profunctor (dimap)
import KSF.Api.Subscription (Subsno)
import KSF.Api.Subscription (fromString, toString) as Subsno
import Routing.Duplex (RouteDuplex', as, end, int, prefix, root, segment, suffix)
import Routing.Duplex.Generic as G

data MittKontoRoute
  = CreditCardUpdate Subsno CreditCardId
  | MittKonto

derive instance genericRoute :: Generic MittKontoRoute _

-- Refines a codec of Strings to CreditCardId
creditCardId :: RouteDuplex' String -> RouteDuplex' CreditCardId
creditCardId = dimap unwrap wrap <<< int

-- Refines a codec of Strings to Subsnos
subsno :: RouteDuplex' String -> RouteDuplex' Subsno
subsno = as Subsno.toString (note "no parse as Subsno" <<< Subsno.fromString)

routes :: RouteDuplex' MittKontoRoute
routes = root $ G.sum
  { "CreditCardUpdate": "betalkort" `prefix`
      (G.product (subsno segment) (creditCardId segment)) `suffix` "uppdatera"
  , "MittKonto": end G.noArgs
  }
