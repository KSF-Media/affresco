module MittKonto.Routes where

import Prelude hiding ((/))

import Data.Either (note)
import Data.Generic.Rep (class Generic)
import KSF.Api.Subscription (Subsno)
import KSF.Api.Subscription (fromString, toString) as Subsno
import Routing.Duplex (RouteDuplex', as, end, prefix, root, segment, suffix)
import Routing.Duplex.Generic as G

data MittKontoRoute
  = CreditCardUpdate Subsno
  | MittKonto

derive instance genericRoute :: Generic MittKontoRoute _

-- Refines a codec of Strings to Subsnos
subsno :: RouteDuplex' String -> RouteDuplex' Subsno
subsno = as Subsno.toString (note "no parse as Subsno" <<< Subsno.fromString)

routes :: RouteDuplex' MittKontoRoute
routes = root $ G.sum
  { "CreditCardUpdate": "prenumerationer" `prefix` subsno segment `suffix` "kreditkort" `suffix` "uppdatera"
  , "MittKonto": end G.noArgs
  }
