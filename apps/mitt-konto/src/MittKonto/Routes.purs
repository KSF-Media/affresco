module MittKonto.Routes where

import Prelude hiding ((/))

import Data.Either (note)
import Data.Generic.Rep (class Generic, NoArguments(..))
import KSF.Api.Subscription (Subsno)
import KSF.Api.Subscription (fromString, toString) as Subsno
import Routing.Duplex (RouteDuplex(..), RouteDuplex', as, end, int, param, prefix, root, segment, suffix)
import Routing.Duplex.Generic as G
import Routing.Duplex.Parser (RouteParser(..), RouteError(..), RouteResult(..))
import Routing.Duplex.Printer as R

data MittKontoRoute
  = InvoiceDetail Int
  | InvoiceList
  | PasswordRecoveryCode String
  | PasswordRecovery
  | CreditCardUpdate Subsno
  | Search
  | MittKonto

derive instance genericRoute :: Generic MittKontoRoute _

-- Consumes or prints a named fragment
hash :: forall a b. String -> RouteDuplex a b -> RouteDuplex a b
hash t (RouteDuplex enc dec) =
  RouteDuplex (\a -> enc a <> R.hash t)
  (dec <* (Chomp \state -> if state.hash == t
                             then Success (state { hash = "" }) NoArguments
                             else Fail (Expected t state.hash)))

-- Refines a codec of Strings to Subsnos
subsno :: RouteDuplex' String -> RouteDuplex' Subsno
subsno = as Subsno.toString (note "no parse as Subsno" <<< Subsno.fromString)

routes :: RouteDuplex' MittKontoRoute
routes = root $ G.sum
  { "InvoiceDetail": "fakturor" `prefix` int segment
  , "InvoiceList": "fakturor" `prefix` end G.noArgs
  , "PasswordRecoveryCode": hash "l%C3%B6senord" $ param "code"
  , "PasswordRecovery": hash "l%C3%B6senord" G.noArgs
  , "CreditCardUpdate": "prenumerationer" `prefix` subsno segment `suffix` "kreditkort" `suffix` "uppdatera"
  , "Search": "sök" `prefix` end G.noArgs
  , "MittKonto": end G.noArgs
  }

needsLogin :: MittKontoRoute -> Boolean
needsLogin PasswordRecovery = false
needsLogin (PasswordRecoveryCode _) = false
needsLogin _ = true
