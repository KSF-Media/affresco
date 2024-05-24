module MittKonto.Routes where

import Prelude hiding ((/))

import Bottega.Models (CreditCardId, CreditCardRegisterNumber)
import Data.Either (note)
import Data.Generic.Rep (class Generic, NoArguments(..))
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap, wrap)
import Data.Profunctor (dimap)
import KSF.Api.Subscription (Subsno)
import KSF.Api.Subscription (fromString, toString) as Subsno
import Routing.Duplex (RouteDuplex(..), RouteDuplex', as, end, int, param, prefix, prop, record, root, segment, suffix)
import Routing.Duplex.Generic as G
import Routing.Duplex.Parser (RouteParser(..), RouteError(..), RouteResult(..))
import Routing.Duplex.Printer as R
import Type.Proxy (Proxy(..))

type CreditCardCallbackParams =
  { registerNumber :: CreditCardRegisterNumber
  , registerCardId :: CreditCardId
  , transactionId :: String
  , responseCode :: String
  }

data MittKontoRoute
  = InvoiceDetail Int
  | InvoiceList
  | PasswordRecoveryCode String
  | PasswordRecoveryCode2 String
  | PasswordRecovery
  | PasswordRecovery2
  | PasswordRecovery3
  | CreditCardUpdate Subsno
  | CreditCardCallback Subsno CreditCardCallbackParams
  | Search
  | Paywall
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

registerNumber :: RouteDuplex' String -> RouteDuplex' CreditCardRegisterNumber
registerNumber = dimap unwrap wrap

registerCardId :: RouteDuplex' String -> RouteDuplex' CreditCardId
registerCardId = dimap unwrap wrap <<< int

routes :: RouteDuplex' MittKontoRoute
routes = root $ G.sum
  { "InvoiceDetail": "fakturor" `prefix` int segment
  , "InvoiceList": "fakturor" `prefix` end G.noArgs
  , "PasswordRecoveryCode": hash "l%C3%B6senord" $ param "code"
  , "PasswordRecoveryCode2": hash "losenord" $ param "code"
  , "PasswordRecovery": hash "l%C3%B6senord" G.noArgs
  , "PasswordRecovery2": hash "losenord" G.noArgs
  , "PasswordRecovery3": hash "l%F6senord" G.noArgs
  , "CreditCardUpdate": "betalkort" `prefix` subsno segment `suffix` "uppdatera"
  , "CreditCardCallback":
      (G.product
       (subsno $ param "subsno")
       (record
        # prop (Proxy :: _ "registerNumber") (registerNumber $ param "order")
        # prop (Proxy :: _ "registerCardId") (registerCardId $ param "cardId")
        # prop (Proxy :: _ "transactionId") (param "transactionId")
        # prop (Proxy :: _ "responseCode") (param "responseCode")))
  , "Search": "sök" `prefix` end G.noArgs
  , "Paywall": "betalvägg" `prefix` end G.noArgs
  , "MittKonto": end G.noArgs
  }

needsLogin :: MittKontoRoute -> Boolean
needsLogin PasswordRecovery = false
needsLogin PasswordRecovery2 = false
needsLogin PasswordRecovery3 = false
needsLogin (PasswordRecoveryCode _) = false
needsLogin (PasswordRecoveryCode2 _) = false
needsLogin (CreditCardCallback _ _) = false
needsLogin _ = true

creditCardCallbackParams :: MittKontoRoute -> Maybe CreditCardCallbackParams
creditCardCallbackParams (CreditCardCallback _ x) = Just x
creditCardCallbackParams _ = Nothing
