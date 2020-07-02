module Data.Generic.Rep.RecordToSum where

import Prelude

import Data.Generic.Rep (class Generic, Constructor(..), Argument(..), Sum(..))
import Data.Generic.Rep as Generic
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Nullable (Nullable, toMaybe)
import Data.Symbol (class IsSymbol, SProxy(..), reflectSymbol)
import Prim.Symbol (class Cons)
import Record as Record
import Prim.Row as Row
import Control.Alt ((<|>))

{- |

Converts from a Record with Nullable fields to any Sum type with
single-argument constructors that has a Generic instance.

Example: if we have a type like

```
data Fruit = Apple String | Banana String | Cherry Int
derive instance genericFruit :: Generic Fruit _
```

..then we can have a record with Nullable fields like this one:

```
type FruitNullable =
  { apple :: Nullable String
  , banana :: Nullable String
  , cherry :: Nullable Int
  }
```

..and use `toSum` to convert from `FruitNullable` to `Fruit`

-}
toSum
  :: forall a rep row
  .  Generic a rep
  => RecordToSum row rep
  => Record row -> Either String a
toSum = map Generic.to <<< recordToSum

class RecordToSum row rep where
  recordToSum :: Record row -> Either String rep

-- | Base case: single constructor
instance recordToSumConstructor ::
  ( IsSymbol name
  , IsSymbol nameLower
  , SymbolFirstToLower name nameLower
  , Row.Cons nameLower (Nullable a) tail row
  )
  => RecordToSum row (Constructor name (Argument a)) where
  recordToSum x = case toMaybe field of
    Nothing -> Left ("Did not find field " <> show ctorLower <> " in the record")
    Just val -> Right (Constructor $ Argument val)
    where
      ctorLower = reflectSymbol (SProxy :: SProxy nameLower)

      field :: Nullable a
      field = Record.get (SProxy :: SProxy nameLower) x

-- | A simple Sum with two leaf constructors
instance recordToSumLeafSum ::
  ( RecordToSum row (Constructor nameL valL)
  , RecordToSum row (Constructor nameR valR)
  )
  => RecordToSum row (Sum (Constructor nameL valL) (Constructor nameR valR)) where
  recordToSum x = map Inl leftAttempt <|> map Inr rightAttempt
    where
      leftAttempt :: Either String (Constructor nameL valL)
      leftAttempt = recordToSum x

      rightAttempt :: Either String (Constructor nameR valR)
      rightAttempt = recordToSum x

-- | The recursive case with a mid-tree Sum
instance recordToSumTreeSum ::
  ( RecordToSum row (Constructor nameL valL)
  , RecordToSum row (Sum left right)
  ) => RecordToSum row (Sum (Constructor nameL valL) (Sum left right)) where
  recordToSum x = map Inl leftAttempt <|> map Inr rightAttempt
    where
      leftAttempt :: Either String (Constructor nameL valL)
      leftAttempt = recordToSum x

      rightAttempt :: Either String (Sum left right)
      rightAttempt = recordToSum x


-- | Utility class to de-capitalize Sum type constructors so that we can
--   match them to record fields
class SymbolFirstToLower (sym :: Symbol) (res :: Symbol) |  sym -> res

instance symbolFirstToLower ::
  ( Cons head tail input
  , SymbolLetterToLower head headLower
  , Cons headLower tail out
  )
  => SymbolFirstToLower input out

class SymbolLetterToLower (sym :: Symbol) (res :: Symbol) | sym -> res
instance symLettera :: SymbolLetterToLower "a" "a"
instance symLetterb :: SymbolLetterToLower "b" "b"
instance symLetterc :: SymbolLetterToLower "c" "c"
instance symLetterd :: SymbolLetterToLower "d" "d"
instance symLettere :: SymbolLetterToLower "e" "e"
instance symLetterf :: SymbolLetterToLower "f" "f"
instance symLetterg :: SymbolLetterToLower "g" "g"
instance symLetterh :: SymbolLetterToLower "h" "h"
instance symLetteri :: SymbolLetterToLower "i" "i"
instance symLetterj :: SymbolLetterToLower "j" "j"
instance symLetterk :: SymbolLetterToLower "k" "k"
instance symLetterl :: SymbolLetterToLower "l" "l"
instance symLetterm :: SymbolLetterToLower "m" "m"
instance symLettern :: SymbolLetterToLower "n" "n"
instance symLettero :: SymbolLetterToLower "o" "o"
instance symLetterp :: SymbolLetterToLower "p" "p"
instance symLetterq :: SymbolLetterToLower "q" "q"
instance symLetterr :: SymbolLetterToLower "r" "r"
instance symLetters :: SymbolLetterToLower "s" "s"
instance symLettert :: SymbolLetterToLower "t" "t"
instance symLetteru :: SymbolLetterToLower "u" "u"
instance symLetterv :: SymbolLetterToLower "v" "v"
instance symLetterw :: SymbolLetterToLower "w" "w"
instance symLetterx :: SymbolLetterToLower "x" "x"
instance symLettery :: SymbolLetterToLower "y" "y"
instance symLetterz :: SymbolLetterToLower "z" "z"
instance symLetterA :: SymbolLetterToLower "A" "a"
instance symLetterB :: SymbolLetterToLower "B" "b"
instance symLetterC :: SymbolLetterToLower "C" "c"
instance symLetterD :: SymbolLetterToLower "D" "d"
instance symLetterE :: SymbolLetterToLower "E" "e"
instance symLetterF :: SymbolLetterToLower "F" "f"
instance symLetterG :: SymbolLetterToLower "G" "g"
instance symLetterH :: SymbolLetterToLower "H" "h"
instance symLetterI :: SymbolLetterToLower "I" "i"
instance symLetterJ :: SymbolLetterToLower "J" "j"
instance symLetterK :: SymbolLetterToLower "K" "k"
instance symLetterL :: SymbolLetterToLower "L" "l"
instance symLetterM :: SymbolLetterToLower "M" "m"
instance symLetterN :: SymbolLetterToLower "N" "n"
instance symLetterO :: SymbolLetterToLower "O" "o"
instance symLetterP :: SymbolLetterToLower "P" "p"
instance symLetterQ :: SymbolLetterToLower "Q" "q"
instance symLetterR :: SymbolLetterToLower "R" "r"
instance symLetterS :: SymbolLetterToLower "S" "s"
instance symLetterT :: SymbolLetterToLower "T" "t"
instance symLetterU :: SymbolLetterToLower "U" "u"
instance symLetterV :: SymbolLetterToLower "V" "v"
instance symLetterW :: SymbolLetterToLower "W" "w"
instance symLetterX :: SymbolLetterToLower "X" "x"
instance symLetterY :: SymbolLetterToLower "Y" "y"
instance symLetterZ :: SymbolLetterToLower "Z" "z"