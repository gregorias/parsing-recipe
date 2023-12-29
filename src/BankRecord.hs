module BankRecord (
  BankRecordB (..),
  BankRecord,
  BankRecordRecipe,
  bankRecordP,
) where

import Barbies (FunctorB (..))
import Barbies.Bare (
  Bare,
  BareB (..),
  Covered,
  Wear,
  bstrip,
 )
import Data.Functor.Barbie (Rec (Rec))
import Data.Time (Day)
import Data.Time.Extra (isoDayP)
import GHC.Generics
import Kitchen (CsvParser, cook)
import Relude
import Text.Megaparsec.Char qualified as MP

data BankRecordB (t :: Type) (f :: Type -> Type) = BankRecordB
  { brbDay :: Wear t f Day
  , brbTitle :: Wear t f Text
  }
  deriving stock (Generic)

instance FunctorB (BankRecordB Covered)

instance BareB BankRecordB

type BankRecordRecipe = BankRecordB Covered CsvParser

type BankRecord = BankRecordB Bare Identity

deriving stock instance Eq BankRecord

deriving stock instance Show BankRecord

bankRecordRecipe :: BankRecordRecipe
bankRecordRecipe = BankRecordB isoDayP (MP.string "Buy" <|> MP.string "Sell")

bankRecordP :: CsvParser BankRecord
bankRecordP = bstrip <$> cooked
 where
  cooked :: CsvParser (BankRecordB Covered Identity)
  cooked = cook bankRecordRecipe
