module Kitchen (
  CsvParser,
  cook,
) where

import GHC.Generics (
  K1 (K1),
  M1 (M1),
  (:*:) ((:*:)),
 )
import GHC.Generics qualified as Generic
import Relude
import Text.Megaparsec (Parsec)
import Text.Megaparsec.Char qualified as MP

type CsvParser = Parsec Void Text

-- | A class of generics that can be cooked.
class GCsvCookable (repRecipe :: k -> Type) (repFood :: k -> Type) where
  gCook :: repRecipe a -> CsvParser (repFood a)

-- Ignore metadata we encounter.
instance
  (GCsvCookable recipe food) =>
  GCsvCookable (M1 i c recipe) (M1 i c food)
  where
  gCook (M1 x) = M1 <$> gCook x

-- The moment we encounter a product type (i.e., fields of a record),
-- separate them with comma during parsing.
instance
  ( GCsvCookable recipe0 food0
  , GCsvCookable recipe1 food1
  ) =>
  GCsvCookable (recipe0 :*: recipe1) (food0 :*: food1)
  where
  gCook (a :*: b) = do
    ra <- gCook a
    void $ MP.char ','
    rb <- gCook b
    return $ ra :*: rb

-- The moment we encounter a field, parse it with the field's recipe.
instance GCsvCookable (K1 Generic.R (CsvParser food)) (K1 Generic.R (Identity food)) where
  gCook (K1 a) = K1 . Identity <$> a

-- | Lift a type into its generic implementation
gCookDefault ::
  ( Generic recipe
  , Generic food
  , GCsvCookable (Generic.Rep recipe) (Generic.Rep food)
  ) =>
  recipe ->
  CsvParser food
gCookDefault recipe = Generic.to <$> gCook (Generic.from recipe)

cook ::
  ( Generic (recordHkt CsvParser)
  , Generic (recordHkt Identity)
  , GCsvCookable (Generic.Rep (recordHkt CsvParser)) (Generic.Rep (recordHkt Identity))
  ) =>
  recordHkt CsvParser ->
  CsvParser (recordHkt Identity)
cook = gCookDefault
