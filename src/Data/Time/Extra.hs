-- Various Day parsers.
module Data.Time.Extra (
  isoDayP,
  usVerboseDayP,
) where

import Data.List (lookup)
import Data.Time (Day, MonthOfYear, defaultTimeLocale, fromGregorianValid, parseTimeM)
import Relude
import Relude.Unsafe qualified as Unsafe
import Text.Megaparsec (Parsec)
import Text.Megaparsec qualified as MP
import Text.Megaparsec.Char (digitChar)
import Text.Megaparsec.Char qualified as MP
import Text.Megaparsec.Char.Lexer qualified as MP

-- | Parses "YYYY-MM-DD" format.
isoDayP :: (Ord e) => Parsec e Text Day
isoDayP = MP.label "date" $ do
  (dayStr, _) <-
    MP.match . MP.try $ do
      MP.label "YYYY" $ replicateM_ 4 digitChar
      sepP
      MP.label "MM" $ replicateM_ 2 digitChar
      sepP
      MP.label "DD" $ replicateM_ 2 digitChar
  parseTimeM False defaultTimeLocale "%F" (toString dayStr)
 where
  sepP :: (Ord e) => Parsec e Text ()
  sepP = void $ MP.string "-"

-- | Parsers strings like "May 4, 2023" into a day.
usVerboseDayP :: (Ord e) => Parsec e Text Day
usVerboseDayP = MP.label "date in US format" $ do
  (dateString, (month, day, year)) <- MP.match . MP.try $ do
    month <- monthP
    void $ MP.single ' '
    day <- MP.decimal
    void $ MP.single ',' >> MP.single ' '
    year <- MP.decimal
    return (month, day, year)
  let maybeDay = fromGregorianValid year month day
  maybe
    (fail $ "Could not parse " <> toString dateString <> " as a valid date.")
    return
    maybeDay

monthAssocList :: [(Text, MonthOfYear)]
monthAssocList =
  [ ("January", 1)
  , ("February", 2)
  , ("March", 3)
  , ("April", 4)
  , ("May", 5)
  , ("June", 6)
  , ("July", 7)
  , ("August", 8)
  , ("September", 9)
  , ("October", 10)
  , ("November", 11)
  , ("December", 12)
  ]

months :: [Text]
months = fst <$> monthAssocList

parseMonth :: Text -> Maybe MonthOfYear
parseMonth = flip lookup monthAssocList

monthP :: (Ord e) => Parsec e Text MonthOfYear
monthP = do
  month <- MP.choice $ fmap MP.string months
  -- fromJust is unsafe, but the parse above guarantees the key's presence.
  -- Unfortunately, proving key presence on the type is too complex, so I think
  -- this solution is reasonable.
  return . Unsafe.fromJust . parseMonth $ month
