module Parse
    (
      totalCents,
      purchaseDate,
      maybeToEither
    ) where

import Text.Read as R
import Data.List as DL
import Data.Char (isDigit)
import Data.Time.Calendar
import Data.Time.Format as DF
import Data.Time.Clock

totalCents :: String -> Either String Int
totalCents "" = Left "empty input"
totalCents  s = case elemIndex ',' s of
    Nothing -> Left "Input contained no ','"
    Just  p -> do
      let (integer_s, fractional_s) = splitAt p s

      let euros    = digits integer_s
      let cents    = digits $ if take 3 fractional_s == ",--" then "00" else fractional_s
      let unsigned = (+) <$> fmap (* 100) euros <*> cents

      if head s == '-' then
        negate <$> unsigned
      else
        unsigned

-- todo: Figure out why an Either in parseTimeM will throw instead of filling in the left.

purchaseDate :: String -> Either String Day
purchaseDate s = do
  let utcTime  = DF.parseTimeM True DF.defaultTimeLocale "%e %b, %Y" s :: Maybe Day
  maybeToEither ("Unable to parse '" ++ s ++ "' as date" ) utcTime

digits :: String -> Either String Int
digits "" = Left "Empty input"
digits s  = annotateLeft ("For input '" ++ s ++ "': ") (R.readEither (filter isDigit s) :: Either String Int)

-- Prepends a string to the Left value of an Either String _
annotateLeft :: String -> Either String a -> Either String a
annotateLeft _ (Right a) = Right a
annotateLeft s (Left  v) = Left(s ++ v)

maybeToEither :: l -> Maybe a -> Either l a
maybeToEither _ (Just a) = Right a
maybeToEither l Nothing  = Left l
