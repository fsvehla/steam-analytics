module Parse
    (
      totalCents
    ) where

import Text.Read as R
import Data.List as DL
import Data.Char (isDigit)

totalCents :: String -> Either String Int
totalCents "" = Left "empty input"
totalCents  s = case elemIndex ',' s of
    Nothing -> Left "Input contained no ','"
    Just  p -> do
      let (integer_s, fractional_s) = splitAt p s

      let euros    = digits integer_s
      let cents    = digits fractional_s
      let unsigned = (+) <$> fmap (* 100) euros <*> cents

      if head s == '-' then
        negate <$> unsigned
      else
        unsigned

digits :: String -> Either String Int
digits "" = Left "Empty input"
digits s  = annotateLeft ("For input '" ++ s ++ "': ") (R.readEither (filter isDigit s) :: Either String Int)

-- Prepends a string to the Left value of an Either String _
annotateLeft :: String -> Either String a -> Either String a
annotateLeft _ (Right a) = Right a
annotateLeft s (Left  v) = Left(s ++ v)
