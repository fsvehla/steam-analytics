module Scrape
    (
      Purchase (..)
    , purchase
    ) where

import Data.Time.Calendar
import Text.HTML.Scalpel
import Parse

data Purchase = Purchase {
  centAmount :: Int
, date       :: Day
} deriving (Show, Eq)

purchase :: String -> Either String Purchase
purchase s = Purchase <$> cents <*> day
  where
    cents = maybeToEither "No node" amountText >>= Parse.totalCents
    day   = maybeToEither "No node" dateText   >>= Parse.purchaseDate

    amountText :: Maybe String
    amountText = scrapeStringLike s (text amountSelector)

    dateText :: Maybe String
    dateText = scrapeStringLike s (text dateSelector)

    amountSelector :: Selector
    amountSelector = "td" @: [hasClass "wht_total"]

    dateSelector :: Selector
    dateSelector = "td" @: [hasClass "wht_date"]
