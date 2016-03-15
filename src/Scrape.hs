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
purchase s = Purchase <$> fmap sign cents <*> day
  where
    sign :: Int -> Int
    sign x = case refundNode of
      Just _  -> negate x
      Nothing -> x

    cents = maybeToEither "No node" amountText >>= Parse.totalCents
    day   = maybeToEither "No node" dateText   >>= Parse.purchaseDate

    amountText :: Maybe String
    amountText = scrapeStringLike s amountScraper

    dateText :: Maybe String
    dateText = scrapeStringLike s dateScraper

    refundNode :: Maybe String
    refundNode = scrapeStringLike s refundScraper

amountScraper :: Scraper String String
amountScraper  = text $ "td"  @: [hasClass "wht_total"]

dateScraper :: Scraper String String
dateScraper    = text $ "td"  @: [hasClass "wht_date"]

refundScraper :: Scraper String String
refundScraper  = html $ "div" @: [hasClass "wth_item_refunded"]
