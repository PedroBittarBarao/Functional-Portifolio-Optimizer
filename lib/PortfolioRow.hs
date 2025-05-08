{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module PortfolioRow where

import GHC.Generics (Generic)
import Data.Csv (ToRecord, ToNamedRecord, DefaultOrdered)

data PortfolioRow = PortfolioRow
  { tickers    :: String
  , weights    :: String
  , ret        :: Double
  , volatility :: Double
  , sharpe     :: Double
  } deriving (Generic, Show)

instance ToRecord PortfolioRow
instance ToNamedRecord PortfolioRow
instance DefaultOrdered PortfolioRow
