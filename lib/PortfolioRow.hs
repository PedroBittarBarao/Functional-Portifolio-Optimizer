{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module PortfolioRow where

import GHC.Generics (Generic)
import Data.Csv (DefaultOrdered, ToNamedRecord, ToRecord)
import Control.DeepSeq (NFData)

data PortfolioRow = PortfolioRow
  { tickers :: String
  , weights :: String
  , ret     :: Double
  , vol     :: Double
  , sharpe  :: Double
  } deriving (Show, Generic, NFData, DefaultOrdered, ToNamedRecord, ToRecord)
