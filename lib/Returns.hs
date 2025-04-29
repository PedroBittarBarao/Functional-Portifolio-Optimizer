{-# LANGUAGE DeriveGeneric #-}

module Returns (StockDay(..), calculateReturns, ReturnsMatrix) where

import qualified Data.Vector as V
import GHC.Generics

data StockDay = StockDay
  { date        :: !String
  , open        :: !Double
  , high        :: !Double
  , low         :: !Double
  , close       :: !Double
  , volume      :: !Int
  , dividends   :: !Double
  , stockSplits :: !Double
  } deriving (Show, Generic)

type ReturnsMatrix = V.Vector (V.Vector Double)

calculateReturns :: V.Vector StockDay -> V.Vector Double
calculateReturns stocks
  | V.length stocks < 2 = V.empty
  | otherwise = V.zipWith calcReturn (V.tail stocks) stocks
  where
    calcReturn today yesterday = (close today / close yesterday) - 1
