{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.ByteString.Lazy as BL
import qualified Data.Vector as V
import Data.Csv
import GHC.Generics
import Control.Monad (forM_)
import System.Random

data StockDay = StockDay
  { date        :: !String
  , open        :: !Double  -- Replace Number with Double
  , high        :: !Double
  , low         :: !Double
  , close       :: !Double
  , volume      :: !Int
  , dividends   :: !Double
  , stockSplits :: !Double
  } deriving (Show, Generic)

-- Make it an instance of FromRecord for decoding from CSV
instance FromRecord StockDay 
instance FromNamedRecord StockDay where
  parseNamedRecord r = StockDay
    <$> r .: "Date"
    <*> r .: "Open"
    <*> r .: "High"
    <*> r .: "Low"
    <*> r .: "Close"
    <*> r .: "Volume"
    <*> r .: "Dividends"
    <*> r .: "Stock Splits"

type ReturnsMatrix = V.Vector (V.Vector Double)
type Weights = V.Vector Double

-- Read the CSV file and parse it into a vector of StockDay records
readStockData :: FilePath -> IO (Either String (V.Vector StockDay))
readStockData filePath = do
  csvData <- BL.readFile filePath
  case decodeByName csvData of
    Left err -> return (Left err)
    Right (_, stocks) -> return (Right stocks)

calculateReturns :: V.Vector StockDay -> V.Vector Double
calculateReturns stocks
  | V.length stocks < 2 = V.empty
  | otherwise = V.zipWith calcReturn (V.tail stocks) stocks
  where
    calcReturn today yesterday = (close today / close yesterday) - 1


createReturnsMatrix :: V.Vector FilePath -> IO ReturnsMatrix
createReturnsMatrix filePaths = do
  results <- V.mapM readStockData filePaths
  let returnsList = V.map (either (const V.empty) calculateReturns) results
  return returnsList

calculateAverageAnnualizedReturn :: ReturnsMatrix -> Weights -> Double
calculateAverageAnnualizedReturn returnsMatrix weights
  | V.null returnsMatrix || V.null (returnsMatrix V.! 0) = 0
  | V.length returnsMatrix /= V.length weights = error "Weights and returns matrix row count must match"
  | otherwise =
      let avgReturns = V.map average returnsMatrix
          weightedAvg = V.sum $ V.zipWith (*) avgReturns weights
      in (1 + weightedAvg) ** 252 - 1
  where
    average v = V.sum v / fromIntegral (V.length v)


-- Geração de um único vetor de pesos aleatórios
generateRandomWeights :: Int -> StdGen -> V.Vector Double
generateRandomWeights n gen =
  let tries = iterate (snd . split) gen
      result = head [ normalizeToOne (V.fromList (take n (randomRs (0.0, 0.2) g :: [Double])))
                    | g <- tries
                    , let vec = V.fromList (take n (randomRs (0.0, 0.2) g :: [Double]))
                    , V.sum vec >= 1.0
                    ]
  in result

-- Normaliza o vetor de pesos para que sua soma seja 1
normalizeToOne :: V.Vector Double -> V.Vector Double
normalizeToOne vec =
  let total = V.sum vec
      scaled = V.map (/ total) vec
      capped = V.map (min 0.2) scaled
      deficit = 1.0 - V.sum capped
  in redistribute capped deficit

-- Redistribui a sobra dos pesos mantendo wi <= 0.2
redistribute :: V.Vector Double -> Double -> V.Vector Double
redistribute vec deficit
  | deficit <= 1e-8 = vec
  | otherwise =
      let adjustable = V.findIndices (< 0.2) vec
          totalRoom = V.sum $ V.map (\i -> 0.2 - vec V.! i) adjustable
          updated = V.imap (\i x ->
                      if i `V.elem` adjustable && totalRoom > 0
                        then x + ((0.2 - x) / totalRoom) * deficit
                        else x) vec
          newDeficit = 1.0 - V.sum updated
      in redistribute updated newDeficit

-- Geração de várias carteiras com pesos aleatórios
generatePortfolios :: Int -> Int -> StdGen -> V.Vector (V.Vector Double)
generatePortfolios n k seed =
  let gens = take k $ iterate (snd . split) seed
  in V.fromList $ map (generateRandomWeights n) gens

main :: IO ()
main = do
  let files = V.fromList ["stocks/AAPL.csv", "stocks/AMGN.csv", "stocks/AXP.csv"]
  matrix <- createReturnsMatrix files
  forM_ matrix print

