{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.ByteString.Lazy as BL
import qualified Data.Csv as Csv
import qualified Data.Vector as V
import System.IO (withFile, IOMode(WriteMode))
import System.Random (newStdGen, StdGen)
import Control.Monad (forM)
import Data.List (intercalate, maximumBy)
import Data.Time.Clock (getCurrentTime, diffUTCTime)
import Text.Printf (printf)
import qualified Data.Map as Map

import Returns
import IOUtils
import Portfolio
import Simulation
import PortfolioRow

formatDoubles :: [Double] -> String
formatDoubles = intercalate ";" . map (printf "%.4f")

processCombination :: ([String], StdGen) -> Map.Map String (V.Vector StockDay) -> IO PortfolioRow
processCombination (tickersList, seed) stockCache = do
  -- Look up stock data in the cache and calculate returns (using `calculateReturns`)
  let returns = V.map (\ticker -> 
                        maybe V.empty calculateReturns (Map.lookup ticker stockCache)) 
                      (V.fromList tickersList)

  -- Filter out any combinations that failed to return data (empty returns)
  let validReturns = V.filter (not . V.null) returns
  
  -- If we have no valid data, return a default row
  if V.null validReturns 
    then return (PortfolioRow "" "" 0 0 0)
    else do
      -- Calculate portfolio statistics
      let portfolios = generatePortfolios 27 20 seed
          stats = V.map (\w -> 
                    let anRet = calculateAverageAnnualizedReturn validReturns w
                        volat = calculateAnnualizedVolatility validReturns w
                        shrp = if volat == 0 then 0 else anRet / volat
                    in PortfolioRow
                         (intercalate ";" tickersList)
                         (formatDoubles (V.toList w))
                         anRet
                         volat
                         shrp
                  ) portfolios
          best = V.maximumBy (\a b -> compare (sharpe a) (sharpe b)) stats
      return best


main :: IO ()
main = do
  start <- getCurrentTime

  let combs = allCombinations

  -- Load all stock data files at once
  stockCache <- loadAllStockData (concat combs)

  -- Process the combinations sequentially
  results <- forM combs $ \tickersList -> do
    seed <- newStdGen
    processCombination (tickersList, seed) stockCache

  -- Find the best portfolio
  let bestOverall = maximumBy (\a b -> compare (sharpe a) (sharpe b)) results

  -- Write the result to the CSV file
  withFile "best_portfolio.csv" WriteMode $ \h -> do
    BL.hPut h (Csv.encodeDefaultOrderedByName [bestOverall])

  end <- getCurrentTime
  putStrLn $ "Tempo total: " ++ show (diffUTCTime end start)
