module Simulation (simulateOneCombination,allCombinations, dow30Tickers) where

import Data.List (subsequences)
import qualified Data.Vector as V
import System.Random

import Portfolio
import IOUtils

dow30Tickers :: [String]
dow30Tickers = ["AAPL", "AMGN", "AMZN","AXP","CRM", "CSCO","CVX","DIS","GS","HD","HON","IBM","JNJ",                       "JPM","KO","MCD","MMM","MRK","MSFT","NKE","NVDA","PG","SHW","TRV","UNH"]

combinations :: Int -> [a] -> [[a]]
combinations 0 _ = [[]]
combinations _ [] = []
combinations k (x:xs) = map (x:) (combinations (k-1) xs) ++ combinations k xs

allCombinations :: [[String]]
allCombinations = combinations 25 dow30Tickers

simulateOneCombination :: [String] -> IO (Weights, Double, Double, Double)
simulateOneCombination tickers = do
  let files = V.fromList (map (\t -> "stocks/" ++ t ++ ".csv") tickers)
  returnsMatrix <- createReturnsMatrix files
  gen <- newStdGen
  let portfolios = generatePortfolios (length tickers) 10 gen -- !!!!!!!!!!!!
  let riskFreeRate = 0.05

  let evaluated = V.map (\w ->
          let ret = calculateAverageAnnualizedReturn returnsMatrix w
              vol = calculateAnnualizedVolatility returnsMatrix w
              sharpe = (ret - riskFreeRate) / vol
          in (w, ret, vol, sharpe)
        ) portfolios

  return $ V.maximumBy (\(_,_,_,s1) (_,_,_,s2) -> compare s1 s2) evaluated