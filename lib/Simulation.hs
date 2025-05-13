module Simulation (allCombinations, dow30Tickers) where

import Data.List (subsequences)
import qualified Data.Vector as V
import System.Random

import Portfolio
import IOUtils

import Control.Parallel.Strategies (parListChunk, using, rdeepseq)


dow30Tickers :: [String]
dow30Tickers = ["AAPL", "AMGN", "AMZN","AXP","BA","CAT","CRM","CSCO","CVX","DIS","GS","HD","HON","IBM","JNJ","JPM","KO","MCD","MMM","MRK","MSFT","NKE","NVDA","PG","SHW","TRV","UNH","V","VZ","WMT"]

-- Gera combinações de k elementos sem gerar todas as subsequences
combinations :: Int -> [a] -> [[a]]
combinations 0 _  = [[]]
combinations _ [] = []
combinations k (x:xs)
  | k < 0     = []
  | otherwise = map (x:) (combinations (k-1) xs) ++ combinations k xs

-- Versão paralela
allCombinations :: [[String]]
allCombinations = 
  let base = ["AAPL", "AMGN", "AMZN","AXP","BA","CAT","CRM","CSCO","CVX","DIS","GS","HD","HON","IBM","JNJ","JPM","KO","MCD","MMM","MRK","MSFT","NKE","NVDA","PG","SHW","TRV","UNH","V","VZ","WMT"]
      chunked = combinations 27 base
  in chunked `using` parListChunk 500 rdeepseq

