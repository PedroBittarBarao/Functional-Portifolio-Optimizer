{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.ByteString.Lazy as BL
import qualified Data.Csv as Csv
import Control.Monad (forM_)
import qualified Data.Vector as V
import System.IO (withFile, IOMode(WriteMode))
import Data.List (intercalate)
import Text.Printf (printf)
import System.Random (getStdGen)

import Returns
import IOUtils
import Portfolio
import Simulation
import PortfolioRow

formatDoubles :: [Double] -> String
formatDoubles = intercalate ";" . map (printf "%.4f")

main :: IO ()
main = do
  let files = V.fromList (map (\t -> "stocks/" ++ t ++ ".csv") dow30Tickers)
  returnsList <- createReturnsMatrix files
  let fullMatrix = zip dow30Tickers (V.toList returnsList)

  seed <- getStdGen

  let combs = allCombinations
  withFile "best_portfolios.csv" WriteMode $ \h -> do
    forM_ combs $ \tickersList -> do
      let returns = selectReturnsMatrix tickersList fullMatrix
          portfolios = generatePortfolios 25 1000 seed
          stats = V.map (\w -> 
                    let ret  = calculateAverageAnnualizedReturn returns w
                        vol  = calculateAnnualizedVolatility returns w
                        shrp = if vol == 0 then 0 else ret / vol
                    in PortfolioRow
                          (intercalate ";" tickersList)
                          (formatDoubles (V.toList w))
                          ret
                          vol
                          shrp
                  ) portfolios
      BL.hPut h (Csv.encodeDefaultOrderedByName (V.toList stats))