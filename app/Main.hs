{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.ByteString.Lazy as BL
import qualified Data.Vector as V
import Data.Csv
import GHC.Generics
import Control.Monad (forM_)
import System.Random
import Text.Printf (printf)

import Returns
import IOUtils
import Portfolio

formatPortfolio :: V.Vector Double -> String
formatPortfolio portfolio =
  "[" ++ (concat . V.toList $ V.map (\w -> printf "%.5f" w ++ ", ") portfolio) ++ "]"


main :: IO ()
main = do
  let files = V.fromList ["stocks/AAPL.csv", "stocks/AMGN.csv", "stocks/AMZN.csv","stocks/AXP.csv","stocks/CRM.csv", "stocks/CSCO.csv","stocks/CVX.csv","stocks/DIS.csv",
                          "stocks/GS.csv","stocks/HD.csv","stocks/HON.csv","stocks/IBM.csv","stocks/JNJ.csv",
                          "stocks/JPM.csv","stocks/KO.csv","stocks/MCD.csv","stocks/MMM.csv","stocks/MRK.csv",
                          "stocks/MSFT.csv","stocks/NKE.csv","stocks/NVDA.csv","stocks/PG.csv","stocks/SHW.csv",
                          "stocks/TRV.csv","stocks/UNH.csv"]
  
  -- Cria e imprime a matriz de retornos
  matrix <- createReturnsMatrix files
  --putStrLn "Matriz de retornos:"
  --forM_ matrix print
  
  -- Gera e imprime 10 portfólios com 25 pesos cada
  --putStrLn "\nPortfólios gerados:"
  gen <- getStdGen
  let portfolios = generatePortfolios 25 10 gen
  --V.forM_ portfolios (putStrLn . formatPortfolio)

  -- Calcula e imprime o retorno médio anualizado de cada portfólio
  putStrLn "\nRetornos médios anualizados:"
  let returns = V.map (calculateAverageAnnualizedReturn matrix) portfolios
  V.forM_ returns print



