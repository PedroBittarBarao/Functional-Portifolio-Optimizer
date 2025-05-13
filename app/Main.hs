{-# LANGUAGE OverloadedStrings #-}


import Data.List.Split (chunksOf)
import qualified Data.ByteString.Lazy as BL
import qualified Data.Csv as Csv
import System.IO (withFile, IOMode(..))
import Control.Monad (forM, forM_)
import Control.Concurrent.Async (mapConcurrently)
import Data.Time.Clock (getCurrentTime, diffUTCTime)
import System.Random (newStdGen,StdGen)

import qualified Data.Vector as V
import qualified Data.Map as Map
import Data.List(intercalate)
import Text.Printf(printf)
import Data.List (maximumBy)

import Returns
import IOUtils
import Portfolio
import Simulation
import PortfolioRow

batchSize :: Int
batchSize = 200  -- Ajuste para controlar uso de memória

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
      let portfolios = generatePortfolios 25 1000 seed
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
      batches = chunksOf batchSize combs

  allResults <- withFile "best_portfolios.csv" WriteMode $ \h -> do
    -- Escreve cabeçalho CSV
    let header = Csv.encodeDefaultOrderedByName ([] :: [PortfolioRow])  -- Garante que o cabeçalho vem da instância ToNamedRecord
    BL.hPut h header

    -- Para acumular todos os resultados
    fmap concat $
      forM batches $ \batch -> do
        let tickersInBatch = concat batch
        stockCache <- loadAllStockData tickersInBatch

        -- Processa as combinações do batch em paralelo
        results <- mapConcurrently (\tickersList -> do
                                      seed <- newStdGen
                                      processCombination (tickersList, seed) stockCache
                                   ) batch

        -- Escreve os resultados no CSV
        BL.hPut h (Csv.encodeDefaultOrderedByName results)
        return results

  -- Calcula o melhor portfólio após todos os batches
  let bestOverall = maximumBy (\a b -> compare (sharpe a) (sharpe b)) allResults

  -- Escreve o melhor portfólio em um arquivo separado
  BL.writeFile "best_overall_portfolio.csv" $
    Csv.encodeDefaultOrderedByName [bestOverall]

  end <- getCurrentTime
  putStrLn $ "Tempo total: " ++ show (diffUTCTime end start)
  putStrLn "Melhor portfólio salvo em best_overall_portfolio.csv"

