{-# LANGUAGE BangPatterns #-}

module Portfolio (Weights, calculateAverageAnnualizedReturn, generateRandomWeights, generatePortfolios, calculateAnnualizedVolatility,findBestSharpePortfolio,formatPortfolio,extractTicker) where

import qualified Data.Vector as V
import System.Random
import Control.Parallel.Strategies
import Text.Printf (printf)
import Data.List (isSuffixOf)
import Data.List.Split (splitOn)
import Data.List (subsequences)
import System.Random.Shuffle (shuffleM)

import Returns

type Weights = V.Vector Double

calculateAverageAnnualizedReturn :: V.Vector (V.Vector Double) -> Weights -> Double
calculateAverageAnnualizedReturn returnsMatrix weights
  | V.null returnsMatrix || V.null (returnsMatrix V.! 0) = 0
  | V.length returnsMatrix /= V.length weights = error "Weights and returns matrix row count must match"
  | otherwise =
      let avgReturns = V.map average returnsMatrix
          weightedAvg = V.sum $ V.zipWith (*) avgReturns weights
      in (1 + weightedAvg) ** 252 - 1
  where
    average v = V.sum v / fromIntegral (V.length v)

generateRandomWeights :: Int -> StdGen -> V.Vector Double
generateRandomWeights n gen =
  let tries = iterate (snd . split) gen
      result = head [ normalizeToOne (V.fromList (take n (randomRs (0.0, 0.2) g :: [Double])))
                    | g <- tries
                    , let vec = V.fromList (take n (randomRs (0.0, 0.2) g :: [Double]))
                    , V.sum vec >= 1.0
                    ]
  in result

normalizeToOne :: V.Vector Double -> V.Vector Double
normalizeToOne vec =
  let total = V.sum vec
      scaled = V.map (/ total) vec
      capped = V.map (min 0.2) scaled
      deficit = 1.0 - V.sum capped
  in redistribute capped deficit

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

generatePortfolios :: Int -> Int -> StdGen -> V.Vector (V.Vector Double)
generatePortfolios n k seed =
  let gens = take k $ iterate (snd . split) seed
      portfolios = map (generateRandomWeights n) gens  
  in V.fromList portfolios


calculateAnnualizedVolatility :: V.Vector (V.Vector Double) -> Weights -> Double
calculateAnnualizedVolatility returnsMatrix weights
  | V.null returnsMatrix || V.null (returnsMatrix V.! 0) = 0
  | V.length returnsMatrix /= V.length weights = error "Weights and returns matrix row count must match"
  | otherwise =
      let covMatrix = calculateCovarianceMatrix returnsMatrix
          portfolioVariance = quadraticForm weights covMatrix
          stdDev = sqrt portfolioVariance
          volatAnual = stdDev * sqrt 252
      in volatAnual

-- Calcula a matriz de covariância entre os vetores de retornos
calculateCovarianceMatrix :: V.Vector (V.Vector Double) -> V.Vector (V.Vector Double)
calculateCovarianceMatrix returns =
  let n = V.length returns
      rows = parMap rdeepseq (\i -> V.generate n (\j -> covariance (returns V.! i) (returns V.! j))) [0 .. n-1]
  in V.fromList rows

covariance :: V.Vector Double -> V.Vector Double -> Double
covariance xs ys =
  let meanX = mean xs
      meanY = mean ys
      paired = V.zip xs ys
      covSum = V.sum $ V.map (\(x, y) -> (x - meanX) * (y - meanY)) paired
  in covSum / fromIntegral (V.length xs - 1)

mean :: V.Vector Double -> Double
mean v = V.sum v / fromIntegral (V.length v)

-- Calcula o valor w^T * sigma * w
quadraticForm :: Weights -> V.Vector (V.Vector Double) -> Double
quadraticForm weights sigma =
  V.sum $ V.imap (\i wi ->
            wi * (V.sum $ V.imap (\j wj -> wj * (sigma V.! i V.! j)) weights)
         ) weights

calculateSharpeRatio :: Double -> V.Vector (V.Vector Double) -> Weights -> Double
calculateSharpeRatio riskFreeRate returnsMatrix weights =
  let ret = calculateAverageAnnualizedReturn returnsMatrix weights
      vol = calculateAnnualizedVolatility returnsMatrix weights
  in if vol == 0 then 0 else (ret - riskFreeRate) / vol

findBestSharpePortfolio :: Double -> V.Vector (V.Vector Double) -> V.Vector Weights -> (Weights, Double)
findBestSharpePortfolio riskFreeRate returnsMatrix portfolios =
  V.maximumBy compareSharpe (V.map (\w -> (w, calculateSharpeRatio riskFreeRate returnsMatrix w)) portfolios)
  where
    compareSharpe (_, sharpe1) (_, sharpe2) = compare sharpe1 sharpe2



-- Extrair ticker do nome do arquivo
extractTicker :: FilePath -> String
extractTicker path =
  let name = last (splitOn "/" path)
  in if ".csv" `isSuffixOf` name
       then take (length name - 4) name
       else name

-- Formatar um portfólio bonito
formatPortfolio :: [String] -> Weights -> Double -> Double -> Double -> String
formatPortfolio tickers weights ret vol sharpe =
  let allocations = V.toList $ V.zipWith (\ticker weight -> ticker ++ ": " ++ printf "%.2f%%" (weight * 100)) (V.fromList tickers) weights
      allocationsStr = unlines allocations
      summary = printf "Return: %.2f%%\nVolatility: %.2f%%\nSharpe Ratio: %.2f" (ret * 100) (vol * 100) sharpe
  in allocationsStr ++ summary





  


