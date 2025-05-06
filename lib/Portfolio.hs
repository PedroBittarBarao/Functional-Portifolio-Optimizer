{-# LANGUAGE BangPatterns #-}

module Portfolio (Weights, calculateAverageAnnualizedReturn, generateRandomWeights, generatePortfolios) where

import qualified Data.Vector as V
import System.Random
import Control.Parallel.Strategies

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
      portfolios = parMap rdeepseq (generateRandomWeights n) gens
  in V.fromList portfolios
