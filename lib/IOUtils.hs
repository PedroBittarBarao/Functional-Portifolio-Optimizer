{-# LANGUAGE OverloadedStrings #-}

module IOUtils (readStockData, createReturnsMatrix) where

import qualified Data.ByteString.Lazy as BL
import qualified Data.Vector as V
import Data.Csv
import Returns (StockDay(..), calculateReturns)

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

readStockData :: FilePath -> IO (Either String (V.Vector StockDay))
readStockData filePath = do
  csvData <- BL.readFile filePath
  return $ case decodeByName csvData of
    Left err -> Left err
    Right (_, stocks) -> Right stocks

createReturnsMatrix :: V.Vector FilePath -> IO (V.Vector (V.Vector Double))
createReturnsMatrix filePaths = do
  results <- V.mapM readStockData filePaths
  return $ V.map (either (const V.empty) calculateReturns) results
