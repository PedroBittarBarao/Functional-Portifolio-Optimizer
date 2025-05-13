{-# LANGUAGE OverloadedStrings #-}

module IOUtils (readStockData, createReturnsMatrix,loadAllStockData) where

import qualified Data.ByteString.Lazy as BL
import qualified Data.Vector as V
import qualified Data.Map as Map
import Data.Csv
import Returns (StockDay(..), calculateReturns)
import Control.Concurrent.Async (mapConcurrently)
import Control.Monad (forM)
import Data.Either (either)


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
  pure $ case decodeByName csvData of
    Left err -> Left err
    Right (_, stocks) -> Right stocks

-- Function to load all stock data at once
loadAllStockData :: [String] -> IO (Map.Map String (V.Vector StockDay))
loadAllStockData tickersList = do
  -- Using V.forM to process each ticker
  stockData <- V.forM (V.fromList tickersList) $ \ticker -> do
    let filePath = "stocks/" ++ ticker ++ ".csv"
    stock <- readStockData filePath
    -- Handle the Either case using 'either'
    return (ticker, either (const V.empty) id stock)  -- Use empty vector on error
  -- Convert the list to a Map
  return (Map.fromList (V.toList stockData))


createReturnsMatrix :: V.Vector FilePath -> IO (V.Vector (V.Vector Double))
createReturnsMatrix filePaths = do
  results <- V.fromList <$> mapConcurrently readStockData (V.toList filePaths)
  return $ V.map (either (const V.empty) calculateReturns) results

