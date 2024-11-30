{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.ByteString.Lazy as BL
import qualified Data.Vector as V
import Data.Csv
import Database.SQLite.Simple
import Database.SQLite.Simple.FromRow

data Airline = Airline
    { airline_name :: String
    , airline_iata :: String      
    } deriving (Show)
instance ToRow Airline where
    toRow (Airline c1, c2) = [SQLText (fromString c1), SQLText (fromString c2)]

data Airport = Airport
    { airport_name :: String
    , country :: String
    , airport_iata :: String      
    } deriving (Show)
instance ToRow Airport where
    toRow (Airport c1, c2, c3) = [SQLText (fromString c1), SQLText (fromString c2), SQLText (fromString c3)]
    
readCSVWithDrop :: FromRecord a => FilePath -> Int -> IO (V.Vector a)
readCSVWithDrop filePath n = do
    csvData <- BL.readFile filePath
    case decode NoHeader csvData of
        Left err -> error err
        Right v  -> return $ V.drop n v

dataAirlines :: FilePath
dataAirlines = "././airlines.csv"

dataAirports :: FilePath
dataAirports = "././airports.csv"

main :: IO ()
main = do
  -- Read and parse the first CSV file
  airlineCsv <- readCSVWithDrop dataAirlines 2 :: IO (V.Vector (String, String, String, String, String, String, String))
  let processedAirlineCsv = V.map (\(_, name, _, iata, _, _, _, _) -> Airline name iata) airlineCsv

  -- Read and parse the second CSV file
  airportCsv <- readCSVWithDrop dataAirports 0 :: IO (V.Vector (String, String, String, String, String, String, String, String, String, String, String, String, String))
  let processedAirportCsv = V.map (\(_, name, _, country, iata, _, _, _) -> Airport name country iata) airportCsv
 
  -- Connect to SQLite database and write data
  conn <- open "././AppDatabase.sqlite"
  execute_ conn "CREATE TABLE IF NOT EXISTS airlines (id INTEGER PRIMARY KEY, name TEXT NOT NULL, iata_code TEXT NOT NULL)"
  execute_ conn "CREATE TABLE IF NOT EXISTS airports (id INTEGER PRIMARY KEY, name TEXT NOT NULL, countrt TEXT NOT NULL, iata_code TEXT NOT NULL)"
  execute_ conn "DELETE FROM airlines" -- Clear existing data
  execute_ conn "DELETE FROM airports" -- Clear existing data
  executeMany conn "INSERT INTO airlines (name, iata_code) VALUES (?,?)" processedAirlineCsv
  executeMany conn "INSERT INTO airports (name, country, iata_code) VALUES (?,?,?)" processedAirposrtCsv
  close conn

  putStrLn "Data written to database successfully."
