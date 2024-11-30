{-# LANGUAGE OverloadedStrings #-}

module MainSpec where

import Test.Hspec
import qualified Data.Vector as V
import qualified Data.ByteString.Lazy as BL
import Data.Csv
import Database.SQLite.Simple
import Main -- Import the main module

-- Sample CSV data for testing
sampleAirlineCsv :: BL.ByteString
sampleAirlineCsv = "id,name,alias,iata,icao,callsign,country,active\n1,Airline1,,A1,ICAO1,Callsign1,Country1,Y\n2,Airline2,,A2,ICAO2,Callsign2,Country2,N\n"

sampleAirportCsv :: BL.ByteString
sampleAirportCsv = "id,name,city,country,iata,icao,latitude,longitude,altitude,time_offset,dst,type,source\n1,Airport1,City1,Country1,AP1,ICAO1,0,0,0,0,E,airport,manual\n2,Airport2,City2,Country2,AP2,ICAO2,0,0,0,0,E,airport,manual\n"

-- Helper function to decode CSV
decodeSample :: FromRecord a => BL.ByteString -> V.Vector a
decodeSample csvData = case decode NoHeader csvData of
    Left err -> error err
    Right v  -> v

main :: IO ()
main = hspec $ do
    describe "readCSVWithDrop" $ do
        it "parses and drops rows correctly for airlines" $ do
            let airlines = decodeSample sampleAirlineCsv :: V.Vector (String, String, String, String, String, String, String, String)
            let filteredAirlines = V.filter (\(_, _, _, iata, _, _, _, _) -> iata /= "\\N" && not (null iata)) airlines
            let processedAirlines = V.map (\(_, name, _, iata, _, _, _, _) -> Airline name iata) filteredAirlines
            V.length processedAirlines `shouldBe` 2
            processedAirlines `shouldBe` V.fromList [Airline "Airline1" "A1", Airline "Airline2" "A2"]

        it "parses and drops rows correctly for airports" $ do
            let airports = decodeSample sampleAirportCsv :: V.Vector (String, String, String, String, String, String, String, String, String, String, String, String, String, String)
            let filteredAirports = V.filter (\(_, _, _, _, iata, _, _, _, _, _, _, _, _, _) -> iata /= "\\N" && not (null iata)) airports
            let processedAirports = V.map (\(_, name, _, country, iata, _, _, _, _, _, _, _, _, _) -> Airport name country iata) filteredAirports
            V.length processedAirports `shouldBe` 2
            processedAirports `shouldBe` V.fromList [Airport "Airport1" "Country1" "AP1", Airport "Airport2" "Country2" "AP2"]

    describe "SQLite database operations" $ do
        it "creates tables and inserts data successfully" $ do
            conn <- open ":memory:" -- Use an in-memory database for testing
            execute_ conn "CREATE TABLE airlines (id INTEGER PRIMARY KEY, name TEXT NOT NULL, iata_code TEXT NOT NULL)"
            execute_ conn "CREATE TABLE airports (id INTEGER PRIMARY KEY, name TEXT NOT NULL, country TEXT NOT NULL, iata_code TEXT NOT NULL)"

            let airlines = V.fromList [Airline "Airline1" "A1", Airline "Airline2" "A2"]
            let airports = V.fromList [Airport "Airport1" "Country1" "AP1", Airport "Airport2" "Country2" "AP2"]

            executeMany conn "INSERT INTO airlines (name, iata_code) VALUES (?,?)" (V.toList airlines)
            executeMany conn "INSERT INTO airports (name, country, iata_code) VALUES (?,?,?)" (V.toList airports)

            [Only airlineCount] <- query_ conn "SELECT COUNT(*) FROM airlines" :: IO [Only Int]
            [Only airportCount] <- query_ conn "SELECT COUNT(*) FROM airports" :: IO [Only Int]

            airlineCount `shouldBe` 2
            airportCount `shouldBe` 2

            close conn
