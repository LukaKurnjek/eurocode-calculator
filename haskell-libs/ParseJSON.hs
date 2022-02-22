
{-
Created on Mon Feb 21 2022

@author: Luka Kurnjek
@license: MIT License
-}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module ParseJSON (
    parseJson
  ) where

import Data.Aeson
import Data.ByteString.Lazy as B
import Data.ByteString.Lazy.Char8 as BC
import GHC.Generics
import Data.Array.ST
import Data.Array.Unboxed
import Data.Maybe (isNothing, fromJust)
import Control.Monad (forM_, when)
import Data.List (intercalate)
import Data.List.Split
import System.Directory (doesFileExist)

type InputParameters = [String]
type OutputParameters = [String]

-- Function that handles the parsing of the JSON file.
parseJson :: InputParameters -> IO OutputParameters
parseJson inputParameters = do
  let typeOfParsing = inputParameters !! 0
  let parameters = Prelude.tail inputParameters
  let functionToCall = case typeOfParsing of
                       "readJSON" -> readJSON
                       "writeJSON" -> writeJSON
                       _ -> (\parameters -> return "Function does not exist.")
  output <- functionToCall parameters
  return [output]

type Parameters = [String]

-- Reads out the JSON file, calls the getCalculatorData function to process
-- the results, transforms them to a string and returns the string.
readJSON :: Parameters -> IO String
readJSON parameters = do
  let calculatorName = parameters !! 0
  jsonData <- returnJSONdata
  let jsonResults = if B.length jsonData > 0
                    then (decode jsonData :: Maybe JSONResults)
                    else (Nothing :: Maybe JSONResults)
  let calculatorList = results <$> jsonResults
  let parametersArray = getCalculatorData calculatorName calculatorList
  paramerersString <- array2String parametersArray
  return paramerersString

type CalculatorName = String

-- Searches the JSON results for the given calculator name, reads out
-- the results and writes them to a UArray Int Double object.
getCalculatorData :: CalculatorName -> Maybe [Results] -> UArray Int Double
getCalculatorData _ Nothing = array (1,12) []
getCalculatorData nameToFind (Just results) = runSTUArray $ do
  parametersArray <- newArray (1,12) 0
  forM_ results $ \result -> do
    let name = calculatorName result
    -- When the name that we a looking for matches we parse out the JSON data.
    when (name == nameToFind) $ do
      let valuesString = calculatorValues result
      let listStrings = Data.List.Split.splitOn "|" valuesString
      let parametersList = Prelude.map read listStrings :: [Double]
      forM_ [1..12] $ \ind -> do
        writeArray parametersArray ind $ parametersList !! (ind - 1)
  return parametersArray

-- Data that represent the JSON data.
data Results = Results
    { calculatorName :: String
    , calculatorValues :: String
    } deriving (Show,Generic)
    
data JSONResults = JSONResults
    { results :: [Results]
    } deriving (Show,Generic)

instance FromJSON Results
instance FromJSON JSONResults
instance ToJSON Results
instance ToJSON JSONResults

-- Transforms a UArray to a String that can be parsed by the python code.
array2String :: UArray Int Double -> IO String
array2String parametersArray = do
  let valuesList = [parametersArray ! 1, parametersArray ! 2, parametersArray ! 3, 
                    parametersArray ! 4, parametersArray ! 5, parametersArray ! 6,
                    parametersArray ! 7, parametersArray ! 8, parametersArray ! 9,
                    parametersArray ! 10, parametersArray ! 11, parametersArray ! 12]
  let valuesString = Data.List.intercalate "|" (Prelude.map show valuesList)
  return valuesString

-- Writes data to the JSON file.
writeJSON :: Parameters -> IO String
writeJSON parameters = do
  let calculatorNameInput = parameters !! 0
  let calculatorData = parameters !! 1
  -- Reads out JSON data.
  jsonData <- returnJSONdata
  let jsonResults = if B.length jsonData > 0
                    then (decode jsonData :: Maybe JSONResults)
                    else (Nothing :: Maybe JSONResults)
  let addedResult = Results calculatorNameInput calculatorData
  if isNothing jsonResults
  -- Creates a new JSON object and writes the data to the JSON file.
  then do
    let updatedJSONResults = JSONResults [addedResult]
    let encodedJson = encode updatedJSONResults
    Prelude.writeFile "../data/saved_parameters" $ BC.unpack encodedJson
  -- Keeps the existing JSON object, adds some data to it and writes it to the JSON file.
  else do
    let jsonResultsRaw = fromJust jsonResults
    let updatedJSONResults = JSONResults $ (results jsonResultsRaw) ++ [addedResult]
    let encodedJson = encode updatedJSONResults
    Prelude.writeFile "../data/saved_parameters" $ BC.unpack encodedJson  
  return "File written."

-- Returs the JSON data. If the file does not exist it returns an empty string.
returnJSONdata :: IO ByteString
returnJSONdata = do
  fileExists <- doesFileExist "../data/saved_parameters"
  if fileExists
  then do
    jsonData <- B.readFile "../data/saved_parameters"
    return jsonData
  else do
    let jsonData = BC.pack ""
    return jsonData







