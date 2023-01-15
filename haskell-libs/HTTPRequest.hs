
{-
Created on Mon Feb 21 2022

@author: Luka Kurnjek
@license: MIT License
-}

{-# LANGUAGE OverloadedStrings #-}
module HTTPRequest (
    httpRequest
  ) where

import Data.Bits (xor)
import Data.List (isInfixOf)
import Data.Time (getCurrentTime, formatTime, defaultTimeLocale)
import Network.HTTP.Simple
import Data.Digest.Pure.MD5 (md5)
import qualified Data.ByteString.Lazy as LB
import qualified Data.ByteString.Char8 as BC
import Control.Exception (IOException, handle)

type InputParameters = [String]
type OutputParameters = [String]

-- Function that handles the registration processes.
httpRequest :: InputParameters -> IO OutputParameters
httpRequest inputParameters = do
  let registrationCode = inputParameters !! 0
  let apiPath = BC.pack $ "/keys/" ++ registrationCode
  let request = buildRequest myHost "GET" apiPath
  output <- makeGETRequest request registrationCode
  return [output]

-- The host where the REST API is located.
myHost :: BC.ByteString
myHost = "127.0.0.1"

-- Function that builds the HTTP request.
buildRequest :: BC.ByteString -> BC.ByteString -> BC.ByteString -> Request
buildRequest host method path = setRequestMethod method
                                $ setRequestHost host
                                $ setRequestPath path
                                $ setRequestSecure False
                                $ setRequestPort 3000
                                $ defaultRequest

-- Function that performs the REST call and handles the response.
type RegistrationCode = String
makeGETRequest :: Request -> RegistrationCode -> IO String
makeGETRequest request registrationCode = do
  -- Makes the HTTP request.
  response <- httpLBS request
  let status = getResponseStatusCode response
  -- If status is Ok (200) write the response to a json file.
  if status == 200
  then do
    let jsonBody = getResponseBody response
    LB.writeFile "../data/response.json" jsonBody
    jsonString <- readFile "../data/response.json"
    -- Checks if the registration code is part of the response.
    -- If yes create the registration file.
    if (registrationCode `isInfixOf` jsonString)
    then do
      currentDay <- getCurrentDate
      createRegistrationFiles currentDay
      return $ show status
    else do
      return "Invalid code."
  -- Handles the case of another response then 200.
  else if status == 404
       then do
         writeFile "response.json" "404: Not found"
         return $ show status
       else do 
         writeFile "response.json" "Connection issue"
         return "ConnectionFailure"

-- Gets the current date as a string and adds one year to it.
getCurrentDate :: IO String
getCurrentDate = do
   now <- getCurrentTime
   let currentDate = read $ formatTime defaultTimeLocale "%Y%m%d" now
   return $ show (currentDate + 10000)

-- Creates a file containing the date one year from now (registration validity)
-- that gets encoded with the xor function and the pad "Haskell1".
createRegistrationFiles :: String -> IO ()
createRegistrationFiles currentDay = do
  writeFile "../data/registrationDay" $ encodeString currentDay
  createMD5Sum

-- Functions for encoding a string with xor and the pad "Haskell1".
stringToNumber :: String -> [Int]
stringToNumber myString = map fromEnum myString

numberToString :: [Int] -> String
numberToString myNumbers = map toEnum myNumbers

myPad :: String
myPad = "Haskell1"

myPadNumber :: [Int]
myPadNumber = stringToNumber myPad

encodeString :: String -> String
encodeString myString = numberToString encodedNumbers
  where myStringNumber = stringToNumber myString
        encodedNumbers = zipWith xor myStringNumber myPadNumber

-- Function that creates a md5sum of the registrationDay file.
createMD5Sum :: IO ()
createMD5Sum = do
    fileContent <- LB.readFile "../data/registrationDay"
    let md5Digest = md5 fileContent
    writeFile "../data/registrationDayCheck" $ show md5Digest








