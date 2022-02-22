
{-
Created on Mon Feb 21 2022

@author: Luka Kurnjek
@license: MIT License
-}

module Main (main) where

-- My own libraries:
import HTTPRequest (httpRequest)
import ParseJSON (parseJson)
import SQLDatabase (parseDatabase)

-- Haskells libraries:
import System.Environment (getArgs)


main :: IO ()
main = do
  inputArguments <- getArgs
  
  -- The first argument specifies the function name that will be called.
  let functionToCall = inputArguments !! 0
  let parameters = if length (inputArguments) > 1
                   then tail inputArguments
                   else []

  -- Calls the function that is specified in the input parameters.
  let calledFunction = case functionToCall of
                       "HTTPRequest" -> httpRequest
                       "ParseJSON" -> parseJson
                       "ParseDatabase" -> parseDatabase
                       _ -> (\parameters -> return ["Function does not exist."])
  outputResult <- calledFunction parameters
  
  -- Prints the output string of the called function.
  print outputResult


