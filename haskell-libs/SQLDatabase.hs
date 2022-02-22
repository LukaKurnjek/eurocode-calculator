
{-
Created on Mon Feb 21 2022

@author: Luka Kurnjek
@license: MIT License
-}

-- Is needed for the execute command to accept a string.
{-# LANGUAGE OverloadedStrings #-}

module SQLDatabase (
    parseDatabase
  ) where

import Data.List (intercalate)
import Control.Applicative
import Database.SQLite.Simple
import Database.SQLite.Simple.FromRow
import Control.Monad (when)

type InputParameters = [String]
type OutputParameters = [String]

-- Function that handles SQL database processing.
parseDatabase :: InputParameters -> IO OutputParameters
parseDatabase inputParameters = do
  let typeOfParsing = inputParameters !! 0
  let parameters = if length inputParameters == 1
                   then []
                   else Prelude.tail inputParameters
  let functionToCall = case typeOfParsing of
                       "readSQL" -> readSQL
                       "writeSQL" -> writeSQL
                       "deleteSQL" -> deleteSQL
                       "createSQLDatabase" -> createDB
                       _ -> (\parameters -> return "Function does not exist.")
  output <- functionToCall parameters
  return [output]
  
type Parameters = [String]

-- Creates the SQL database.
createDB :: Parameters -> IO String
createDB parameters = do
  conn <- open "../data/database.db"
  execute_ conn "CREATE TABLE IF NOT EXISTS bookmarks (id INTEGER PRIMARY KEY, tab INTEGER, eurocode TEXT, chapter TEXT, equation TEXT)"
  return "Created SQL database"

-- Reads out the entire SQL database and returns the result as a string.
readSQL :: Parameters -> IO String
readSQL parameters = do
  conn <- open "../data/database.db"
  resp <- query_ conn "SELECT * FROM bookmarks;" :: IO [Bookmark]
  return $ show resp

-- Writes to the SQL database only if the parameters do not allready exist in the database. 
writeSQL :: Parameters -> IO String
writeSQL parameters = do
  let tab = parameters !! 0
  let eurocode = parameters !! 1
  let chapter = parameters !! 2
  let equation = parameters !! 3
  -- Reads the database and checkes if the parameters allready exist in the database.
  conn <- open "../data/database.db"
  resp <- query conn "SELECT * FROM bookmarks WHERE tab = (?) AND eurocode = (?) AND chapter = (?) AND equation = (?);"
            (tab,eurocode,chapter,equation) :: IO [Bookmark]
  -- If the parameters do not exists it performs a write to the SQL database.
  if (show resp) == "[]"
  then do
    execute conn "INSERT INTO bookmarks (tab,eurocode,chapter,equation) VALUES (?,?,?,?)"
      (tab,eurocode,chapter,equation)
    return "Written to SQL database."
  else do
    return "Item allready exists."

-- Deletes parameters from the SQL database. If less parameters are give it deletes all entries
-- in the database that match the given input parameters.
deleteSQL :: Parameters -> IO String
deleteSQL parameters = do
  let parametersLength = length parameters
  let tab = parameters !! 0
  let eurocode = parameters !! 1
  let chapter = if parametersLength > 2
                then parameters !! 2
                else ""
  let equation = if parametersLength > 3
                 then parameters !! 3
                 else ""
  conn <- open "../data/database.db"
  case parametersLength of
    2 -> execute conn "DELETE FROM bookmarks WHERE (tab,eurocode) = (?,?)"
           (tab,eurocode)
    3 -> execute conn "DELETE FROM bookmarks WHERE (tab,eurocode,chapter) = (?,?,?)"
           (tab,eurocode,chapter)
    4 -> execute conn "DELETE FROM bookmarks WHERE (tab,eurocode,chapter,equation) = (?,?,?,?)"
           (tab,eurocode,chapter,equation)
  return "Deleting from SQL database."

-- Type for the SQL database element.
data Bookmark = Bookmark
  { idInp :: Int
  , tabInp :: Int
  , eurocodeInp :: String
  , chapterInp :: String
  , equationInp :: String
  }

instance Show Bookmark where
  show bookmark = mconcat [ show $ tabInp bookmark
                          , "|"
                          , eurocodeInp bookmark
                          , "|"
                          , chapterInp bookmark
                          , "|"
                          , equationInp bookmark]

instance FromRow Bookmark where
  fromRow = Bookmark <$> field
                     <*> field
                     <*> field
                     <*> field
                     <*> field


