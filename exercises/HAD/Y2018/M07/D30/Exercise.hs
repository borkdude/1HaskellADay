module Y2018.M07.D30.Exercise where

import Data.Set (Set, empty, fromList
                , difference, intersection)

-- We have two databases

pilot, entities, exDir :: FilePath
exDir = "Y2018/M07/D30/"
pilot = "pilot_db.csv"
entities = "entities_db.csv"

-- We want to merge those databases into one SUPER-database

-- Read in the tables from each database (mind the header information) and do
-- a set difference. What are the names of the tables that both databases share?
-- How many tables are common between both databases?

type Table = String
type Database = Set Table

readFile' :: FilePath -> IO String
readFile' fp = readFile ("exercises/HAD/" ++ exDir ++ fp)

readLines' :: FilePath -> IO [String]
readLines' = fmap lines . readFile' 

readDatabase :: FilePath -> IO Database
readDatabase fp = do
  headers <- readLines' fp
  let tables = drop 2 headers
  return $ fromList tables

diffTables :: Database -> Database -> Set Table
diffTables = difference

sharedTables :: Database -> Database -> Set Table
sharedTables = intersection

main = do
  pilotDb <- readDatabase pilot
  entityDB <- readDatabase entities
  let shared = sharedTables pilotDb entityDB
  print ("diff", diffTables pilotDb entityDB)
  print ("shared", shared)
  print ("shared count: ", length shared)
