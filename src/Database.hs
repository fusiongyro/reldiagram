module Database (getRelationsForConnection) where

import Control.Applicative
import Data.Maybe

import Database.HDBC
import Database.HDBC.PostgreSQL

import Types

allRelationsSQL :: String
allRelationsSQL = "SELECT \n" ++
                    "  tc.table_name as owning_table,\n" ++
                    "  ctu.table_name as destination_table\n" ++
                    "FROM\n" ++
                    "  information_schema.table_constraints tc,\n" ++
                    "  information_schema.constraint_table_usage ctu\n" ++
                    "WHERE\n" ++
                    "  constraint_type = 'FOREIGN KEY' AND\n" ++ 
                    "  ctu.constraint_name = tc.constraint_name"

allLinks :: Connection -> IO [TableLink]
allLinks db = mapMaybe parseRow <$> quickQuery' db allRelationsSQL []
    where
        parseRow [owner, destination] = Just $ fromSql owner `References` fromSql destination
        parseRow _ = Nothing

getRelationsForConnection :: String -> IO [TableLink]
getRelationsForConnection = flip withPostgreSQL allLinks