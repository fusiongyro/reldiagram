module SQL where

import Control.Applicative
import Control.Monad
import Data.ByteString
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

queryDB :: Connection -> IO [TableLink]
queryDB db = mapMaybe parseRow <$> quickQuery db allRelationsSQL []
    where
        parseRow [SqlByteString owner, SqlByteString destination] = Just $ owner `References` destination
        parseRow _ = Nothing

