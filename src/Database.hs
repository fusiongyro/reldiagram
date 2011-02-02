module Database where

import Control.Applicative
import Control.Monad
import Data.ByteString
import Data.Maybe

import Database.HDBC
import Database.HDBC.PostgreSQL
import Data.Encoding

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
        parseRow [SqlByteString owner, SqlByteString destination] = 
            Just $ (decode UTF8 owner) `References` (decode UTF8 destination)
        parseRow _ = Nothing


getPassword :: IO String
getPassword = do
  putStr "Password: "
  hFlush stdout
  pass <- withEcho False getLine
  putChar '\n'
  return pass

withEcho :: Bool -> IO a -> IO a
withEcho echo action = do
  old <- hGetEcho stdin
  bracket_ (hSetEcho stdin echo) (hSetEcho stdin old) action