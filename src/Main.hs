module Main where

import Database
import Diagram
import Options

main :: IO ()
main = do
    connections <- getConnectionStrings
    graphs <- mapM getRelationsForConnection connections
    mapM_ writeGraph graphs