module Diagram (writeGraph) where

import Types

writeGraph :: [TableLink] -> IO ()
writeGraph links = putStr $ "digraph G {\n" ++ concatMap writeLink links ++ "}\n"
    where
        writeLink (o `References` d) = "  " ++ o ++ " -> " ++ d ++ ";\n"
