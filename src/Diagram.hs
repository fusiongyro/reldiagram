module Diagram (writeGraph) where

import Types

writeGraph :: [TableLink] -> String
writeGraph links = "digraph G {\n" ++ concatMap writeLink links ++ "}\n"
    where
        writeLink (o `References` d) = "  " ++ o ++ " -> " ++ d ++ ";\n"
