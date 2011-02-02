module Types(TableLink(..)) where

import Data.ByteString

data TableLink = String `References` String
    deriving (Show)

