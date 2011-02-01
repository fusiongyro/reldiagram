module Types(TableLink(..)) where

import Data.ByteString

data TableLink = ByteString `References` ByteString
    deriving (Show)

