module Types(TableLink(..)) where

data TableLink = String `References` String
    deriving (Show)

