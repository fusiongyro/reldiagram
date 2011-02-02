{-# LANGUAGE PatternGuards #-}

module Control.Monad.Utils (readM) where

readM :: (Monad m, Read a) => String -> m a
readM s | [x] <- parse = return x
        | otherwise    = fail $ "Failed to parse \"" ++ s ++ "\" as a number."
  where
    parse = [x | (x,[]) <- reads s]
