module Control.Exception.Utils (tryIO, exceptIO) where

import Control.Applicative ((<$>))
import Control.Exception (try, IOException)
import Control.Monad (MonadPlus(..), mzero, mplus)

tryIO :: IO a -> IO (Either IOException a)
tryIO = try

exceptIO :: (MonadPlus m) => IO a -> IO (m a)
exceptIO f = either (const mzero) (mplus mzero . return) <$> tryIO f
