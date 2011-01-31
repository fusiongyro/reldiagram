{-# LANGUAGE PatternGuards #-}

module Options ( Options(..)
               , parseOptions
               , optionsToConnectionString
               ) where

import Control.Applicative
import Control.Monad
import Control.Exception

import System
import System.Console.GetOpt
import System.Posix.User

{-
args: databases. if no databases are specified, then the PGDATABASE environment variable is used.
-}

data Options = Options {
                 dbHost                   :: String
               , dbPort                   :: Integer
               , dbUsername               :: String
               , dbPassword               :: String
               , tablesToExamine          :: [String]
               , schemas                  :: [String]
               , databasesToExamine       :: [String]
               } deriving (Show)

defaultOptions :: IO Options
defaultOptions = do
    username <- getEffectiveUserName
    return Options {
      dbHost = "localhost"
    , dbPort = 5432
    , dbUsername = username
    , dbPassword = ""
    , tablesToExamine = []
    , databasesToExamine = []
    , schemas = []
    }

flags :: IO [OptDescr (Options -> IO Options)]
flags = do
  username <- getEffectiveUserName
  return [ Option [] ["help"]      (NoArg  showHelp)                "show this help, then exit"
         , Option [] ["version"]   (NoArg  showVersion)             "show version information, then exit"
         , Option "h" ["host"]     (ReqArg hostOption "HOST")       "database server host or socket directory (default: \"local socket\")"
         , Option "p" ["port"]     (ReqArg portOption "PORT")       "database server port (default: \"5432\")"
         , Option "U" ["username"] (ReqArg userOption "USERNAME") $ "database user name (default: \"" ++ username ++ "\")"
         , Option "t" ["table"]    (ReqArg tableOption "TABLE")     "dump the named table(s) only"
         , Option "n" ["schema"]   (ReqArg schemaOption "SCHEMA")   "dump the named schema(s) only"
         ]

showHelp, showVersion :: Options -> IO Options
showHelp    opts = do
  fl <- flags
  prog <- getProgName
  putStrLn $ usageInfo ("Usage: " ++ prog ++ " [OPTIONS]... DATABASE...") fl
  _ <- exitWith ExitSuccess
  return opts
showVersion opts = putStrLn "reldiagram version 0.1" >> exitWith ExitSuccess >> return opts

hostOption, portOption, userOption, tableOption, schemaOption :: String -> Options -> IO Options
hostOption   h opts = return $ opts { dbHost = h }
portOption   p opts = maybe failParse parsePort (readM p)
    where
      failParse = do
        putStrLn $ "Unable to parse port \"" ++ p ++ "\""
        _ <- exitFailure
        return opts
      parsePort port = return $ opts { dbPort = port }
userOption   u opts = return $ opts { dbUsername = u }
tableOption  t opts = return $ opts { tablesToExamine = tablesToExamine opts ++ [t] }
schemaOption n opts = return $ opts { schemas = schemas opts ++ [n] }

optionsToConnectionString :: Options -> String
optionsToConnectionString opts = undefined

parseOptions :: [String] -> IO Options
parseOptions args = do
  options <- flags
  case getOpt RequireOrder options args of
    (opts, args,   []) -> foldl (>>=) defaultOptions opts
    (   _,    _, errs) -> putStrLn "error" >> exitFailure >> defaultOptions

exceptIO :: (MonadPlus m) => IO a -> IO (m a)
exceptIO f = either (const mzero) (mplus mzero . return) <$> tryIO f

getEnvM :: (MonadPlus m) => String -> IO (m String)
getEnvM s = either (const mzero) (mplus mzero . return) <$> tryIO (getEnv s)

tryIO :: IO a -> IO (Either IOException a)
tryIO = try

readM :: (Monad m, Read a) => String -> m a
readM s | [x] <- parse = return x
        | otherwise    = fail $ "Failed to parse \"" ++ s ++ "\" as a number."
  where
    parse = [x | (x,[]) <- reads s]
