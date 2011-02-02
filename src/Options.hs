module Options ( getConnectionStrings
               ) where

import Control.Monad
import Data.Maybe
import System
import System.Console.GetOpt
import System.Posix.User (getEffectiveUserName)
import Text.Printf

import Control.Monad.Utils
import Control.Exception.Utils

data Options = Options {
                 dbHost                   :: String   -- the host to connect to
               , dbPort                   :: Integer  -- the port on the host to connect to
               , dbUsername               :: String   -- the username of this user to auth with
               , dbPassword               :: String   -- the password for this user
               , tablesToExamine          :: [String] -- the list of tables to examine
               , schemas                  :: [String] -- the list of schemas to examine
               , databasesToExamine       :: [String] -- the list of databases to examine
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
userOption   u opts  = return $ opts { dbUsername = u }
tableOption  t opts  = return $ opts { tablesToExamine = tablesToExamine opts ++ [t] }
schemaOption n opts  = return $ opts { schemas = schemas opts ++ [n] }

optionsToConnectionStrings :: Options -> [String]
optionsToConnectionStrings (Options { dbHost = host
                                    , dbPort = port
                                    , dbUsername = user
                                    , dbPassword = password
                                    , databasesToExamine = dbs }) =
    map render dbs
        where
            render = printf "host=%s port=%d user=%s password=%s dbname=%s" host port user password

parseOptions :: [String] -> IO Options
parseOptions args = do
  options <- flags
  case getOpt RequireOrder options args of
    (opts, args',   []) -> foldl (>>=) defaultOptions opts >>= addArgs args' >>= postParsing
    (   _,     _, errs) -> putStrLn (concat errs) >> exitFailure >> defaultOptions

addArgs :: [String] -> Options -> IO Options
addArgs args opts = return opts { databasesToExamine = args }

postParsing :: Options -> IO Options
postParsing opts@(Options {databasesToExamine = []}) = do
    env <- exceptIO (getEnv "PGDATABASE")
    userName <- getEffectiveUserName
    return opts { databasesToExamine = [fromJust $ env `mplus` Just userName]}
postParsing opts = return opts

getConnectionStrings :: IO [String]
getConnectionStrings = do
    args <- getArgs
    options <- parseOptions args
    return $ optionsToConnectionStrings options