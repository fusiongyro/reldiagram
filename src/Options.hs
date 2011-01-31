module Options ( Options(..)
               , parseOptions
               , optionsToConnectionString
               ) where

import System.Console.GetOpt
import System.Environment
import System.Posix.User

{-

Our options facility is designed to match PostgreSQL's dump/dumpall options
fairly closely. So, to that end, we support the following options:

--help      show this help, then exit
--version   show version information, then exit
-h --host=HOSTNAME   database server host or socket directory (default: "local socket")
-p --port=PORT   database server port (default: "5432")
-U, --username=USERNAME  database user name (default: "CURRENTLY-LOGGED-IN-USER")

-t, --table=TABLE  dump the named table(s) only
-n, --schema=SCHEMA        dump the named schema(s) only

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
    return $ Options {
      dbHost = "localhost"
    , dbPort = 5432
    , dbUsername = username
    , dbPassword = ""
    , tablesToExamine = []
    , databasesToExamine = []
    , schemas = []
    }

flags :: [OptDescr (Options -> IO Options)]
flags = [ Option [] ["help"]      (NoArg  showHelp)              "show this help, then exit"
        , Option [] ["version"]   (NoArg  showVersion)           "show version information, then exit"
        , Option "h" ["host"]     (ReqArg hostOption "HOST")     "database server host or socket directory (default: \"local socket\")"
        , Option "p" ["port"]     (ReqArg portOption "PORT")     "database server port (default: \"5432\")"
        , Option "U" ["username"] (ReqArg userOption "USERNAME") "database user name (default: \"CURRENTLY-LOGGED-IN-USER\")"
        , Option "t" ["table"]    (ReqArg tableOption "TABLE")   "dump the named table(s) only"
        , Option "n" ["schema"]   (ReqArg schemaOption "SCHEMA") "dump the named schema(s) only"
        ]

showHelp, showVersion :: Options -> IO Options
showHelp    = undefined
showVersion = undefined

hostOption, portOption, userOption, tableOption, schemaOption :: String -> Options -> IO Options
hostOption   = undefined
portOption   = undefined
userOption   = undefined
tableOption  = undefined
schemaOption = undefined

optionsToConnectionString :: Options -> String
optionsToConnectionString opts = undefined

parseOptions :: [String] -> IO Options
parseOptions = undefined