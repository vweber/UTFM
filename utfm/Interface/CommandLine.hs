module UTFM.Interface.CommandLine where
--
-- Command-line option arguments parser
--
-- Based on code from: http://stackoverflow.com/questions/10786980/parsing-command-line-arguments-in-haskell-using-getopt
-- 

import Control.Monad
import Control.Monad.Error
import System.Console.GetOpt
import System.Environment(getArgs, getProgName)

data Options = Options {
    optType :: FilePath
  , optConf :: FilePath
  , optProd :: FilePath
  , optUnfl :: Bool
  , optProp :: Bool
  , optFull :: Bool
  } deriving Show

defaultOptions = Options {
    optType = ""
  , optConf = ""
  , optProd = ""
  , optUnfl = False
  , optProp = False
  , optFull = False
  }

printOptions :: Options -> IO ()
printOptions options
  = putStrLn $ show options

options :: [OptDescr (Options -> Either String Options)]
options =
  [ Option ['t'] ["type"]
      (ReqArg (\t opts -> Right opts { optType = t }) "<file.utfm>")
      "path to typedeclaration"
  , Option ['c'] ["conf"]
      (ReqArg (\c opts -> Right opts { optConf = c }) "<file.utfmc>")
      "path to configuration"
  , Option ['p'] ["prod"]
      (ReqArg (\p opts -> Right opts { optProd = p }) "<file.utfmd>")
      "path to product"
  , Option ['u'] [""]
      (NoArg (\opts -> Right opts { optUnfl = True }))
      "unfold configuration"
  , Option ['r'] [""]
      (NoArg (\opts -> Right opts { optProp = True }))
      "propagate configuration"
  , Option ['f'] [""]
      (NoArg (\opts -> Right opts { optFull = True }))
      "map to product (full configuration)"
  ]

parseArgs :: IO Options
parseArgs = do
  argv <- getArgs
  progName <- getProgName
  let header = "Usage: " ++ progName ++ " [OPTION...]"
  let helpMessage = usageInfo header options
  let (actions, nonOptions, errors) = getOpt RequireOrder options argv
  case (actions, nonOptions, errors) of
    (opts, [], []) ->
      case foldM (flip id) defaultOptions opts of
        Right opts -> return opts
        Left errorMessage -> ioError (userError (errorMessage ++ "\n" ++ helpMessage))
    (_, _, errs) -> ioError (userError (concat errs ++ helpMessage))