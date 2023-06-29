{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}

import Control.Monad (when)
import Data.List (foldl1', reverse)
import qualified Data.Set as Set
import System.Directory (listDirectory)
import System.Environment (getArgs)
import System.Exit (exitSuccess)
import System.FilePath (takeBaseName)

data Options = Options
  { oDirs :: [FilePath]
  , oShowHelp :: Bool
  }

emptyOptions :: Options
emptyOptions =
  Options
    { oDirs = []
    , oShowHelp = False
    }

main :: IO ()
main = do
  args <- getArgs
  let options = parseArgs emptyOptions args
  when (oShowHelp options) do
    putStrLn "Usage: program <dir1> <dir2> ... [--help]"
    exitSuccess
  case oDirs options of
    [] -> pure ()
    dirs -> do
      fileSets <- mapM getFileNamesWithoutExtensionAsSet dirs
      let intersectionSet = foldl1' Set.intersection fileSets
      mapM_ putStrLn $ Set.toList intersectionSet

parseArgs :: Options -> [String] -> Options
parseArgs o = \case
  [] -> o
  "--help" : rest -> parseArgs o{oShowHelp = True} rest
  dir : rest -> parseArgs o{oDirs = dir : oDirs o} rest

getFileNamesWithoutExtensionAsSet :: FilePath -> IO (Set.Set String)
getFileNamesWithoutExtensionAsSet dir = do
  contents <- listDirectory dir
  pure $ Set.fromList $ map takeBaseName contents
