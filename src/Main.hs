{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Replace case with fromMaybe" #-}
{-# HLINT ignore "Redundant fmap" #-}
{-# HLINT ignore "Redundant pure" #-}
{-# HLINT ignore "Use forM_" #-}
{-# HLINT ignore "Use if" #-}

import Control.Monad (filterM, forM_, when)
import Data.Char (isSpace)
import Data.List (foldl1')
import qualified Data.Set as Set
import System.Directory (canonicalizePath, copyFile, createFileLink, doesDirectoryExist, listDirectory)
import System.Environment (getArgs)
import System.Exit (exitSuccess)
import System.FilePath (normalise, takeBaseName, takeExtension, (</>))
import System.IO (readFile)
import System.Process (callCommand)
import System.Random (randomRIO)

data Options = Options
  { oInputs :: [FilePath]
  , oRandomOutput :: Maybe FilePath
  , oPrint :: Bool
  , oSeed :: Maybe Int
  , oShowHelp :: Bool
  }

emptyOptions :: Options
emptyOptions =
  Options
    { oInputs = []
    , oRandomOutput = Nothing
    , oPrint = False
    , oSeed = Nothing
    , oShowHelp = False
    }

displayHelp :: IO ()
displayHelp = do
  putStrLn "Usage: program [OPTIONS]"
  putStrLn "  --input <dir/file>    Specify an input directory or file."
  putStrLn "                        Directories are scanned for files."
  putStrLn "                        Text files are read where each line is a base filename."
  putStrLn "                        This option can be used multiple times."
  putStrLn "                        The intersection of base filenames among all inputs is used."
  putStrLn "  --random-output <dir> Specify the output directory for random file links."
  putStrLn "  --print               Print base names to standard output."
  putStrLn "  --seed <int>          Seed for random number generation."
  putStrLn "  --help                Display this help message."

parseArgs :: Options -> [String] -> Options
parseArgs o = \case
  [] -> o
  "--help" : rest -> parseArgs o{oShowHelp = True} rest
  "--input" : input : rest -> parseArgs o{oInputs = input : oInputs o} rest
  "--random-output" : dir : rest -> parseArgs o{oRandomOutput = Just dir} rest
  "--print" : rest -> parseArgs o{oPrint = True} rest
  "--seed" : n : rest -> parseArgs o{oSeed = Just (read n)} rest
  _ : rest -> parseArgs o rest

trim :: String -> String
trim = f . f
 where
  f = reverse . dropWhile isSpace

readInput :: FilePath -> IO (Set.Set String)
readInput input = do
  isDir <- doesDirectoryExist input
  names <-
    if isDir
      then do
        contents <- listDirectory input
        pure $ map (input </>) contents
      else do
        contents <- readFile input
        pure $ filter (not . null) $ map trim $ lines contents
  let names' = map takeBaseName names
  pure $ Set.fromList names'

-- Returns [0, n)
rollDie :: Int -> IO Int
rollDie n = randomRIO (0, n - 1)

selectRandomFile :: String -> [FilePath] -> IO (Maybe FilePath)
selectRandomFile baseName dirs = do
  dirIndex <- rollDie $ length dirs
  let dir = dirs !! dirIndex
  files <- listDirectory dir
  let files' = filter (\f -> takeBaseName f == baseName) files
  case files' of
    [file] -> pure $ Just $ dir </> file
    _ -> pure Nothing

createSymlinkWindows :: FilePath -> FilePath -> IO ()
createSymlinkWindows target linkName = do
  let command = "mklink " ++ normalise linkName ++ " " ++ normalise target
  callCommand command

createSymlink :: FilePath -> FilePath -> IO ()
createSymlink = if True then createSymlinkWindows else createFileLink

createCopy :: FilePath -> FilePath -> IO ()
createCopy = if True then copyFile else createFileLink

main :: IO ()
main = do
  args <- getArgs
  let options = parseArgs emptyOptions args
  when (oShowHelp options) do
    displayHelp
    exitSuccess
  let inputs = oInputs options
  inputDirs <- filterM doesDirectoryExist inputs
  baseNamesList <- mapM readInput inputs
  let baseNames =
        foldl1'
          Set.intersection
          if null baseNamesList
            then [Set.empty]
            else baseNamesList
  when (oPrint options) do
    mapM_ putStrLn $ Set.toList baseNames
  case oRandomOutput options of
    Nothing -> pure ()
    Just outputDir -> forM_ baseNames \baseName -> do
      mTarget <- selectRandomFile baseName inputDirs
      case mTarget of
        Nothing -> error $ "No file found for base name: " ++ baseName
        Just target -> do
          print target
          let link = outputDir </> baseName ++ takeExtension target
          target' <- canonicalizePath target
          print (target', link)
          createCopy target' link
