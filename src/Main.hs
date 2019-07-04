{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}

module Main where

import           Control.Concurrent                            (newChan,
                                                                readChan)
import           Control.Exception                             (SomeAsyncException (..),
                                                                fromException,
                                                                throwIO, try)
import           Data.Functor                                  (void)
import           Data.List
import           Data.Maybe
import qualified Data.Set                                      as Set
import           Development.Shake                             hiding
                                                                (doesFileExist)
import           Development.Shake.Classes
import           Development.Shake.Database
import           Development.Shake.FilePath
import           Distribution.PackageDescription               (GenericPackageDescription,
                                                                PackageDescription,
                                                                allBuildInfo,
                                                                hsSourceDirs,
                                                                packageDescription)
import           Distribution.PackageDescription.Configuration (flattenPackageDescription)
import           Distribution.PackageDescription.Parsec        (readGenericPackageDescription)
import           Distribution.Verbosity                        (silent)
import           Language.Haskell.Ghcid                        as Ghcid
import           Options.Applicative
import           System.Directory                              (doesFileExist, getCurrentDirectory,
                                                                listDirectory)
import           System.Environment                            (getArgs,
                                                                getEnvironment,
                                                                withArgs)
import           System.FilePath
import           System.FSNotify                               (eventPath,
                                                                watchTreeChan,
                                                                withManager)
import           System.Posix.Process

--------------------------------------------------------------------------------

data WatchOpt =
  WatchOpt
    { watchPath   :: String    -- ^ path to watch dir, by default current
    , includePath :: String    -- ^ include particular files when watching dir
    , excludePath :: String    -- ^ exclude particular files when watching dir
    , cache       :: Bool      -- ^ use Shake cache functionality from `Development.Shake.Database`
    , remake      :: Bool      -- ^ delete old database upon startup
    , watch       :: Bool      -- ^ rerun shake with the database after any changes observed to the included directories
    , delay       :: Int       -- ^ milliseconds to wait for duplicate events
    , action      :: [String]  -- ^ command to run
    } deriving (Show)

watchOpt :: Parser WatchOpt
watchOpt =
  WatchOpt
    <$> strOption (  long "path"
                  <> short 'p'
                  <> metavar "PATH"
                  <> help "directory / file to watch" )
    <*> strOption (  long "include"
                  <> short 'i'
                  <> value []
                  <> metavar "INCLUDE"
                  <> help "pattern for including files")
    <*> strOption (  long "exclude"
                  <> short 'e'
                  <> value []
                  <> metavar "EXCLUDE"
                  <> help "pattern for excluding files")
    <*> flag False True
                  (  long "cache"
                  <> short 'c'
                  <> long "Switch, to run caching stage (by default = false)")
    <*> flag False True
                  (  long "remake"
                  <> short 'r'
                  <> long "Switch, to delete old cache on start")
    <*> flag False True
                  (  long "watch"
                  <> short 'w'
                  <> long "Switch, to actually watch outside directories for changes")
    <*> option auto (  long "throttle"
                    <> short 't'
                    <> value 100
                    <> metavar "MILLIS"
                    <> help "milliseconds to wait for duplicate events")
    <*> (some . strArgument)
                    (  metavar "COMMAND"
                    <> help "command to run" )

--------------------------------------------------------------------------------

dummyRules = do
    "*.out" %> \out -> do
        liftIO $ appendFile "log.txt" "x"
        copyFile' (out -<.> "in") out
        removeFilesAfter "." ["log.txt"]

main :: IO ()
main = do
  -- parse command line options, redirect execution
  execParser opts >>= runWatcher
    where
      opts = info (helper <*> watchOpt)
         (  fullDesc
         <> progDesc "Start shake watching functionality"
         <> header   "Shake Watch")

runWatcher :: WatchOpt -> IO ()
runWatcher opts@(WatchOpt wp ip ep ch re wa dl a) = do
  putStrLn $ "Watching external dir:" ++  wp

  -- check whether we're running in cache mode
  case ch of
    False -> do
      runWatcherUnCached opts
    True  -> do

      -- Check if we need to recreate shake db
      case re of
        False -> return $ ()
        True  -> do
          -- TODO: create totally new Shake dbщ
          return $ ()

      let sopts = shakeOptions{shakeFiles="/dev/null"}

      (shDb, shClose) <- shakeOpenDatabase sopts dummyRules
      shakeDb         <- shDb
      env             <- getEnvironment

      cwd   <- getCurrentDirectory
      files <- listDirectory cwd
      let cbl = head $ filter (isInfixOf ".cabal") files
      hs <- getSourceDirectories cbl


      -- Get the list of files that shake considers to be alive. This assumes
      -- user set
      --
      --   shakeLiveFiles = [".shake/live"]
      --
      isShakeLiveAvailable <- doesFileExist ".shake/live"
      case isShakeLiveAvailable of
        False -> do
          -- no Shake live defined in project
          -- identify files to watch on our own

          -- 1 haskell source files
          -- 2 additional files
          putStrLn $ show $ hs
          return $ ()

        True  -> do
          files <- Set.fromList . map (cwd </>) . lines <$> readFile ".shake/live"
          putStrLn $ show $ files



          -- Start watching the filesystem with FSNotify, and rebuild once any of these files
          -- changes.
          withManager $ \manager -> do
            chan <- newChan
            void (watchTreeChan manager "." ((`elem` files) . eventPath) chan)
            void (readChan chan) -- We block here

            -- Loop. Here my compiled shakefile is itself a build target, so I exec
            -- it rather than loop here, to get the latest & greatest shake
            -- executable.
            env <- getEnvironment
            executeFile "bin/Shakefile" False (["watch"]) (Just env)

      shClose

runWatcherUnCached :: WatchOpt -> IO ()
runWatcherUnCached = undefined

ghcidStart :: IO ()
ghcidStart = do
  env <- getEnvironment
  pid <- forkProcess $ do
    putStrLn $ "Loading GHCi for live reloads"
    executeFile "ghcid" True ["-- command 'stack ghci' --reload"] (Just env) -- start ghcid with tracking functionality
  return $ ()

ghcidStop :: IO ()
ghcidStop  = do
  env <- getEnvironment
  executeFile "killall" True ["ghcid"] (Just env)
  return $ ()

ghcidRestart :: IO ()
ghcidRestart = do
  ghcidStop
  ghcidStart
  return $ ()


--------------------------------------------------------------------------------

collectSourceDirectories :: PackageDescription -> [FilePath]
collectSourceDirectories =
  concatMap hsSourceDirs . allBuildInfo


getSourceDirectories :: FilePath -> IO [FilePath]
getSourceDirectories cabalFile = do
  genpkg <- readGenericPackageDescription silent cabalFile
  let pkg = packageDescription genpkg
  return (collectSourceDirectories (flattenPackageDescription genpkg))
