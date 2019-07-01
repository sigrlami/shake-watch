{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}

module Main where

import           Control.Concurrent         (newChan, readChan)
import           Control.Exception          (SomeAsyncException (..),
                                             fromException, throwIO, try)
import           Data.Functor               (void)
import qualified Data.Set                   as Set
import           Development.Shake
import           Development.Shake.Database
import           Language.Haskell.Ghcid     as Ghcid
import           Options.Applicative
import           System.Directory           (getCurrentDirectory)
import           System.Environment         (getArgs, getEnvironment, withArgs)
import           System.FilePath
import           System.FSNotify            (eventPath, watchTreeChan,
                                             withManager)
import           System.Posix.Process

--------------------------------------------------------------------------------

data WatchOpt =
  WatchOpt
    { watchPath   :: String    -- ^ path to watchc dir, by default current
    , includePath :: String    -- ^ include particular files when watching dir
    , excludePath :: String    -- ^ exclude particular files when watching dir
    , cache       :: Bool      -- ^ use Shake cache functionality from `Development.Shake.Database`
    , delay       :: Int       -- ^ milliseconds to wait for duplicate events
    , action      :: [String]  -- ^ command to run
    } deriving (Show)

watchOpt :: Parser WatchOpt
watchOpt =
  WatchOpt
    <$> strOption (  long "path"
                  <> metavar "PATH"
                  <> help "directory / file to watch" )
    <*> strOption (  long "include"
                  <> value []
                  <> metavar "INCLUDE"
                  <> help "pattern for including files")
    <*> strOption (  long "exclude"
                  <> value []
                  <> metavar "EXCLUDE"
                  <> help "pattern for excluding files")
    <*> flag False True
                  (  long "cache"
                  <> short 'c'
                  <> long "Switch, to run caching stage (by default = false)")
    <*> option auto (  long "throttle"
                    <> value 0
                    <> metavar "MILLIS"
                    <> help "milliseconds to wait for duplicate events")
    <*> (some . strArgument)
                    (  metavar "COMMAND"
                    <> help "command to run" )

--------------------------------------------------------------------------------

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
runWatcher (WatchOpt wp ip ep dl c a ) = do
  -- Get the list of files that shake considers to be alive. This assumes
  -- we've set
  --
  --   shakeLiveFiles = [".shake/live"]
  --
  cwd   <- getCurrentDirectory
  files <- Set.fromList . map (cwd </>) . lines <$> readFile ".shake/live"

  -- Start watching the filesystem, and rebuild once any of these files
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
