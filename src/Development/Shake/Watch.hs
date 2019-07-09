{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}

module Development.Shake.Watch where

import           Control.Concurrent
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
import           Development.Shake.Watch.Types
import           Development.Shake.Watch.Utils
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

runWatcher :: ShakeOptions -> WatchOpt -> Rules () -> IO ()
runWatcher shOpts opts@(WatchOpt wp ip ep ch re wa dl a) rules = do
  putStrLn $ "Watching external dir:" ++ wp

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

      (shDb, shClose) <- shakeOpenDatabase shOpts rules
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
          -- No Shake live defined in project
          -- identify files to watch on our own

          -- 1 find haskell source directories
          putStrLn $ "shake-watch: tracking haskell at: " ++ (show $ hs)
          hsfiles <- listDirectory $ head hs
          forkIO $
            withManager $
              \manager -> do
                chan <- newChan
                void (watchTreeChan manager "." ((`elem` hsfiles) . eventPath) chan)
                void (readChan chan) -- We block here
                -- TODO: load ghcid

          -- 2 additional directories provided by shake-watch user
          putStrLn $ "shake-watch: tracking additional files at " ++ (show $ wp)
          withManager $
            \manager -> do
              chan <- newChan
              void (watchTreeChan manager "." ((`elem` files) . eventPath) chan)
              void (readChan chan) -- We block here

              env <- getEnvironment
              executeFile "bin/Shakefile" False (["watch"]) (Just env)

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
