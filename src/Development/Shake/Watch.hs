{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}

module Development.Shake.Watch where

import           Control.Concurrent
import           Control.Exception                             (SomeAsyncException (..),
                                                                fromException,
                                                                throwIO, try)
import           Control.Monad                                 (forM_, forever,
                                                                liftM, mzero,
                                                                when)
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
import           Network.Socket                                (withSocketsDo)
import qualified Network.WebSockets                            as WS
import           Options.Applicative
import           System.Console.GetOpt
import           System.Directory                              (doesFileExist, getCurrentDirectory,
                                                                listDirectory)
import           System.Environment                            (getArgs,
                                                                getEnvironment,
                                                                withArgs)
import           System.FilePath
import qualified System.FSNotify                               as FSNotify
import           System.Posix.Process


--------------------------------------------------------------------------------

type ShakeAction a =
       ShakeOptions
    -> [OptDescr (Either String a)]
    -> (ShakeOptions -> [a] -> [String] -> IO (Maybe (ShakeOptions, Rules ())))
    -> IO ()

--runWatcher :: ShakeOptions -> WatchOpt -> Rules () -> ShakeAction a -> IO ()
runWatcher shOpts opts@(WatchOpt wp ip ep ch re wa dl a) rules shAct = do
  putStrLn $ "\nshake-watch: cwd - " ++ wp

  -- check if we're running in cache mode
  case ch of
    False -> do
      runWatcherUnCached opts
    True  -> do

      -- Check if we need to recreate shake db
      case re of
        False -> return $ ()
        True  -> do
          -- TODO: create totally new Shake db
          return $ ()

      let sopts = shakeOptions{shakeFiles="/dev/null"}

      -- .shake/.shake.database
      --(shDb, shClose) <- shakeOpenDatabase shOpts rules
      --shakeDb         <- shDb
      env             <- getEnvironment

      cwd   <- getCurrentDirectory
      files <- listDirectory cwd
      let cbl = head $ filter (isInfixOf ".cabal") files
      hs <- getSourceDirectories cbl

      -- Get the list of files that shake considers to be alive. s
      -- This assume user set
      -- shakeLiveFiles = [".shake/live"]
      isShakeLiveAvailable <- doesFileExist ".shake/live"
      case isShakeLiveAvailable of
        False -> do
          -- No Shake live defined in project
          -- identify files to watch on our own

          -- 1 find haskell source directories
          let tr = cwd ++ "/" ++ (head hs) ++ "/"
          putStrLn $ "shake-watch: tracking haskell at: " -- ++ (show $ hs)
          putStrLn $ "shake-watch: " ++ tr

          hsfiles <- listDirectory $ tr
          putStrLn $ "shake-watch:  "
          -- mapM_ (\x -> putStrLn $ "  -" ++ (show x)) hsfiles

          forkIO $ FSNotify.withManager $
              \manager -> do
                FSNotify.watchTree
                  manager         -- manager
                  (tr)            -- directory to watch
                  (const True)    -- predicate
                  (\e -> do       -- action Event -> IO ()
                    case e of
                      FSNotify.Modified path time isDir -> do
                        putStrLn $ "shake-watch:  "
                        putStrLn $ "   changed " ++ path
                        putStrLn $ "   reload ..."
                        -- TODO: rebuild haskell
                      FSNotify.Removed path time isDir -> do
                        return $ ()
                      _        -> do
                        putStrLn $ "   " ++ show e
                    return $ ()
                  )

                -- block for endless execution
                forever $ threadDelay 100000

          -- 2 additional directories provided by shake-watch user
          putStrLn $ "shake-watch: tracking additional directory at: "
          putStrLn $ "shake-watch: " ++ ip
          forkIO $ FSNotify.withManager $
              \manager -> do
                FSNotify.watchTree
                  manager         -- manager
                  (ip)            -- directory to watch
                  (const True)    -- predicate
                  (\e -> do       -- action
                    case e of
                      FSNotify.Modified path time isDir -> do
                        putStrLn $ "shake-watch:  "
                        putStrLn $ "   changed " ++ path
                        putStrLn $ "   rebuild ..."
                        -- run Shake action
                        shAct
                        -- notify Preview server that rebuild happend
                        withSocketsDo
                          $ WS.runClient "127.0.0.1" 3030 "/" wsClient

                      FSNotify.Removed path time isDir -> do
                        return $ ()
                      _        -> do
                        putStrLn $ "   " ++ show e
                    return $ ()
                  )
                forever $ threadDelay 100000 -- block for endless execution

          return $ ()

        True  -> do
          files <- Set.fromList . map (cwd </>) . lines <$> readFile ".shake/live"
          putStrLn $ show $ files

          -- Start watching the filesystem with FSNotify, and rebuild once any of these files
          -- changes.
          FSNotify.withManager $ \manager -> do
            chan <- newChan
            void (FSNotify.watchTreeChan manager "." ((`elem` files) . FSNotify.eventPath) chan)
            void (readChan chan) -- We block here

            -- Loop. Here my compiled shakefile is itself a build target, so I exec
            -- it rather than loop here, to get the latest & greatest shake
            -- executable.
            env <- getEnvironment
            executeFile "bin/Shakefile" False (["watch"]) (Just env)

      --shClose

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
