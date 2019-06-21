{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}
{-# LANGUAGE ViewPatterns      #-}

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
  args <- getArgs
  case args of
    "watch":args ->
      -- Strip off the "watch" arg for the real shake script.
      withArgs args $ do
        -- Run the real main under 'try', ignoring synchronous exceptions
        -- because we don't care if the build fails - we want to watch & rebuild
        -- regardless. Re-throw async exceptions.
        try realMain >>= \case
          Left (fromException -> Just (SomeAsyncException ex)) -> throwIO ex
          _ -> pure ()

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
        executeFile "bin/Shakefile" False ("watch":args) (Just env)

    _ ->
      realMain

realMain :: IO ()
realMain = do
  return $ ()

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
