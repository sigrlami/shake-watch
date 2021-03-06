{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}

module Main where

import           Control.Concurrent
import           Control.Exception             (SomeAsyncException (..),
                                                fromException, throwIO, try)
import           Data.Functor                  (void)
import           Data.List
import           Data.Maybe
import           Data.Semigroup                ((<>))
import qualified Data.Set                      as Set
import           Development.Shake             hiding (doesFileExist)
import           Development.Shake.Classes
import           Development.Shake.Database
import           Development.Shake.FilePath
import           Development.Shake.Watch.Types
import           Options.Applicative
import           System.Directory              (doesFileExist,
                                                getCurrentDirectory,
                                                listDirectory)
import           System.Environment            (getArgs, getEnvironment,
                                                withArgs)
import           System.FilePath
import           System.FSNotify               (eventPath, watchTreeChan,
                                                withManager)
import           System.Posix.Process

--------------------------------------------------------------------------------

-- Parser for standard structure
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
