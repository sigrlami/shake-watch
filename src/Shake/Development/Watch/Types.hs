module Shake.Development.Watch.Types where

import           GHC.Generics


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
