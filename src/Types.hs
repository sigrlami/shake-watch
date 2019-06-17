module Types where

import           GHC.Generics


--------------------------------------------------------------------------------

-- | Simple data type containing a few information on how to start ghcid.
--
data WatchSettings =
  WatchSettings
    { rootDir :: FilePath -- ^ Project directory from which ghcid can be started successfully.
    , cmd     :: String   -- ^ Command to start a ghci session (usually @cabal repl@ or -- @stack ghci@).
    } deriving (Eq, Ord, Show, Generic, FromJSON, ToJSON)
