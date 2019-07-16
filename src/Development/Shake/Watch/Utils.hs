{-# LANGUAGE OverloadedStrings #-}

module Development.Shake.Watch.Utils where

import           Data.List
import           Data.Maybe
import qualified Data.Set                                      as Set
import qualified Data.Text                                     as T
import           Development.Shake
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
import           Network.Socket                                (withSocketsDo)
import qualified Network.WebSockets                            as WS
import           Options.Applicative
import           System.Directory                              (doesFileExist, getCurrentDirectory,
                                                                listDirectory)

--------------------------------------------------------------------------------

collectSourceDirectories :: PackageDescription -> [FilePath]
collectSourceDirectories =
  concatMap hsSourceDirs . allBuildInfo

-- | Use Cabal API to collect list of directories with Haskell source
getSourceDirectories :: FilePath -> IO [FilePath]
getSourceDirectories cabalFile = do
  genpkg <- readGenericPackageDescription silent cabalFile
  let pkg = packageDescription genpkg
  return (collectSourceDirectories (flattenPackageDescription genpkg))

-- | Small websockets client for updates when
wsClient :: WS.ClientApp ()
wsClient conn = do
  putStrLn "Connected!"
  WS.sendTextData conn ("update" :: T.Text)
  WS.sendClose    conn ("Bye!"   :: T.Text)
