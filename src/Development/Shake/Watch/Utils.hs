module Development.Shake.Watch.Utils where

import           Data.List
import           Data.Maybe
import qualified Data.Set                                      as Set
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
import           Options.Applicative
import           System.Directory                              (doesFileExist, getCurrentDirectory,
                                                                listDirectory)

--------------------------------------------------------------------------------

collectSourceDirectories :: PackageDescription -> [FilePath]
collectSourceDirectories =
  concatMap hsSourceDirs . allBuildInfo

getSourceDirectories :: FilePath -> IO [FilePath]
getSourceDirectories cabalFile = do
  genpkg <- readGenericPackageDescription silent cabalFile
  let pkg = packageDescription genpkg
  return (collectSourceDirectories (flattenPackageDescription genpkg))
