module Paths_onicom (
    version,
    getBinDir, getLibDir, getDataDir, getLibexecDir,
    getDataFileName
  ) where

import qualified Control.Exception as Exception
import Data.Version (Version(..))
import System.Environment (getEnv)
catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
catchIO = Exception.catch


version :: Version
version = Version {versionBranch = [0,1], versionTags = []}
bindir, libdir, datadir, libexecdir :: FilePath

bindir     = "/root/.cabal/bin"
libdir     = "/root/.cabal/lib/onicom-0.1/ghc-7.4.1"
datadir    = "/root/.cabal/share/onicom-0.1"
libexecdir = "/root/.cabal/libexec"

getBinDir, getLibDir, getDataDir, getLibexecDir :: IO FilePath
getBinDir = catchIO (getEnv "onicom_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "onicom_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "onicom_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "onicom_libexecdir") (\_ -> return libexecdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
