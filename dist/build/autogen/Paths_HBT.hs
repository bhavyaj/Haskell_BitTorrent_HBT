module Paths_HBT (
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
version = Version {versionBranch = [0,1,0,0], versionTags = []}
bindir, libdir, datadir, libexecdir :: FilePath

bindir     = "/home/bhavya/Acads/cs653/HBT_Final/bin"
libdir     = "/home/bhavya/Acads/cs653/HBT_Final/lib/HBT-0.1.0.0/ghc-7.4.2"
datadir    = "/home/bhavya/Acads/cs653/HBT_Final/share/HBT-0.1.0.0"
libexecdir = "/home/bhavya/Acads/cs653/HBT_Final/libexec"

getBinDir, getLibDir, getDataDir, getLibexecDir :: IO FilePath
getBinDir = catchIO (getEnv "HBT_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "HBT_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "HBT_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "HBT_libexecdir") (\_ -> return libexecdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
