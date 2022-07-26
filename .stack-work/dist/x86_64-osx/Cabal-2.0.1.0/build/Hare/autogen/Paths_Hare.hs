{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -fno-warn-implicit-prelude #-}
module Paths_Hare (
    version,
    getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where

import qualified Control.Exception as Exception
import Data.Version (Version(..))
import System.Environment (getEnv)
import Prelude

#if defined(VERSION_base)

#if MIN_VERSION_base(4,0,0)
catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
#else
catchIO :: IO a -> (Exception.Exception -> IO a) -> IO a
#endif

#else
catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
#endif
catchIO = Exception.catch

version :: Version
version = Version [1,0] []
bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/Users/samthorley/cs3141/ass/ass2/.stack-work/install/x86_64-osx/027e29d09671af9f36c1ffa0ae2929c2cccf01521de42cf94df76085f449b162/8.2.2/bin"
libdir     = "/Users/samthorley/cs3141/ass/ass2/.stack-work/install/x86_64-osx/027e29d09671af9f36c1ffa0ae2929c2cccf01521de42cf94df76085f449b162/8.2.2/lib/x86_64-osx-ghc-8.2.2/Hare-1.0-8HgU0w26Z2J24N6GkCiSfg-Hare"
dynlibdir  = "/Users/samthorley/cs3141/ass/ass2/.stack-work/install/x86_64-osx/027e29d09671af9f36c1ffa0ae2929c2cccf01521de42cf94df76085f449b162/8.2.2/lib/x86_64-osx-ghc-8.2.2"
datadir    = "/Users/samthorley/cs3141/ass/ass2/.stack-work/install/x86_64-osx/027e29d09671af9f36c1ffa0ae2929c2cccf01521de42cf94df76085f449b162/8.2.2/share/x86_64-osx-ghc-8.2.2/Hare-1.0"
libexecdir = "/Users/samthorley/cs3141/ass/ass2/.stack-work/install/x86_64-osx/027e29d09671af9f36c1ffa0ae2929c2cccf01521de42cf94df76085f449b162/8.2.2/libexec/x86_64-osx-ghc-8.2.2/Hare-1.0"
sysconfdir = "/Users/samthorley/cs3141/ass/ass2/.stack-work/install/x86_64-osx/027e29d09671af9f36c1ffa0ae2929c2cccf01521de42cf94df76085f449b162/8.2.2/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "Hare_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "Hare_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "Hare_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "Hare_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "Hare_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "Hare_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
