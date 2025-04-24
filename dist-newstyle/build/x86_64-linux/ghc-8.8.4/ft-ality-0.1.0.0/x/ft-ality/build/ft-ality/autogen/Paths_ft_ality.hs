{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_ft_ality (
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
version = Version [0,1,0,0] []
bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/home/wmonacho/.cabal/bin"
libdir     = "/home/wmonacho/.cabal/lib/x86_64-linux-ghc-8.8.4/ft-ality-0.1.0.0-inplace-ft-ality"
dynlibdir  = "/home/wmonacho/.cabal/lib/x86_64-linux-ghc-8.8.4"
datadir    = "/home/wmonacho/.cabal/share/x86_64-linux-ghc-8.8.4/ft-ality-0.1.0.0"
libexecdir = "/home/wmonacho/.cabal/libexec/x86_64-linux-ghc-8.8.4/ft-ality-0.1.0.0"
sysconfdir = "/home/wmonacho/.cabal/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "ft_ality_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "ft_ality_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "ft_ality_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "ft_ality_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "ft_ality_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "ft_ality_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
