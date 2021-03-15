{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_JqClone (
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

bindir     = "/Users/vykintascivas/Desktop/jq-clone/.stack-work/install/x86_64-osx/2626e9078d979092fcb22044e051effa32206b867b47518d6fd4ba7cdd88836b/8.10.4/bin"
libdir     = "/Users/vykintascivas/Desktop/jq-clone/.stack-work/install/x86_64-osx/2626e9078d979092fcb22044e051effa32206b867b47518d6fd4ba7cdd88836b/8.10.4/lib/x86_64-osx-ghc-8.10.4/JqClone-0.1.0.0-FKlx1u5pEJa3jIr85G00cR"
dynlibdir  = "/Users/vykintascivas/Desktop/jq-clone/.stack-work/install/x86_64-osx/2626e9078d979092fcb22044e051effa32206b867b47518d6fd4ba7cdd88836b/8.10.4/lib/x86_64-osx-ghc-8.10.4"
datadir    = "/Users/vykintascivas/Desktop/jq-clone/.stack-work/install/x86_64-osx/2626e9078d979092fcb22044e051effa32206b867b47518d6fd4ba7cdd88836b/8.10.4/share/x86_64-osx-ghc-8.10.4/JqClone-0.1.0.0"
libexecdir = "/Users/vykintascivas/Desktop/jq-clone/.stack-work/install/x86_64-osx/2626e9078d979092fcb22044e051effa32206b867b47518d6fd4ba7cdd88836b/8.10.4/libexec/x86_64-osx-ghc-8.10.4/JqClone-0.1.0.0"
sysconfdir = "/Users/vykintascivas/Desktop/jq-clone/.stack-work/install/x86_64-osx/2626e9078d979092fcb22044e051effa32206b867b47518d6fd4ba7cdd88836b/8.10.4/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "JqClone_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "JqClone_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "JqClone_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "JqClone_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "JqClone_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "JqClone_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
