{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module Paths_maze (
    version,
    getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where


import qualified Control.Exception as Exception
import qualified Data.List as List
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

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir `joinFileName` name)

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath



bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath
bindir     = "/home/vaz/Desktop/maze/.stack-work/install/x86_64-linux-tinfo6/5eaaf7e233b808788975d4050bba6e2e86b82c19cca232573edd1cfcab07b790/9.4.6/bin"
libdir     = "/home/vaz/Desktop/maze/.stack-work/install/x86_64-linux-tinfo6/5eaaf7e233b808788975d4050bba6e2e86b82c19cca232573edd1cfcab07b790/9.4.6/lib/x86_64-linux-ghc-9.4.6/maze-0.1.0.0-KdeKV9LdRuGBAN53JXDPcv-maze"
dynlibdir  = "/home/vaz/Desktop/maze/.stack-work/install/x86_64-linux-tinfo6/5eaaf7e233b808788975d4050bba6e2e86b82c19cca232573edd1cfcab07b790/9.4.6/lib/x86_64-linux-ghc-9.4.6"
datadir    = "/home/vaz/Desktop/maze/.stack-work/install/x86_64-linux-tinfo6/5eaaf7e233b808788975d4050bba6e2e86b82c19cca232573edd1cfcab07b790/9.4.6/share/x86_64-linux-ghc-9.4.6/maze-0.1.0.0"
libexecdir = "/home/vaz/Desktop/maze/.stack-work/install/x86_64-linux-tinfo6/5eaaf7e233b808788975d4050bba6e2e86b82c19cca232573edd1cfcab07b790/9.4.6/libexec/x86_64-linux-ghc-9.4.6/maze-0.1.0.0"
sysconfdir = "/home/vaz/Desktop/maze/.stack-work/install/x86_64-linux-tinfo6/5eaaf7e233b808788975d4050bba6e2e86b82c19cca232573edd1cfcab07b790/9.4.6/etc"

getBinDir     = catchIO (getEnv "maze_bindir")     (\_ -> return bindir)
getLibDir     = catchIO (getEnv "maze_libdir")     (\_ -> return libdir)
getDynLibDir  = catchIO (getEnv "maze_dynlibdir")  (\_ -> return dynlibdir)
getDataDir    = catchIO (getEnv "maze_datadir")    (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "maze_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "maze_sysconfdir") (\_ -> return sysconfdir)




joinFileName :: String -> String -> FilePath
joinFileName ""  fname = fname
joinFileName "." fname = fname
joinFileName dir ""    = dir
joinFileName dir fname
  | isPathSeparator (List.last dir) = dir ++ fname
  | otherwise                       = dir ++ pathSeparator : fname

pathSeparator :: Char
pathSeparator = '/'

isPathSeparator :: Char -> Bool
isPathSeparator c = c == '/'
