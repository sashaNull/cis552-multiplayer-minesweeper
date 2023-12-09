{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module Paths_project_cis5520 (
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
bindir     = "D:\\CODING\\cis552-multiplayer-minesweeper\\.stack-work\\install\\61edb194\\bin"
libdir     = "D:\\CODING\\cis552-multiplayer-minesweeper\\.stack-work\\install\\61edb194\\lib\\x86_64-windows-ghc-9.4.5\\project-cis5520-0.1.0.0-4XPHqj8ABbb8egNBVoLWWr-project-cis5520-exe"
dynlibdir  = "D:\\CODING\\cis552-multiplayer-minesweeper\\.stack-work\\install\\61edb194\\lib\\x86_64-windows-ghc-9.4.5"
datadir    = "D:\\CODING\\cis552-multiplayer-minesweeper\\.stack-work\\install\\61edb194\\share\\x86_64-windows-ghc-9.4.5\\project-cis5520-0.1.0.0"
libexecdir = "D:\\CODING\\cis552-multiplayer-minesweeper\\.stack-work\\install\\61edb194\\libexec\\x86_64-windows-ghc-9.4.5\\project-cis5520-0.1.0.0"
sysconfdir = "D:\\CODING\\cis552-multiplayer-minesweeper\\.stack-work\\install\\61edb194\\etc"

getBinDir     = catchIO (getEnv "project_cis5520_bindir")     (\_ -> return bindir)
getLibDir     = catchIO (getEnv "project_cis5520_libdir")     (\_ -> return libdir)
getDynLibDir  = catchIO (getEnv "project_cis5520_dynlibdir")  (\_ -> return dynlibdir)
getDataDir    = catchIO (getEnv "project_cis5520_datadir")    (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "project_cis5520_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "project_cis5520_sysconfdir") (\_ -> return sysconfdir)




joinFileName :: String -> String -> FilePath
joinFileName ""  fname = fname
joinFileName "." fname = fname
joinFileName dir ""    = dir
joinFileName dir fname
  | isPathSeparator (List.last dir) = dir ++ fname
  | otherwise                       = dir ++ pathSeparator : fname

pathSeparator :: Char
pathSeparator = '\\'

isPathSeparator :: Char -> Bool
isPathSeparator c = c == '/' || c == '\\'
