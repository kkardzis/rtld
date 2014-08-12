-------------------------------------------------------------------------------
-- |
-- Module      :  System.RTLD
-- Copyright   :  Copyright © 2012-2014 Krzysztof Kardzis
-- License     :  ISC License (MIT/BSD-style, see LICENSE file for details)
-- 
-- Maintainer  :  Krzysztof Kardzis <kkardzis@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable
--
-------------------------------------------------------------------------------
{-# LANGUAGE ForeignFunctionInterface #-}

module System.RTLD
  ( RTLD (..)
  
  , LIBH, dynload, dynfree, dynfunc, dynfail

  , SYMTABENTRY (..)
  , RTSO (..)
  , rtload
  , rtfree

  ) where

import Foreign.C.String
import Foreign.C.Types
import Foreign.Ptr
import Foreign.Storable
import Foreign.Marshal.Array
import Foreign.Marshal.Utils (toBool)
#ifdef WINRTLD
import Foreign.Marshal.Alloc (alloca)
#endif

import Control.Concurrent (MVar, modifyMVar_)
import Control.Exception  (bracket_, bracketOnError)
import Control.Monad      (when, void, foldM)

#include "RTLD.h"


-------------------------------------------------------------------------------
class RTLD so where
  withlib :: so -> IO a -> IO a
  loadlib :: so -> IO ()
  freelib :: so -> IO ()

  withlib so = bracket_ (loadlib so) (freelib so)


-------------------------------------------------------------------------------
type LIBH = Ptr ()


-------------------------------------------------------------------------------
#ifdef WINRTLD

#define WIN32_LEAN_AND_MEAN
#include <windows.h>

dynload :: String -> IO LIBH
dynload xs = withCString xs loadLibrary

dynfunc :: LIBH -> Ptr CChar -> IO (FunPtr ())
dynfunc = getProcAddress

dynfree :: LIBH -> IO Bool
dynfree hm = fmap toBool (freeLibrary hm)

dynfail :: IO String
dynfail = alloca $ \csptr -> do
  err <- getLastError
  st <- formatMessage dwflags nullPtr err 0 csptr 0 nullPtr
  xs <- if (st==0) then return "" else peek csptr >>= peekCString
  _ <- peek csptr >>= \cs -> localFree (castPtr cs)
  return $ concat ["(", show err, ") ", xs]

dwflags :: DWORD
dwflags = #{const (FORMAT_MESSAGE_ALLOCATE_BUFFER|FORMAT_MESSAGE_FROM_SYSTEM)}

type FARPROC = FunPtr ()
type HMODULE = Ptr ()
type HLOCAL = Ptr ()
type LPCVOID = Ptr ()
type LPCSTR = Ptr CChar
type LPSTR = Ptr (Ptr CChar) -- actually should be (Ptr CChar)
type DWORD = CULong
type BOOL = CInt

foreign import stdcall "LoadLibraryA"
  loadLibrary :: LPCSTR -> IO HMODULE

foreign import stdcall "GetProcAddress"
  getProcAddress :: HMODULE -> LPCSTR -> IO FARPROC

foreign import stdcall "FreeLibrary"
  freeLibrary :: HMODULE -> IO BOOL

foreign import stdcall "GetLastError"
  getLastError :: IO DWORD

foreign import stdcall "FormatMessageA"
  formatMessage :: DWORD -> LPCVOID -> DWORD -> DWORD
                -> LPSTR -> DWORD -> Ptr () -> IO DWORD

foreign import stdcall "LocalFree"
  localFree :: HLOCAL -> IO HLOCAL



-------------------------------------------------------------------------------
#else

#include <dlfcn.h>

dynload :: String -> IO LIBH
dynload xs = withCString xs (\cs -> dlopen cs #{const RTLD_LAZY})

dynfunc :: LIBH -> Ptr CChar -> IO (FunPtr ())
dynfunc = dlsym

dynfree :: LIBH -> IO Bool
dynfree hm = fmap (not . toBool) (dlclose hm)

dynfail :: IO String
dynfail = dlerror >>= peekCString

foreign import ccall "dlopen"  dlopen  :: Ptr CChar -> CInt -> IO (Ptr ())
foreign import ccall "dlsym"   dlsym   :: Ptr () -> Ptr CChar -> IO (FunPtr ())
foreign import ccall "dlclose" dlclose :: Ptr () -> IO CInt
foreign import ccall "dlerror" dlerror :: IO (Ptr CChar)

#endif





-------------------------------------------------------------------------------
newtype SYMTABENTRY = RTSYM (CInt,CInt,(Ptr CChar))

instance Storable SYMTABENTRY where
  sizeOf    _ = #{size    SYMTABENTRY}
  alignment _ = #{alignof SYMTABENTRY}
  poke _ _    = undefined
  peek ptr    = do
    vmin <- #{peek SYMTABENTRY, vmin} ptr
    vmax <- #{peek SYMTABENTRY, vmax} ptr
    name <- #{peek SYMTABENTRY, name} ptr
    return (RTSYM (vmin,vmax,name))


-------------------------------------------------------------------------------
data RTSO a = RTSO
  { rtPKGMVAR :: MVar (Maybe (a, LIBH, Int))
  , rtPKGNAME :: String
  , rtLIBNAME :: a -> String
  , rtSONAMES :: a -> [String]
  , rtONLOAD  :: a -> IO ()
  , rtONFREE  :: a -> IO ()
  , rtGETAPI  :: a -> IO (Maybe a)
  , rtSYMTAB  :: Ptr SYMTABENTRY
  , rtADRTAB  :: Ptr (FunPtr ())
  , rtTABLEN  :: Int
  }


-------------------------------------------------------------------------------
rtload :: (Ord a, Enum a, Bounded a) => RTSO a -> a -> IO ()
rtload rtso rqApi =
  modifyMVar_ (rtPKGMVAR rtso) $ \state -> case state of
    Nothing -> libloadA rtso rqApi >>= \(ldApi,h) -> return (Just (ldApi,h,1))
    Just (ldApi,h,x) -> libtest rtso rqApi ldApi >> return (Just (ldApi,h,x+1))

rtfree :: RTSO a -> IO ()
rtfree rtso =
  modifyMVar_ (rtPKGMVAR rtso) $ \state -> case state of
    Nothing -> return Nothing
    Just (ldApi,h,1) -> libfree rtso ldApi h >> return Nothing
    Just (ldApi,h,x) -> return (Just (ldApi,h,x-1))


-------------------------------------------------------------------------------
libloadA :: (Ord a, Enum a, Bounded a) => RTSO a -> a -> IO (a, LIBH)
libloadA rtso rqApi =
  bracketOnError
    (foldM mloadfile Nothing (rtSONAMES rtso rqApi))
    (maybe (return ()) (void . dynfree . fst))
    (maybe (libloadfailA rtso rqApi) (libloadB rtso rqApi))

mloadfile :: Maybe (LIBH, String) -> String -> IO (Maybe (LIBH, String))
mloadfile mlh@(Just _) _ = return mlh
mloadfile Nothing soname =
  let check libh = if (libh==nullPtr) then Nothing else (Just (libh,soname))
  in  fmap check (dynload soname)

libloadfailA :: RTSO a -> a -> IO (a, LIBH)
libloadfailA rtso rqApi = error $ concat
  [ rtPKGNAME rtso, " failed to load ", rtLIBNAME rtso rqApi
  , " ", show (rtSONAMES rtso rqApi) ]


-------------------------------------------------------------------------------
libloadB :: (Ord a, Enum a, Bounded a)
         => RTSO a -> a -> (LIBH, String) -> IO (a, LIBH)
libloadB rtso rqApi so@(libh, soname) =
  bracketOnError (symload rtso libh) (const $ symfree rtso)
    (maybe (libloadfailB rtso rqApi soname) (libloadC rtso rqApi so))

libloadC :: (Ord a) => RTSO a -> a -> (LIBH, String) -> a -> IO (a, LIBH)
libloadC rtso rqApi (libh, soname) smApi =
  bracketOnError (rtONLOAD rtso smApi) (\_ -> rtONFREE rtso smApi)
    $ \_ -> do
      ldApiM <- rtGETAPI rtso smApi
      case ldApiM of
        Nothing    -> libloadfailB rtso rqApi soname
        Just ldApi -> if (ldApi>=rqApi && smApi>=rqApi)
          then return ((min ldApi smApi), libh)
          else libloadfailB rtso rqApi soname

libloadfailB :: RTSO a -> a -> String -> IO (a, LIBH)
libloadfailB rtso rqApi soname = error $ concat
  [ rtPKGNAME rtso, " failed to load ", rtLIBNAME rtso rqApi
  , " ('", soname, "' found, but version doesn't match)" ]


-------------------------------------------------------------------------------
libtest :: (Ord a) => RTSO a -> a -> a -> IO ()
libtest rtso rqApi ldApi = when (rqApi > ldApi) (libtestfail rtso rqApi ldApi)

libtestfail :: RTSO a -> a -> a -> IO ()
libtestfail rtso rqApi ldApi = error $ concat
  [ rtPKGNAME rtso, " version mismatch: "
  , rtLIBNAME rtso rqApi, " required, but "
  , rtLIBNAME rtso ldApi, " already loaded" ]


-------------------------------------------------------------------------------
libfree :: RTSO a -> a -> LIBH -> IO ()
libfree rtso ldApi libh = do
  rtONFREE rtso ldApi
  x <- symfree rtso >> dynfree libh
  when (not x) (libfreefail rtso ldApi)

libfreefail :: RTSO a -> a -> IO ()
libfreefail rtso ldApi = error $ concat
  [ rtPKGNAME rtso, " failed to free ", rtLIBNAME rtso ldApi ]


-------------------------------------------------------------------------------
symload :: (Ord a, Enum a, Bounded a) => RTSO a -> LIBH -> IO (Maybe a)
symload rtso libh = do
  (vmins,vmaxs,names) <- fmap vpack $ peekArray (rtTABLEN rtso) (rtSYMTAB rtso)
  addrs <- mapM (dynfunc libh) names
  pokeArray (rtADRTAB rtso) addrs
  return $ case (vtags (zip3 vmins vmaxs addrs)) of
    [] -> Nothing
    xs -> Just (maximum xs)

symfree :: RTSO a -> IO ()
symfree rtso =
  pokeArray (rtADRTAB rtso) (take (rtTABLEN rtso) (repeat nullFunPtr))

vpack :: (Enum a) => [SYMTABENTRY] -> ([a], [a], [Ptr CChar])
vpack xs = unzip3 $ map (\(RTSYM (x,y,z)) -> (foo x, foo y, z)) xs
  where foo = toEnum . fromIntegral

vtags :: (Ord a, Enum a, Bounded a) => [(a,a,FunPtr ())] -> [a]
vtags xs = filter (\x -> all (vtest x) xs) [minBound .. maxBound]

vtest :: (Ord a, Enum a, Bounded a) => a -> (a,a,FunPtr ()) -> Bool
vtest vnom (vmin,vmax,addr) = (addr/=nullFunPtr) || (vnom<vmin) || (vnom>vmax)

