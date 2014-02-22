------------------------------------------------------------------------------
-- Copyright (c) 2014, Enzo Haussecker, Nathan Howell. All rights reserved. --
------------------------------------------------------------------------------

{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS -Wall                  #-}

-- | Haskell bindings to the C time library.
module Data.Time.Exts.C where

import Data.Convertible       (Convertible(..))
import Foreign.C.String       (CString)
import Foreign.C.Types        (CInt(..), CLong, CTime(..))
import Foreign.Marshal.Alloc  (alloca)
import Foreign.Marshal.Unsafe (unsafeLocalState)
import Foreign.Marshal.Utils  (with)
import Foreign.Ptr            (FunPtr, Ptr, nullPtr, plusPtr)
import Foreign.Storable       (Storable(..))

#include <bindings.dsl.h>
#include <time.h>
#include <sys/time.h>

#starttype struct tm
#field tm_sec    , CInt
#field tm_min    , CInt
#field tm_hour   , CInt
#field tm_mday   , CInt
#field tm_mon    , CInt
#field tm_year   , CInt
#field tm_wday   , CInt
#field tm_yday   , CInt
#field tm_isdst  , CInt
#field tm_gmtoff , CLong
#field tm_zone   , CString
#stoptype

#starttype struct timeval
#field tv_sec  , CLong
#field tv_usec , CLong
#stoptype

#ccall timegm , Ptr <tm> -> IO CTime
#ccall gmtime_r , Ptr CTime -> Ptr <tm> -> IO (Ptr <tm>)
#ccall gettimeofday , Ptr <timeval> -> Ptr () -> IO CInt

instance Convertible CTime C'tm where
  safeConvert = Right . unsafeLocalState . flip with f
    where f x = alloca $ \ ptr -> c'gmtime_r x ptr >>= peek

instance Convertible C'tm CTime where
  safeConvert = Right . unsafeLocalState . flip with c'timegm

-- | Get the current Unix date and time from the system clock.
getTimeOfDay :: IO C'timeval
getTimeOfDay = with (C'timeval 0 0) $ \ ptr -> c'gettimeofday ptr nullPtr >>= getResult ptr
  where getResult ptr 0 = peek ptr
        getResult _ err = error $ "getTimeOfDay: " ++ show err
