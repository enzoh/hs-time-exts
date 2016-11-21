-- |
-- Module     : Foreign.C.Time
-- Copyright  : 2013-2016 Enzo Haussecker
-- License    : BSD3
-- Maintainer : Enzo Haussecker <enzo@sovereign.io>
-- Stability  : Stable

{-# LANGUAGE LambdaCase   #-}
{-# LANGUAGE TypeFamilies #-}

{-# OPTIONS -fno-warn-orphans      #-}
{-# OPTIONS -fno-warn-unused-binds #-}

module Foreign.C.Time (

  -- * Types
       C'timeval(..)
     , C'tm(..)

  -- * Re-Exports
     , CTime(..)

  -- * System Clock
     , getTimeOfDay

     ) where

import Data.Time.Exts.Base (Human(..))
import Foreign.C.String (CString)
import Foreign.C.Types (CInt(..), CLong, CTime(..))
import Foreign.Marshal.Alloc (alloca)
import Foreign.Marshal.Unsafe (unsafeLocalState)
import Foreign.Marshal.Utils (with)
import Foreign.Ptr (FunPtr, Ptr, nullPtr, plusPtr)
import Foreign.Storable (Storable(..))

#include <bindings.dsl.h>
#include <time.h>
#include <sys/time.h>

#starttype struct tm
#field tm_sec,    CInt
#field tm_min,    CInt
#field tm_hour,   CInt
#field tm_mday,   CInt
#field tm_mon,    CInt
#field tm_year,   CInt
#field tm_wday,   CInt
#field tm_yday,   CInt
#field tm_isdst,  CInt
#field tm_gmtoff, CLong
#field tm_zone,   CString
#stoptype

#starttype struct timeval
#field tv_sec,    CLong
#field tv_usec,   CLong
#stoptype

#ccall timegm, Ptr <tm> -> IO CTime
#ccall gmtime_r, Ptr CTime -> Ptr <tm> -> IO (Ptr <tm>)
#ccall gettimeofday, Ptr <timeval> -> Ptr () -> IO CInt

instance Human CTime where
   type Components CTime = C'tm
   pack = unsafeLocalState . flip with c'timegm
   unpack = unsafeLocalState . flip with f
      where f x = alloca $ \ ptr -> c'gmtime_r x ptr >>= peek

-- |
-- Get the current time from the system clock.
getTimeOfDay :: IO C'timeval
getTimeOfDay = with (C'timeval 0 0) $ \ ptr -> c'gettimeofday ptr nullPtr >>= getResult ptr
   where getResult ptr = \ case 0 -> peek ptr; n -> error $ "getTimeOfDay: " ++ show n
