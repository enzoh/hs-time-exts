---------------------------------------------------------------
-- Copyright (c) 2014, Enzo Haussecker. All rights reserved. --
---------------------------------------------------------------

{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# OPTIONS -Wall                       #-}

-- | Unix timestamps of varying granularity.
module Data.Time.Exts.Unix (

 -- ** Unix Class
       Unix(..)

 -- ** Unix Timestamps
     , UnixDate(..)
     , UnixTime(..)
     , UnixTimeMillis(..)
     , UnixTimeMicros(..)
     , UnixTimeNanos(..)
     , UnixTimePicos(..)
     , UnixDateTime(..)
     , UnixDateTimeMillis(..)
     , UnixDateTimeMicros(..)
     , UnixDateTimeNanos(..)
     , UnixDateTimePicos(..)

 -- ** Create Unix Timestamps
     , createUnixDate
     , createUnixTime
     , createUnixTimeMillis
     , createUnixTimeMicros
     , createUnixTimeNanos
     , createUnixTimePicos
     , createUnixDateTime
     , createUnixDateTimeMillis
     , createUnixDateTimeMicros
     , createUnixDateTimeNanos
     , createUnixDateTimePicos

 -- ** Get Current Unix Timestamps
     , getCurrentUnixDate
     , getCurrentUnixTime
     , getCurrentUnixTimeMillis
     , getCurrentUnixTimeMicros
     , getCurrentUnixTimeNanos
     , getCurrentUnixTimePicos
     , getCurrentUnixDateTime
     , getCurrentUnixDateTimeMillis
     , getCurrentUnixDateTimeMicros
     , getCurrentUnixDateTimeNanos
     , getCurrentUnixDateTimePicos

 -- ** Pretty Unix Timestamps
     , prettyUnixDate
     , prettyUnixTime
     , prettyUnixDateTime

     ) where

import Control.Arrow         ((***), first)
import Control.DeepSeq       (NFData)
import Data.Aeson            (FromJSON, ToJSON)
import Data.Convertible      (Convertible(..), convert)
import Data.Int              (Int16, Int32, Int64)
import Data.Label            (get, mkLabels, modify)
import Data.Time.Exts.Base
import Data.Time.Exts.C
import Data.Typeable         (Typeable)
import Foreign.C.Types       (CLong(..))
import Foreign.Marshal.Utils (with)
import Foreign.Ptr           (castPtr, nullPtr, plusPtr)
import Foreign.Storable      (Storable(..))
import GHC.Generics          (Generic)
import System.Random         (Random(..))
import Text.Printf           (printf)

-- | The Unix timestamp type class.
class Unix u where

    -- | Get the base component of a Unix timestamp.
    unixBase :: u -> Int64

    -- | Get the normalized base component of a Unix timestamp.
    unixNorm :: u -> Int64

-- | Days since Unix epoch.
newtype UnixDate = UnixDate {
    _ud_day_base :: Int32
  } deriving (Eq,FromJSON,Generic,NFData,Ord,Storable,ToJSON,Typeable)

-- | Seconds since midnight (excluding leap seconds).
newtype UnixTime = UnixTime {
    _ut_sec_base :: Int32
  } deriving (Eq,FromJSON,Generic,NFData,Ord,Storable,ToJSON,Typeable)

-- | Milliseconds since midnight (excluding leap seconds).
newtype UnixTimeMillis = UnixTimeMillis {
    _ut_mil_base :: Int32
  } deriving (Eq,FromJSON,Generic,NFData,Ord,Storable,ToJSON,Typeable)

-- | Microseconds since midnight (excluding leap seconds).
newtype UnixTimeMicros = UnixTimeMicros {
    _ut_mic_base :: Int64
  } deriving (Eq,FromJSON,Generic,NFData,Ord,Storable,ToJSON,Typeable)

-- | Nanoseconds since midnight (excluding leap seconds).
newtype UnixTimeNanos = UnixTimeNanos {
    _ut_nan_base :: Int64
  } deriving (Eq,FromJSON,Generic,NFData,Ord,Storable,ToJSON,Typeable)

-- | Picoseconds since midnight (excluding leap seconds).
newtype UnixTimePicos = UnixTimePicos {
    _ut_pic_base :: Int64
  } deriving (Eq,FromJSON,Generic,NFData,Ord,Storable,ToJSON,Typeable)

-- | Seconds since Unix epoch (excluding leap seconds).
newtype UnixDateTime = UnixDateTime {
    _udt_sec_base :: Int64
  } deriving (Eq,FromJSON,Generic,NFData,Ord,Storable,ToJSON,Typeable)

-- | Milliseconds since Unix epoch (excluding leap seconds).
newtype UnixDateTimeMillis = UnixDateTimeMillis {
    _udt_mil_base :: Int64
  } deriving (Eq,FromJSON,Generic,NFData,Ord,Storable,ToJSON,Typeable)

-- | Microseconds since Unix epoch (excluding leap seconds).
newtype UnixDateTimeMicros = UnixDateTimeMicros {
    _udt_mic_base :: Int64
  } deriving (Eq,FromJSON,Generic,NFData,Ord,Storable,ToJSON,Typeable)

-- | Nanoseconds since Unix epoch (excluding leap seconds).
data UnixDateTimeNanos = UnixDateTimeNanos {
    _udt_nan_base :: {-# UNPACK #-} !Int64
  , _udt_nan_nano :: {-# UNPACK #-} !Int16
  } deriving (Eq,Generic,Ord,Typeable)

-- | Picoseconds since Unix epoch (excluding leap seconds).
data UnixDateTimePicos = UnixDateTimePicos {
    _udt_pic_base :: {-# UNPACK #-} !Int64
  , _udt_pic_pico :: {-# UNPACK #-} !Int32
  } deriving (Eq,Generic,Ord,Typeable)

instance FromJSON UnixDateTimeNanos
instance FromJSON UnixDateTimePicos

instance NFData UnixDateTimeNanos
instance NFData UnixDateTimePicos

instance Storable UnixDateTimeNanos where
    sizeOf  _ = 10
    alignment = sizeOf
    peekElemOff ptr  n = do
      let off = 10 * n
      base <- peek . plusPtr ptr $ off
      nano <- peek . plusPtr ptr $ off + 8
      return $! UnixDateTimeNanos base nano
    pokeElemOff ptr  n UnixDateTimeNanos{..} = do
      let off = 10 * n
      poke (plusPtr ptr $ off    ) _udt_nan_base
      poke (plusPtr ptr $ off + 8) _udt_nan_nano

instance Storable UnixDateTimePicos where
    sizeOf  _ = 12
    alignment = sizeOf
    peekElemOff ptr  n = do
      let off = 12 * n
      base <- peek . plusPtr ptr $ off
      pico <- peek . plusPtr ptr $ off + 8
      return $! UnixDateTimePicos base pico
    pokeElemOff ptr  n UnixDateTimePicos{..} = do
      let off = 12 * n
      poke (plusPtr ptr $ off    ) _udt_pic_base
      poke (plusPtr ptr $ off + 8) _udt_pic_pico

instance ToJSON UnixDateTimeNanos
instance ToJSON UnixDateTimePicos

mkLabels [ ''DateTimeStruct
         , ''UnixDate
         , ''UnixTime
         , ''UnixTimeMillis
         , ''UnixTimeMicros
         , ''UnixTimeNanos
         , ''UnixTimePicos
         , ''UnixDateTime
         , ''UnixDateTimeMillis
         , ''UnixDateTimeMicros
         , ''UnixDateTimeNanos
         , ''UnixDateTimePicos
         ]

instance Bounded UnixDate where
    minBound = UnixDate 0
    maxBound = UnixDate 2932896

instance Bounded UnixTime where
    minBound = UnixTime 0
    maxBound = UnixTime 86399

instance Bounded UnixTimeMillis where
    minBound = UnixTimeMillis 0
    maxBound = UnixTimeMillis 86399999

instance Bounded UnixTimeMicros where
    minBound = UnixTimeMicros 0
    maxBound = UnixTimeMicros 86399999999

instance Bounded UnixTimeNanos where
    minBound = UnixTimeNanos 0
    maxBound = UnixTimeNanos 86399999999999

instance Bounded UnixTimePicos where
    minBound = UnixTimePicos 0
    maxBound = UnixTimePicos 86399999999999999

instance Bounded UnixDateTime where
    minBound = UnixDateTime 0
    maxBound = UnixDateTime 253402300799

instance Bounded UnixDateTimeMillis where
    minBound = UnixDateTimeMillis 0
    maxBound = UnixDateTimeMillis 253402300799999

instance Bounded UnixDateTimeMicros where
    minBound = UnixDateTimeMicros 0
    maxBound = UnixDateTimeMicros 253402300799999999

instance Bounded UnixDateTimeNanos where
    minBound = UnixDateTimeNanos 0 0
    maxBound = UnixDateTimeNanos 253402300799999999 999

instance Bounded UnixDateTimePicos where
    minBound = UnixDateTimePicos 0 0
    maxBound = UnixDateTimePicos 253402300799999999 999999

instance Unix UnixDate where
    unixBase = fromIntegral . get ud_day_base
    unixNorm = unixBase

instance Unix UnixTime where
    unixBase = fromIntegral . get ut_sec_base
    unixNorm = unixBase

instance Unix UnixTimeMillis where
    unixBase = fromIntegral . get ut_mil_base
    unixNorm = flip div 1000 . unixBase

instance Unix UnixTimeMicros where
    unixBase = get ut_mic_base
    unixNorm = flip div 1000000 . unixBase

instance Unix UnixTimeNanos where
    unixBase = get ut_nan_base
    unixNorm = flip div 1000000000 . unixBase

instance Unix UnixTimePicos where
    unixBase = get ut_pic_base
    unixNorm = flip div 1000000000000 . unixBase

instance Unix UnixDateTime where
    unixBase = get udt_sec_base
    unixNorm = unixBase

instance Unix UnixDateTimeMillis where
    unixBase = get udt_mil_base
    unixNorm = flip div 1000 . unixBase

instance Unix UnixDateTimeMicros where
    unixBase = get udt_mic_base
    unixNorm = flip div 1000000 . unixBase

instance Unix UnixDateTimeNanos where
    unixBase = get udt_nan_base
    unixNorm = flip div 1000000 . unixBase

instance Unix UnixDateTimePicos where
    unixBase = get udt_pic_base
    unixNorm = flip div 1000000 . unixBase

instance DateTimeMath UnixDate Day where
    date `plus` Day day =
      check "plus{UnixDate,Day}" $
        modify ud_day_base (+ day) date

instance DateTimeMath UnixTime Hour where
    time `plus` Hour hour =
      check "plus{UnixTime,Hour}" $
        modify ut_sec_base (+ fromIntegral hour * 3600) time

instance DateTimeMath UnixTime Minute where
    time `plus` Minute minute =
      check "plus{UnixTime,Minute}" $
        modify ut_sec_base (+ fromIntegral minute * 60) time

instance DateTimeMath UnixTime Second where
    time `plus` Second second =
      check "plus{UnixTime,Second}" $
        modify ut_sec_base (+ fromIntegral second) time

instance DateTimeMath UnixTimeMillis Hour where
    time `plus` Hour hour =
      check "plus{UnixTimeMillis,Hour}" $
        modify ut_mil_base (+ fromIntegral hour * 3600000) time

instance DateTimeMath UnixTimeMillis Minute where
    time `plus` Minute minute =
      check "plus{UnixTimeMillis,Minute}" $
        modify ut_mil_base (+ fromIntegral minute * 60000) time

instance DateTimeMath UnixTimeMillis Second where
    time `plus` Second second =
      check "plus{UnixTimeMillis,Second}" $
        modify ut_mil_base (+ fromIntegral second * 1000) time

instance DateTimeMath UnixTimeMillis Millis where
    time `plus` Millis millis =
      check "plus{UnixTimeMillis,Millis}" $
        modify ut_mil_base (+ fromIntegral millis) time

instance DateTimeMath UnixTimeMicros Hour where
    time `plus` Hour hour =
      check "plus{UnixTimeMicros,Hour}" $
        modify ut_mic_base (+ hour * 3600000000) time

instance DateTimeMath UnixTimeMicros Minute where
    time `plus` Minute minute =
      check "plus{UnixTimeMicros,Minute}" $
        modify ut_mic_base (+ minute * 60000000) time

instance DateTimeMath UnixTimeMicros Second where
    time `plus` Second second =
      check "plus{UnixTimeMicros,Second}" $
        modify ut_mic_base (+ second * 1000000) time

instance DateTimeMath UnixTimeMicros Millis where
    time `plus` Millis millis =
      check "plus{UnixTimeMicros,Millis}" $
        modify ut_mic_base (+ millis * 1000) time

instance DateTimeMath UnixTimeMicros Micros where
    time `plus` Micros micros =
      check "plus{UnixTimeMicros,Micros}" $
        modify ut_mic_base (+ micros) time

instance DateTimeMath UnixTimeNanos Hour where
    time `plus` Hour hour =
      check "plus{UnixTimeNanos,Hour}" $
        modify ut_nan_base (+ hour * 3600000000000) time

instance DateTimeMath UnixTimeNanos Minute where
    time `plus` Minute minute =
      check "plus{UnixTimeNanos,Minute}" $
        modify ut_nan_base (+ minute * 60000000000) time

instance DateTimeMath UnixTimeNanos Second where
    time `plus` Second second =
      check "plus{UnixTimeNanos,Second}" $
        modify ut_nan_base (+ second * 1000000000) time

instance DateTimeMath UnixTimeNanos Millis where
    time `plus` Millis millis =
      check "plus{UnixTimeNanos,Millis}" $
        modify ut_nan_base (+ millis * 1000000) time

instance DateTimeMath UnixTimeNanos Micros where
    time `plus` Micros micros =
      check "plus{UnixTimeNanos,Micros}" $
        modify ut_nan_base (+ micros * 1000) time

instance DateTimeMath UnixTimeNanos Nanos where
    time `plus` Nanos nanos =
      check "plus{UnixTimeNanos,Nanos}" $
        modify ut_nan_base (+ nanos) time

instance DateTimeMath UnixTimePicos Hour where
    time `plus` Hour hour =
      check "plus{UnixTimePicos,Hour}" $
        modify ut_pic_base (+ hour * 3600000000000000) time

instance DateTimeMath UnixTimePicos Minute where
    time `plus` Minute minute =
      check "plus{UnixTimePicos,Minute}" $
        modify ut_pic_base (+ minute * 60000000000000) time

instance DateTimeMath UnixTimePicos Second where
    time `plus` Second second =
      check "plus{UnixTimePicos,Second}" $
        modify ut_pic_base (+ second * 1000000000000) time

instance DateTimeMath UnixTimePicos Millis where
    time `plus` Millis millis =
      check "plus{UnixTimePicos,Millis}" $
        modify ut_pic_base (+ millis * 1000000000) time

instance DateTimeMath UnixTimePicos Micros where
    time `plus` Micros micros =
      check "plus{UnixTimePicos,Micros}" $
        modify ut_pic_base (+ micros * 1000000) time

instance DateTimeMath UnixTimePicos Nanos where
    time `plus` Nanos nanos =
      check "plus{UnixTimePicos,Nanos}" $
        modify ut_pic_base (+ nanos * 1000) time

instance DateTimeMath UnixTimePicos Picos where
    time `plus` Picos picos =
      check "plus{UnixTimePicos,Picos}" $
        modify ut_pic_base (+ picos) time

instance DateTimeMath UnixDateTime Day where
    time `plus` Day day =
      check "plus{UnixDateTime,Day}" $
        modify udt_sec_base (+ fromIntegral day * 86400) time

instance DateTimeMath UnixDateTime Hour where
    time `plus` Hour hour =
      check "plus{UnixDateTime,Hour}" $
        modify udt_sec_base (+ hour * 3600) time

instance DateTimeMath UnixDateTime Minute where
    time `plus` Minute minute =
      check "plus{UnixDateTime,Minute}" $
        modify udt_sec_base (+ minute * 60) time

instance DateTimeMath UnixDateTime Second where
    time `plus` Second second =
      check "plus{UnixDateTime,Second}" $
        modify udt_sec_base (+ second) time

instance DateTimeMath UnixDateTimeMillis Day where
    time `plus` Day day =
      check "plus{UnixDateTimeMillis,Day}" $
        modify udt_mil_base (+ fromIntegral day * 86400000) time

instance DateTimeMath UnixDateTimeMillis Hour where
    time `plus` Hour hour =
      check "plus{UnixDateTimeMillis,Hour}" $
        modify udt_mil_base (+ hour * 3600000) time

instance DateTimeMath UnixDateTimeMillis Minute where
    time `plus` Minute minute =
      check "plus{UnixDateTimeMillis,Minute}" $
        modify udt_mil_base (+ minute * 60000) time

instance DateTimeMath UnixDateTimeMillis Second where
    time `plus` Second second =
      check "plus{UnixDateTimeMillis,Second}" $
        modify udt_mil_base (+ second * 1000) time

instance DateTimeMath UnixDateTimeMillis Millis where
    time `plus` Millis millis =
      check "plus{UnixDateTimeMillis,Millis}" $
        modify udt_mil_base (+ millis) time

instance DateTimeMath UnixDateTimeMicros Day where
    time `plus` Day day =
      check "plus{UnixDateTimeMicros,Day}" $
        modify udt_mic_base (+ fromIntegral day * 86400000000) time

instance DateTimeMath UnixDateTimeMicros Hour where
    time `plus` Hour hour =
      check "plus{UnixDateTimeMicros,Hour}" $
        modify udt_mic_base (+ hour * 3600000000) time

instance DateTimeMath UnixDateTimeMicros Minute where
    time `plus` Minute minute =
      check "plus{UnixDateTimeMicros,Minute}" $
        modify udt_mic_base (+ minute * 60000000) time

instance DateTimeMath UnixDateTimeMicros Second where
    time `plus` Second second =
      check "plus{UnixDateTimeMicros,Second}" $
        modify udt_mic_base (+ second * 1000000) time

instance DateTimeMath UnixDateTimeMicros Millis where
    time `plus` Millis millis =
      check "plus{UnixDateTimeMicros,Millis}" $
        modify udt_mic_base (+ millis * 1000) time

instance DateTimeMath UnixDateTimeMicros Micros where
    time `plus` Micros micros =
      check "plus{UnixDateTimeMicros,Micros}" $
        modify udt_mic_base (+ micros) time

instance DateTimeMath UnixDateTimeNanos Day where
    time `plus` Day day =
      check "plus{UnixDateTimeNanos,Day}" $
        modify udt_nan_base (+ fromIntegral day * 86400000000) time

instance DateTimeMath UnixDateTimeNanos Hour where
    time `plus` Hour hour =
      check "plus{UnixDateTimeNanos,Hour}" $
        modify udt_nan_base (+ hour * 3600000000) time

instance DateTimeMath UnixDateTimeNanos Minute where
    time `plus` Minute minute =
      check "plus{UnixDateTimeNanos,Minute}" $
        modify udt_nan_base (+ minute * 60000000) time

instance DateTimeMath UnixDateTimeNanos Second where
    time `plus` Second second =
      check "plus{UnixDateTimeNanos,Second}" $
        modify udt_nan_base (+ second * 1000000) time

instance DateTimeMath UnixDateTimeNanos Millis where
    time `plus` Millis millis =
      check "plus{UnixDateTimeNanos,Millis}" $
        modify udt_nan_base (+ millis * 1000) time

instance DateTimeMath UnixDateTimeNanos Micros where
    time `plus` Micros micros =
      check "plus{UnixDateTimeNanos,Micros}" $
        modify udt_nan_base (+ micros) time

instance DateTimeMath UnixDateTimeNanos Nanos where
    UnixDateTimeNanos{..} `plus` Nanos nanos =
      check "plus{UnixDateTimeNanos,Nanos}" .
        uncurry UnixDateTimeNanos .
          ((+ _udt_nan_base) *** fromIntegral) .
            flip divMod 1000 $
              fromIntegral _udt_nan_nano + nanos

instance DateTimeMath UnixDateTimePicos Day where
    time `plus` Day day =
      check "plus{UnixDateTimePicos,Day}" $
        modify udt_pic_base (+ fromIntegral day * 86400000000) time

instance DateTimeMath UnixDateTimePicos Hour where
    time `plus` Hour hour =
      check "plus{UnixDateTimePicos,Hour}" $
        modify udt_pic_base (+ hour * 3600000000) time

instance DateTimeMath UnixDateTimePicos Minute where
    time `plus` Minute minute =
      check "plus{UnixDateTimePicos,Minute}" $
        modify udt_pic_base (+ minute * 60000000) time

instance DateTimeMath UnixDateTimePicos Second where
    time `plus` Second second =
      check "plus{UnixDateTimePicos,Second}" $
        modify udt_pic_base (+ second * 1000000) time

instance DateTimeMath UnixDateTimePicos Millis where
    time `plus` Millis millis =
      check "plus{UnixDateTimePicos,Millis}" $
        modify udt_pic_base (+ millis * 1000) time

instance DateTimeMath UnixDateTimePicos Micros where
    time `plus` Micros micros =
      check "plus{UnixDateTimePicos,Micros}" $
        modify udt_pic_base (+ micros) time

instance DateTimeMath UnixDateTimePicos Nanos where
    UnixDateTimePicos{..} `plus` Nanos nanos =
      check "plus{UnixDateTimePicos,Nanos}" .
        uncurry UnixDateTimePicos .
          ((+ _udt_pic_base) *** fromIntegral) .
            flip divMod 1000000 $
              fromIntegral _udt_pic_pico + nanos * 1000

instance DateTimeMath UnixDateTimePicos Picos where
    UnixDateTimePicos{..} `plus` Picos picos =
      check "plus{UnixDateTimePicos,Picos}" .
        uncurry UnixDateTimePicos .
          ((+ _udt_pic_base) *** fromIntegral) .
            flip divMod 1000000 $
              fromIntegral _udt_pic_pico + picos

instance Enum UnixDate where
    succ = flip plus $ Day 1
    pred = flip plus . Day $ - 1
    toEnum   = check "toEnum{UnixDate}" . UnixDate . fromIntegral
    fromEnum = fromIntegral . _ud_day_base

instance Enum UnixTime where
    succ = flip plus $ Second 1
    pred = flip plus . Second $ - 1
    toEnum   = check "toEnum{UnixTime}" . UnixTime . fromIntegral
    fromEnum = fromIntegral . _ut_sec_base

instance Enum UnixTimeMillis where
    succ = flip plus $ Millis 1
    pred = flip plus . Millis $ - 1
    toEnum   = check "toEnum{UnixTimeMillis}" . UnixTimeMillis . fromIntegral
    fromEnum = fromIntegral . _ut_mil_base

instance Enum UnixTimeMicros where
    succ = flip plus $ Micros 1
    pred = flip plus . Micros $ - 1
    toEnum   = check "toEnum{UnixTimeMicros}" . UnixTimeMicros . fromIntegral
    fromEnum = fromIntegral . _ut_mic_base

instance Enum UnixTimeNanos where
    succ = flip plus $ Nanos 1
    pred = flip plus . Nanos $ - 1
    toEnum   = check "toEnum{UnixTimeNanos}" . UnixTimeNanos . fromIntegral
    fromEnum = fromIntegral . _ut_nan_base

instance Enum UnixTimePicos where
    succ = flip plus $ Picos 1
    pred = flip plus . Picos $ - 1
    toEnum   = check "toEnum{UnixTimePicos}" . UnixTimePicos . fromIntegral
    fromEnum = fromIntegral . _ut_pic_base

instance Enum UnixDateTime where
    succ = flip plus $ Second 1
    pred = flip plus . Second $ - 1
    toEnum   = check "toEnum{UnixDateTime}" . UnixDateTime . fromIntegral
    fromEnum = fromIntegral . _udt_sec_base

instance Enum UnixDateTimeMillis where
    succ = flip plus $ Millis 1
    pred = flip plus . Millis $ - 1
    toEnum   = check "toEnum{UnixDateTimeMillis}" . UnixDateTimeMillis . fromIntegral
    fromEnum = fromIntegral . _udt_mil_base

instance Enum UnixDateTimeMicros where
    succ = flip plus $ Micros 1
    pred = flip plus . Micros $ - 1
    toEnum   = check "toEnum{UnixDateTimeMicros}" . UnixDateTimeMicros . fromIntegral
    fromEnum = fromIntegral . _udt_mic_base

-- | Create a Unix date.
--
-- > >>> createUnixDate 2013 November 3
-- > 2013-11-03
--
createUnixDate :: Year -> Month -> Day -> UnixDate
createUnixDate year month day =
   check "createUnixDate" $ UnixDate base
   where Day base = epochToDate year month day

-- | Create a Unix time.
--
-- > >>> createUnixTime 4 52 7
-- > 04:52:07
--
createUnixTime :: Hour -> Minute -> Second -> UnixTime
createUnixTime hour minute second =
   check "createUnixTime" $ UnixTime base
   where base = fromIntegral $ midnightToTime hour minute second

-- | Create a Unix time with millisecond granularity.
--
-- > >>> createUnixTimeMillis 15 22 47 2
-- > 15:22:47.002
--
createUnixTimeMillis :: Hour -> Minute -> Second -> Millis -> UnixTimeMillis
createUnixTimeMillis hour minute second (Millis millis) =
   check "createUnixTimeMillis" $ UnixTimeMillis base
   where Second seconds = midnightToTime hour minute second
         base = fromIntegral seconds * 1000 + fromIntegral millis

-- | Create a Unix time with microsecond granularity.
--
-- > >>> createUnixTimeMicros 10 6 33 575630
-- > 10:06:33.575630
--
createUnixTimeMicros :: Hour -> Minute -> Second -> Micros -> UnixTimeMicros
createUnixTimeMicros hour minute second (Micros micros) =
   check "createUnixTimeMicros" $ UnixTimeMicros base
   where Second seconds = midnightToTime hour minute second
         base = seconds * 1000000 + fromIntegral micros

-- | Create a Unix time with nanosecond granularity.
--
-- > >>> createUnixTimeNanos 23 19 54 465837593
-- > 23:19:54.465837593
--
createUnixTimeNanos :: Hour -> Minute -> Second -> Nanos -> UnixTimeNanos
createUnixTimeNanos hour minute second (Nanos nanos) =
   check "createUnixTimeNanos" $ UnixTimeNanos base
   where Second seconds = midnightToTime hour minute second
         base = seconds * 1000000000 + fromIntegral nanos

-- | Create a Unix time with picosecond granularity.
--
-- > >>> createUnixTimePicos 17 25 36 759230473534
-- > 17:25:36.759230473534
--
createUnixTimePicos :: Hour -> Minute -> Second -> Picos -> UnixTimePicos
createUnixTimePicos hour minute second (Picos picos) =
   check "createUnixTimePicos" $ UnixTimePicos base
   where Second seconds = midnightToTime hour minute second
         base = seconds * 1000000000000 + fromIntegral picos

-- | Create a Unix date and time.
--
-- > >>> createUnixDateTime 2012 April 27 7 37 30
-- > 2012-04-27 07:37:30
--
createUnixDateTime :: Year -> Month -> Day -> Hour -> Minute -> Second -> UnixDateTime
createUnixDateTime year month day hour minute second =
   check "createUnixDateTime" $ UnixDateTime base
   where Second base = epochToTime year month day hour minute second

-- | Create a Unix date and time with millisecond granularity.
--
-- > >>> createUnixDateTimeMillis 2014 February 2 8 52 37 983
-- > 2014-02-02 08:52:37.983
--
createUnixDateTimeMillis :: Year -> Month -> Day -> Hour -> Minute -> Second -> Millis -> UnixDateTimeMillis
createUnixDateTimeMillis year month day hour minute second (Millis millis) =
   check "createUnixDateTimeMillis" $ UnixDateTimeMillis base
   where Second seconds = epochToTime year month day hour minute second
         base = seconds * 1000 + millis

-- | Create a Unix date and time with microsecond granularity.
--
-- > >>> createUnixDateTimeMicros 2011 January 22 17 34 13 138563
-- > 2011-01-22 17:34:13.138563
--
createUnixDateTimeMicros :: Year -> Month -> Day -> Hour -> Minute -> Second -> Micros -> UnixDateTimeMicros
createUnixDateTimeMicros year month day hour minute second (Micros micros) =
   check "createUnixDateTimeMicros" $ UnixDateTimeMicros base
   where Second seconds = epochToTime year month day hour minute second
         base = seconds * 1000000 + micros

-- | Create a Unix date and time with nanosecond granularity.
--
-- > >>> createUnixDateTimeNanos 2012 June 28 1 30 35 688279651
-- > 2012-06-28 01:30:35.688279651
--
createUnixDateTimeNanos :: Year -> Month -> Day -> Hour -> Minute -> Second -> Nanos -> UnixDateTimeNanos
createUnixDateTimeNanos year month day hour minute second (Nanos nanos) =
   check "createUnixDateTimeNanos" $ UnixDateTimeNanos base nano
   where (micros, nano) = fmap fromIntegral $ divMod nanos 1000
         Second seconds = epochToTime year month day hour minute second
         base = seconds * 1000000 + micros

-- | Create a Unix date and time with picosecond granularity.
--
-- > >>> createUnixDateTimePicos 2014 August 2 10 57 54 809479393286
-- > 2014-08-02 10:57:54.809479393286
--
createUnixDateTimePicos :: Year -> Month -> Day -> Hour -> Minute -> Second -> Picos -> UnixDateTimePicos
createUnixDateTimePicos year month day hour minute second (Picos picos) =
   check "createUnixDateTimePicos" $ UnixDateTimePicos base pico
   where (micros, pico) = fmap fromIntegral $ divMod picos 1000000
         Second seconds = epochToTime year month day hour minute second
         base = seconds * 1000000 + micros

-- | Decompose the number of days since
--   January 1st into month and day components.
decompYearToDate :: Day -> Bool -> (Month, Day)
decompYearToDate days leap =
   if leap
   then if days >= 182
        then if days >= 274
             then if days >= 335
                  then (December, days - 334)
                  else if days >= 305
                       then (November, days - 304)
                       else (October , days - 273)
             else if days >= 244
                  then (September, days - 243)
                  else if days >= 213
                       then (August, days - 212)
                       else (July  , days - 181)
        else if days >= 091
             then if days >= 152
                  then (June, days - 151)
                  else if days >= 121
                       then (May  , days - 120)
                       else (April, days - 090)
             else if days >= 060
                  then (March, days - 059)
                  else if days >= 031
                       then (February, days - 030)
                       else (January , days + 001)
   else if days >= 181
        then if days >= 273
             then if days >= 334
                  then (December, days - 333)
                  else if days >= 304
                       then (November, days - 303)
                       else (October , days - 272)
             else if days >= 243
                  then (September, days - 242)
                  else if days >= 212
                       then (August, days - 211)
                       else (July  , days - 180)
        else if days >= 090
             then if days >= 151
                  then (June, days - 150)
                  else if days >= 120
                       then (May  , days - 119)
                       else (April, days - 089)
             else if days >= 059
                  then (March, days - 058)
                  else if days >= 031
                       then (February, days - 030)
                       else (January , days + 001)

-- | Decompose a Unix date into a human-readable format.
decompUnixDate :: UnixDate -> DateStruct
decompUnixDate (UnixDate base) =
   go 1970 $ Day base
   where go :: Year -> Day -> DateStruct
         go !year !days =
            if days >= size
            then go (year + 1) (days - size)
            else DateStruct year month mday wday
            where wday = toEnum $ (fromIntegral base + 4) `mod` 7
                  leap = isLeapYear year
                  size = if leap then 366 else 365
                  (month, mday) = decompYearToDate days leap

-- | Decompose a Unix time into a human-readable format.
decompUnixTime :: UnixTime -> TimeStruct
decompUnixTime (UnixTime base) =
   TimeStruct hour mn sec
   where (hour, mod1) = fromIntegral *** fromIntegral $ divMod base 3600
         (mn  , sec ) = fmap             realToFrac   $ divMod mod1 0060

-- | Decompose a Unix time with millisecond granularity into a human-readable format.
decompUnixTimeMillis :: UnixTimeMillis -> TimeStruct
decompUnixTimeMillis (UnixTimeMillis base) =
   TimeStruct hour mn $ sec + mill / 1000
   where (hour, mod1) = fromIntegral *** fromIntegral $ divMod base 3600000
         (mn  , mod2) =                                 divMod mod1 0060000
         (sec , mill) = realToFrac   *** realToFrac   $ divMod mod2 0001000

-- | Decompose a Unix time with microsecond granularity into a human-readable format.
decompUnixTimeMicros :: UnixTimeMicros -> TimeStruct
decompUnixTimeMicros (UnixTimeMicros base) =
   TimeStruct hour mn $ sec + micr / 1000000
   where (hour, mod1) = Hour       *** Minute     $ divMod base 3600000000
         (mn  , mod2) =                             divMod mod1 0060000000
         (sec , micr) = realToFrac *** realToFrac $ divMod mod2 0001000000

-- | Decompose a Unix time with nanosecond granularity into a human-readable format.
decompUnixTimeNanos :: UnixTimeNanos -> TimeStruct
decompUnixTimeNanos (UnixTimeNanos base) =
   TimeStruct hour mn $ sec + nano / 1000000000
   where (hour, mod1) = Hour       *** Minute     $ divMod base 3600000000000
         (mn  , mod2) =                             divMod mod1 0060000000000
         (sec , nano) = realToFrac *** realToFrac $ divMod mod2 0001000000000

-- | Decompose a Unix time with picosecond granularity into a human-readable format.
decompUnixTimePicos :: UnixTimePicos -> TimeStruct
decompUnixTimePicos (UnixTimePicos base) =
   TimeStruct hour mn $ sec + pico / 1000000000000
   where (hour, mod1) = Hour       *** Minute     $ divMod base 3600000000000000
         (mn  , mod2) =                             divMod mod1 0060000000000000
         (sec , pico) = realToFrac *** realToFrac $ divMod mod2 0001000000000000

-- | Decompose a Unix date and time into a human-readable format.
decompUnixDateTime :: UnixDateTime -> DateTimeStruct
decompUnixDateTime (UnixDateTime base) =
   DateTimeStruct _d_year _d_mon _d_mday _d_wday hour mn sec
   where DateStruct{..} = decompUnixDate $ UnixDate date
         (date, mod1)   = fromIntegral *** Hour         $ divMod base 86400
         (hour, mod2)   = fmap             fromIntegral $ divMod mod1 03600
         (mn  , sec )   = fmap             realToFrac   $ divMod mod2 00060

-- | Decompose a Unix date and time with millisecond granularity into a human-readable format.
decompUnixDateTimeMillis :: UnixDateTimeMillis -> DateTimeStruct
decompUnixDateTimeMillis (UnixDateTimeMillis base) =
   DateTimeStruct _d_year _d_mon _d_mday _d_wday hour mn $ sec + mill / 1000
   where DateStruct{..} = decompUnixDate $ UnixDate date
         (date, mod1)   = fromIntegral *** Hour         $ divMod base 86400000
         (hour, mod2)   = fmap             fromIntegral $ divMod mod1 03600000
         (mn  , mod3)   =                                 divMod mod2 00060000
         (sec , mill)   = realToFrac   *** realToFrac   $ divMod mod3 00001000

-- | Decompose a Unix date and time with microsecond granularity into a human-readable format.
decompUnixDateTimeMicros :: UnixDateTimeMicros -> DateTimeStruct
decompUnixDateTimeMicros (UnixDateTimeMicros base) =
   DateTimeStruct _d_year _d_mon _d_mday _d_wday hour mn $ sec + micr / 1000000
   where DateStruct{..} = decompUnixDate $ UnixDate date
         (date, mod1)   = fromIntegral *** Hour         $ divMod base 86400000000
         (hour, mod2)   = fmap             fromIntegral $ divMod mod1 03600000000
         (mn  , mod3)   =                                 divMod mod2 00060000000
         (sec , micr)   = realToFrac   *** realToFrac   $ divMod mod3 00001000000

-- | Decompose a Unix date and time with nanosecond granularity into a human-readable format.
decompUnixDateTimeNanos :: UnixDateTimeNanos -> DateTimeStruct
decompUnixDateTimeNanos (UnixDateTimeNanos base nano) =
   modify dt_sec (+ fromIntegral nano / 1000000000) . decompUnixDateTimeMicros $ UnixDateTimeMicros base

-- | Decompose a Unix date and time with picosecond granularity into a human-readable format.
decompUnixDateTimePicos :: UnixDateTimePicos -> DateTimeStruct
decompUnixDateTimePicos (UnixDateTimePicos base pico) =
   modify dt_sec (+ fromIntegral pico / 1000000000000) . decompUnixDateTimeMicros $ UnixDateTimeMicros base

instance Convertible UnixDateTime UnixDate where
    safeConvert = Right . UnixDate . fromIntegral . flip div 00000086400 . _udt_sec_base

instance Convertible UnixDateTime UnixTime where
    safeConvert = Right . UnixTime . fromIntegral . flip mod 00000086400 . _udt_sec_base

instance Convertible UnixDateTimeMillis UnixDate where
    safeConvert = Right . UnixDate . fromIntegral . flip div 00086400000 . _udt_mil_base

instance Convertible UnixDateTimeMillis UnixTime where
    safeConvert = Right . UnixTime . fromIntegral . flip mod 00086400000 . _udt_mil_base

instance Convertible UnixDateTimeMicros UnixDate where
    safeConvert = Right . UnixDate . fromIntegral . flip div 86400000000 . _udt_mic_base

instance Convertible UnixDateTimeMicros UnixTime where
    safeConvert = Right . UnixTime . fromIntegral . flip mod 86400000000 . _udt_mic_base

instance Convertible UnixDateTimeNanos UnixDate where
    safeConvert = Right . UnixDate . fromIntegral . flip div 86400000000 . _udt_nan_base

instance Convertible UnixDateTimeNanos UnixTime where
    safeConvert = Right . UnixTime . fromIntegral . flip mod 86400000000 . _udt_nan_base

instance Convertible UnixDateTimePicos UnixDate where
    safeConvert = Right . UnixDate . fromIntegral . flip div 86400000000 . _udt_pic_base

instance Convertible UnixDateTimePicos UnixTime where
    safeConvert = Right . UnixTime . fromIntegral . flip mod 86400000000 . _udt_pic_base

instance Date UnixDate where
    toDateStruct = decompUnixDate
    fromDateStruct DateStruct{..} =
      createUnixDate _d_year _d_mon _d_mday

instance Date UnixDateTime where
    toDateStruct = decompUnixDate . convert
    fromDateStruct DateStruct{..} =
      createUnixDateTime _d_year _d_mon _d_mday 0 0 0

instance Date UnixDateTimeMillis where
    toDateStruct = decompUnixDate . convert
    fromDateStruct DateStruct{..} =
      createUnixDateTimeMillis _d_year _d_mon _d_mday 0 0 0 0

instance Date UnixDateTimeMicros where
    toDateStruct = decompUnixDate . convert
    fromDateStruct DateStruct{..} =
      createUnixDateTimeMicros _d_year _d_mon _d_mday 0 0 0 0

instance Date UnixDateTimeNanos where
    toDateStruct = decompUnixDate . convert
    fromDateStruct DateStruct{..} =
      createUnixDateTimeNanos _d_year _d_mon _d_mday 0 0 0 0

instance Date UnixDateTimePicos where
    toDateStruct = decompUnixDate . convert
    fromDateStruct DateStruct{..} =
      createUnixDateTimePicos _d_year _d_mon _d_mday 0 0 0 0

instance Time UnixTime where
    toTimeStruct = decompUnixTime
    fromTimeStruct TimeStruct{..} =
      createUnixTime _t_hour _t_min sec
      where sec = round _t_sec

instance Time UnixTimeMillis where
    toTimeStruct = decompUnixTimeMillis
    fromTimeStruct TimeStruct{..} =
      createUnixTimeMillis _t_hour _t_min sec mil
      where (sec, mil) = properFracMillis _t_sec

instance Time UnixTimeMicros where
    toTimeStruct = decompUnixTimeMicros
    fromTimeStruct TimeStruct{..} =
      createUnixTimeMicros _t_hour _t_min sec mic
      where (sec, mic) = properFracMicros _t_sec

instance Time UnixTimeNanos where
    toTimeStruct = decompUnixTimeNanos
    fromTimeStruct TimeStruct{..} =
      createUnixTimeNanos _t_hour _t_min sec nan
      where (sec, nan) = properFracNanos _t_sec

instance Time UnixTimePicos where
    toTimeStruct = decompUnixTimePicos
    fromTimeStruct TimeStruct{..} =
      createUnixTimePicos _t_hour _t_min sec pic
      where (sec, pic) = properFracPicos _t_sec

instance Time UnixDateTime where
    toTimeStruct = decompUnixTime . convert
    fromTimeStruct TimeStruct{..} =
      createUnixDateTime 1970 January 1 _t_hour _t_min sec
      where sec = round _t_sec

instance Time UnixDateTimeMillis where
    toTimeStruct = decompUnixTime . convert
    fromTimeStruct TimeStruct{..} =
      createUnixDateTimeMillis 1970 January 1 _t_hour _t_min sec mil
      where (sec, mil) = properFracMillis _t_sec

instance Time UnixDateTimeMicros where
    toTimeStruct = decompUnixTime . convert
    fromTimeStruct TimeStruct{..} =
      createUnixDateTimeMicros 1970 January 1 _t_hour _t_min sec mic
      where (sec, mic) = properFracMicros _t_sec

instance Time UnixDateTimeNanos where
    toTimeStruct = decompUnixTime . convert
    fromTimeStruct TimeStruct{..} =
      createUnixDateTimeNanos 1970 January 1 _t_hour _t_min sec nan
      where (sec, nan) = properFracNanos _t_sec

instance Time UnixDateTimePicos where
    toTimeStruct = decompUnixTime . convert
    fromTimeStruct TimeStruct{..} =
      createUnixDateTimePicos 1970 January 1 _t_hour _t_min sec pic
      where (sec, pic) = properFracPicos _t_sec

instance DateTime UnixDateTime where
    toDateTimeStruct = decompUnixDateTime
    fromDateTimeStruct DateTimeStruct{..} =
      createUnixDateTime _dt_year _dt_mon _dt_mday _dt_hour _dt_min sec
      where sec = round _dt_sec :: Second

instance DateTime UnixDateTimeMillis where
    toDateTimeStruct = decompUnixDateTimeMillis
    fromDateTimeStruct DateTimeStruct{..} =
      createUnixDateTimeMillis _dt_year _dt_mon _dt_mday _dt_hour _dt_min sec mil
      where (sec, mil) = properFracMillis _dt_sec

instance DateTime UnixDateTimeMicros where
    toDateTimeStruct = decompUnixDateTimeMicros
    fromDateTimeStruct DateTimeStruct{..} =
      createUnixDateTimeMicros _dt_year _dt_mon _dt_mday _dt_hour _dt_min sec mic
      where (sec, mic) = properFracMicros _dt_sec

instance DateTime UnixDateTimeNanos where
    toDateTimeStruct = decompUnixDateTimeNanos
    fromDateTimeStruct DateTimeStruct{..} =
      createUnixDateTimeNanos _dt_year _dt_mon _dt_mday _dt_hour _dt_min sec nan
      where (sec, nan) = properFracNanos _dt_sec

instance DateTime UnixDateTimePicos where
    toDateTimeStruct = decompUnixDateTimePicos
    fromDateTimeStruct DateTimeStruct{..} =
      createUnixDateTimePicos _dt_year _dt_mon _dt_mday _dt_hour _dt_min sec pic
      where (sec, pic) = properFracPicos _dt_sec

instance Show UnixDate where
    show date = printf "%04d-%02d-%02d" _d_year mon _d_mday
      where DateStruct{..} = toDateStruct date
            mon = fromEnum _d_mon + 1

instance Show UnixTime where
    show time = printf "%02d:%02d:%02d" _t_hour _t_min sec
      where TimeStruct{..} = toTimeStruct time
            sec = round _t_sec :: Second

instance Show UnixTimeMillis where
    show time = printf "%02d:%02d:%02d.%03d" _t_hour _t_min sec mil
      where TimeStruct{..} = toTimeStruct time
            (sec, mil) = properFracMillis _t_sec

instance Show UnixTimeMicros where
    show time = printf "%02d:%02d:%02d.%06d" _t_hour _t_min sec mic
      where TimeStruct{..} = toTimeStruct time
            (sec, mic) = properFracMicros _t_sec

instance Show UnixTimeNanos where
    show time = printf "%02d:%02d:%02d.%09d" _t_hour _t_min sec nan
      where TimeStruct{..} = toTimeStruct time
            (sec, nan) = properFracNanos _t_sec

instance Show UnixTimePicos where
    show time = printf "%02d:%02d:%02d.%012d" _t_hour _t_min sec pic
      where TimeStruct{..} = toTimeStruct time
            (sec, pic) = properFracPicos _t_sec

instance Show UnixDateTime where
    show time = printf "%04d-%02d-%02d %02d:%02d:%02d" _dt_year mon _dt_mday _dt_hour _dt_min sec
      where DateTimeStruct{..} = toDateTimeStruct time
            mon = fromEnum _dt_mon + 1
            sec = round _dt_sec :: Second

instance Show UnixDateTimeMillis where
    show time = printf "%04d-%02d-%02d %02d:%02d:%02d.%03d" _dt_year mon _dt_mday _dt_hour _dt_min sec mil
      where DateTimeStruct{..} = toDateTimeStruct time
            mon = fromEnum _dt_mon + 1
            (sec, mil) = properFracMillis _dt_sec

instance Show UnixDateTimeMicros where
    show time = printf "%04d-%02d-%02d %02d:%02d:%02d.%06d" _dt_year mon _dt_mday _dt_hour _dt_min sec mic
      where DateTimeStruct{..} = toDateTimeStruct time
            mon = fromEnum _dt_mon + 1
            (sec, mic) = properFracMicros _dt_sec

instance Show UnixDateTimeNanos where
    show time = printf "%04d-%02d-%02d %02d:%02d:%02d.%09d" _dt_year mon _dt_mday _dt_hour _dt_min sec nan
      where DateTimeStruct{..} = toDateTimeStruct time
            mon = fromEnum _dt_mon + 1
            (sec, nan) = properFracNanos _dt_sec

instance Show UnixDateTimePicos where
    show time = printf "%04d-%02d-%02d %02d:%02d:%02d.%012d" _dt_year mon _dt_mday _dt_hour _dt_min sec pic
      where DateTimeStruct{..} = toDateTimeStruct time
            mon = fromEnum _dt_mon + 1
            (sec, pic) = properFracPicos _dt_sec

-- | Get the current Unix date from the system clock.
--
-- > >>> getCurrentUnixDate
-- > 2013-11-03
--
getCurrentUnixDate :: IO UnixDate
getCurrentUnixDate = getCurrentUnixDateTime >>= return . convert

-- | Get the current Unix time from the system clock.
--
-- > >>> getCurrentUnixTime
-- > 05:45:06
--
getCurrentUnixTime :: IO UnixTime
getCurrentUnixTime = getCurrentUnixDateTime >>= return . convert

-- | Get the current Unix time with millisecond granularity from the system clock.
--
-- > >>> getCurrentUnixTimeMillis
-- > 06:30:08.840
--
getCurrentUnixTimeMillis :: IO UnixTimeMillis
getCurrentUnixTimeMillis =
   with (C'timeval 0 0) $ \ ptr ->
   c'gettimeofday ptr nullPtr >>= getResult ptr
   where getResult ptr 0 = peek ptr >>= \ (C'timeval (CLong base) (CLong micr)) ->
           return $! UnixTimeMillis . fromIntegral $ (base `mod` 86400) * 1000 + micr `div` 1000
         getResult _   _ = error "getCurrentUnixTimeMillis: unknown"

-- | Get the current Unix time with microsecond granularity from the system clock.
--
-- > >>> getCurrentUnixTimeMicros
-- > 06:40:39.102910
--
getCurrentUnixTimeMicros :: IO UnixTimeMicros
getCurrentUnixTimeMicros =
   with (C'timeval 0 0) $ \ ptr ->
   c'gettimeofday ptr nullPtr >>= getResult ptr
   where getResult ptr 0 = peek ptr >>= \ (C'timeval (CLong base) (CLong micr)) ->
           return $! UnixTimeMicros $ (base `mod` 86400) * 1000000 + micr
         getResult _   _ = error "getCurrentUnixTimeMicros: unknown"

-- | Get the current Unix time with nanosecond granularity from the system clock.
--
-- > >>> getCurrentUnixTimeNanos
-- > 06:40:45.903610000
--
--   Note that this functions calls @gettimeofday@ behind the scenes. Therefore,
--   the resultant timestamp will have nanosecond granularity, but only microsecond
--   resolution.
getCurrentUnixTimeNanos :: IO UnixTimeNanos
getCurrentUnixTimeNanos =
   with (C'timeval 0 0) $ \ ptr ->
   c'gettimeofday ptr nullPtr >>= getResult ptr
   where getResult ptr 0 = peek ptr >>= \ (C'timeval (CLong base) (CLong micr)) ->
           return $! UnixTimeNanos $ (base `mod` 86400) * 1000000000 + micr * 1000
         getResult _   _ = error "getCurrentUnixTimeNanos: unknown"

-- | Get the current Unix time with picosecond granularity from the system clock.
--
-- > >>> getCurrentUnixTimePicos
-- > 06:47:15.379247000000
--
--   Note that this functions calls @gettimeofday@ behind the scenes. Therefore,
--   the resultant timestamp will have picosecond granularity, but only microsecond
--   resolution.
getCurrentUnixTimePicos :: IO UnixTimePicos
getCurrentUnixTimePicos =
   with (C'timeval 0 0) $ \ ptr ->
   c'gettimeofday ptr nullPtr >>= getResult ptr
   where getResult ptr 0 = peek ptr >>= \ (C'timeval (CLong base) (CLong micr)) ->
           return $! UnixTimePicos $ (base `mod` 86400) * 1000000000000 + micr * 1000000
         getResult _   _ = error "getCurrentUnixTimePicos: unknown"

-- | Get the current Unix date and time from the system clock.
--
-- > >>> getCurrentUnixDateTime
-- > 2013-11-03 23:09:38
--
getCurrentUnixDateTime :: IO UnixDateTime
getCurrentUnixDateTime =
   with (C'timeval 0 0) $ \ ptr ->
   c'gettimeofday ptr nullPtr >>= getResult ptr
   where getResult ptr 0 = peek $ castPtr ptr
         getResult _   _ = error "getCurrentUnixDateTime: unknown"

-- | Get the current Unix date and time with millisecond granularity from the system clock.
--
-- > >>> getCurrentUnixDateTimeMillis
-- > 2013-11-03 23:09:51.986
--
getCurrentUnixDateTimeMillis :: IO UnixDateTimeMillis
getCurrentUnixDateTimeMillis =
   with (C'timeval 0 0) $ \ ptr ->
   c'gettimeofday ptr nullPtr >>= getResult ptr
   where getResult ptr 0 = peek ptr >>= \ (C'timeval (CLong base) (CLong micr)) ->
           return $! UnixDateTimeMillis $ base * 1000 + micr `div` 1000
         getResult _   _ = error "getCurrentUnixDateTimeMillis: unknown"

-- | Get the current Unix date and time with microsecond granularity from the system clock.
--
-- > >>> getCurrentUnixDateTimeMicros
-- > 2013-11-03 23:10:06.498559
--
getCurrentUnixDateTimeMicros :: IO UnixDateTimeMicros
getCurrentUnixDateTimeMicros =
   with (C'timeval 0 0) $ \ ptr ->
   c'gettimeofday ptr nullPtr >>= getResult ptr
   where getResult ptr 0 = peek ptr >>= \ (C'timeval (CLong base) (CLong micr)) ->
           return $! UnixDateTimeMicros $ base * 1000000 + micr
         getResult _   _ = error "getCurrentUnixDateTimeMicros: unknown"

-- | Get the current Unix date and time with nanosecond granularity from the system clock.
--
-- > >>> getCurrentUnixDateTimeNanos
-- > 2013-11-03 23:10:23.697893000
--
--   Note that this functions calls @gettimeofday@ behind the scenes. Therefore, the
--   resultant timestamp will have nanosecond granularity, but only microsecond resolution.
getCurrentUnixDateTimeNanos :: IO UnixDateTimeNanos
getCurrentUnixDateTimeNanos =
   with (C'timeval 0 0) $ \ ptr ->
   c'gettimeofday ptr nullPtr >>= getResult ptr
   where getResult ptr 0 = peek ptr >>= \ (C'timeval (CLong base) (CLong micr)) ->
           return $! UnixDateTimeNanos (base * 1000000 + micr) 0
         getResult _   _ = error "getCurrentUnixDateTimeNanos: unknown"

-- | Get the current Unix date and time with picosecond granularity from the system clock.
--
-- > >>> getCurrentUnixDateTimePicos
-- > 2013-11-03 23:10:44.633032000000
--
--   Note that this functions calls @gettimeofday@ behind the scenes. Therefore, the
--   resultant timestamp will have nanosecond granularity, but only microsecond resolution.
getCurrentUnixDateTimePicos :: IO UnixDateTimePicos
getCurrentUnixDateTimePicos =
   with (C'timeval 0 0) $ \ ptr ->
   c'gettimeofday ptr nullPtr >>= getResult ptr
   where getResult ptr 0 = peek ptr >>= \ (C'timeval (CLong base) (CLong micr)) ->
           return $! UnixDateTimePicos (base * 1000000 + micr) 0
         getResult _   _ = error "getCurrentUnixDateTimePicos: unknown"

-- | Convert a Unix date and time with nanosecond granularity into an integer.
fromNanos :: UnixDateTimeNanos -> Integer
fromNanos (UnixDateTimeNanos base nano) = toInteger base * 0001000 + toInteger nano

-- | Convert a Unix date and time with picosecond granularity into an integer.
fromPicos :: UnixDateTimePicos -> Integer
fromPicos (UnixDateTimePicos base pico) = toInteger base * 1000000 + toInteger pico

-- | Convert an integer into a Unix date and time with nanosecond granularity.
toNanos :: Integer -> UnixDateTimeNanos
toNanos = uncurry UnixDateTimeNanos . (fromInteger *** fromInteger) . flip divMod 0001000

-- | Convert an integer into a Unix date and time with picosecond granularity.
toPicos :: Integer -> UnixDateTimePicos
toPicos = uncurry UnixDateTimePicos . (fromInteger *** fromInteger) . flip divMod 1000000

instance Duration UnixDate Day where
    duration (UnixDate old) (UnixDate new) = Day (new - old)

instance Duration UnixTime Hour where
    duration (UnixTime old) (UnixTime new) = fromIntegral (new - old) `div` 3600

instance Duration UnixTime Minute where
    duration (UnixTime old) (UnixTime new) = fromIntegral (new - old) `div` 60

instance Duration UnixTime Second where
    duration (UnixTime old) (UnixTime new) = fromIntegral (new - old)

instance Duration UnixTimeMillis Hour where
    duration (UnixTimeMillis old) (UnixTimeMillis new) = fromIntegral (new - old) `div` 3600000

instance Duration UnixTimeMillis Minute where
    duration (UnixTimeMillis old) (UnixTimeMillis new) = fromIntegral (new - old) `div` 60000

instance Duration UnixTimeMillis Second where
    duration (UnixTimeMillis old) (UnixTimeMillis new) = fromIntegral (new - old) `div` 1000

instance Duration UnixTimeMillis Millis where
    duration (UnixTimeMillis old) (UnixTimeMillis new) = fromIntegral (new - old)

instance Duration UnixTimeMicros Hour where
    duration (UnixTimeMicros old) (UnixTimeMicros new) = Hour (new - old) `div` 3600000000

instance Duration UnixTimeMicros Minute where
    duration (UnixTimeMicros old) (UnixTimeMicros new) = Minute (new - old) `div` 60000000

instance Duration UnixTimeMicros Second where
    duration (UnixTimeMicros old) (UnixTimeMicros new) = Second (new - old) `div` 1000000

instance Duration UnixTimeMicros Millis where
    duration (UnixTimeMicros old) (UnixTimeMicros new) = Millis (new - old) `div` 1000

instance Duration UnixTimeMicros Micros where
    duration (UnixTimeMicros old) (UnixTimeMicros new) = Micros (new - old)

instance Duration UnixTimeNanos Hour where
    duration (UnixTimeNanos old) (UnixTimeNanos new) = Hour (new - old) `div` 3600000000000

instance Duration UnixTimeNanos Minute where
    duration (UnixTimeNanos old) (UnixTimeNanos new) = Minute (new - old) `div` 60000000000

instance Duration UnixTimeNanos Second where
    duration (UnixTimeNanos old) (UnixTimeNanos new) = Second (new - old) `div` 1000000000

instance Duration UnixTimeNanos Millis where
    duration (UnixTimeNanos old) (UnixTimeNanos new) = Millis (new - old) `div` 1000000

instance Duration UnixTimeNanos Micros where
    duration (UnixTimeNanos old) (UnixTimeNanos new) = Micros (new - old) `div` 1000

instance Duration UnixTimeNanos Nanos where
    duration (UnixTimeNanos old) (UnixTimeNanos new) = Nanos (new - old)

instance Duration UnixTimePicos Hour where
    duration (UnixTimePicos old) (UnixTimePicos new) = Hour (new - old) `div` 3600000000000000

instance Duration UnixTimePicos Minute where
    duration (UnixTimePicos old) (UnixTimePicos new) = Minute (new - old) `div` 60000000000000

instance Duration UnixTimePicos Second where
    duration (UnixTimePicos old) (UnixTimePicos new) = Second (new - old) `div` 1000000000000

instance Duration UnixTimePicos Millis where
    duration (UnixTimePicos old) (UnixTimePicos new) = Millis (new - old) `div` 1000000000

instance Duration UnixTimePicos Micros where
    duration (UnixTimePicos old) (UnixTimePicos new) = Micros (new - old) `div` 1000000

instance Duration UnixTimePicos Nanos where
    duration (UnixTimePicos old) (UnixTimePicos new) = Nanos (new - old) `div` 1000

instance Duration UnixTimePicos Picos where
    duration (UnixTimePicos old) (UnixTimePicos new) = Picos (new - old)

instance Duration UnixDateTime Day where
    duration (UnixDateTime old) (UnixDateTime new) = fromIntegral $ (new - old) `div` 86400

instance Duration UnixDateTime Hour where
    duration (UnixDateTime old) (UnixDateTime new) = Hour (new - old) `div` 3600

instance Duration UnixDateTime Minute where
    duration (UnixDateTime old) (UnixDateTime new) = Minute (new - old) `div` 60

instance Duration UnixDateTime Second where
    duration (UnixDateTime old) (UnixDateTime new) = Second (new - old)

instance Duration UnixDateTimeMillis Day where
    duration (UnixDateTimeMillis old) (UnixDateTimeMillis new) = fromIntegral $ (new - old) `div` 86400000

instance Duration UnixDateTimeMillis Hour where
    duration (UnixDateTimeMillis old) (UnixDateTimeMillis new) = Hour (new - old) `div` 3600000

instance Duration UnixDateTimeMillis Minute where
    duration (UnixDateTimeMillis old) (UnixDateTimeMillis new) = Minute (new - old) `div` 60000

instance Duration UnixDateTimeMillis Second where
    duration (UnixDateTimeMillis old) (UnixDateTimeMillis new) = Second (new - old) `div` 1000

instance Duration UnixDateTimeMillis Millis where
    duration (UnixDateTimeMillis old) (UnixDateTimeMillis new) = Millis (new - old)

instance Duration UnixDateTimeMicros Day where
    duration (UnixDateTimeMicros old) (UnixDateTimeMicros new) = fromIntegral $ (new - old) `div` 86400000000

instance Duration UnixDateTimeMicros Hour where
    duration (UnixDateTimeMicros old) (UnixDateTimeMicros new) = Hour (new - old) `div` 3600000000

instance Duration UnixDateTimeMicros Minute where
    duration (UnixDateTimeMicros old) (UnixDateTimeMicros new) = Minute (new - old) `div` 60000000

instance Duration UnixDateTimeMicros Second where
    duration (UnixDateTimeMicros old) (UnixDateTimeMicros new) = Second (new - old) `div` 1000000

instance Duration UnixDateTimeMicros Millis where
    duration (UnixDateTimeMicros old) (UnixDateTimeMicros new) = Millis (new - old) `div` 1000

instance Duration UnixDateTimeMicros Micros where
    duration (UnixDateTimeMicros old) (UnixDateTimeMicros new) = Micros (new - old)

instance Duration UnixDateTimeNanos Day where
    duration (UnixDateTimeNanos old _) (UnixDateTimeNanos new _) = fromIntegral $ (new - old) `div` 86400000000

instance Duration UnixDateTimeNanos Hour where
    duration (UnixDateTimeNanos old _) (UnixDateTimeNanos new _) = Hour (new - old) `div` 3600000000

instance Duration UnixDateTimeNanos Minute where
    duration (UnixDateTimeNanos old _) (UnixDateTimeNanos new _) = Minute (new - old) `div` 60000000

instance Duration UnixDateTimeNanos Second where
    duration (UnixDateTimeNanos old _) (UnixDateTimeNanos new _) = Second (new - old) `div` 1000000

instance Duration UnixDateTimeNanos Millis where
    duration (UnixDateTimeNanos old _) (UnixDateTimeNanos new _) = Millis (new - old) `div` 1000

instance Duration UnixDateTimeNanos Micros where
    duration (UnixDateTimeNanos old _) (UnixDateTimeNanos new _) = Micros (new - old)

instance Duration UnixDateTimeNanos Nanos where
    duration old new =
      if res < toInteger (maxBound::Int64) then fromInteger res
      else error "duration{UnixDateTimeNanos,Nanos}: integer overflow"
      where res = (fromNanos new - fromNanos old)

instance Duration UnixDateTimePicos Day where
    duration (UnixDateTimePicos old _) (UnixDateTimePicos new _) = fromIntegral $ (new - old) `div` 86400000000

instance Duration UnixDateTimePicos Hour where
    duration (UnixDateTimePicos old _) (UnixDateTimePicos new _) = Hour (new - old) `div` 3600000000

instance Duration UnixDateTimePicos Minute where
    duration (UnixDateTimePicos old _) (UnixDateTimePicos new _) = Minute (new - old) `div` 60000000

instance Duration UnixDateTimePicos Second where
    duration (UnixDateTimePicos old _) (UnixDateTimePicos new _) = Second (new - old) `div` 1000000

instance Duration UnixDateTimePicos Millis where
    duration (UnixDateTimePicos old _) (UnixDateTimePicos new _) = Millis (new - old) `div` 1000

instance Duration UnixDateTimePicos Micros where
    duration (UnixDateTimePicos old _) (UnixDateTimePicos new _) = Micros (new - old)

instance Duration UnixDateTimePicos Nanos where
    duration old new =
      if res < toInteger (maxBound::Int64) then fromInteger res
      else error "duration{UnixDateTimePicos,Nanos}: integer overflow"
      where res = (fromPicos new - fromPicos old) `div` 1000

instance Duration UnixDateTimePicos Picos where
    duration old new =
      if res < toInteger (maxBound::Int64) then fromInteger res
      else error "duration{UnixDateTimePicos,Picos}: integer overflow"
      where res = (fromPicos new - fromPicos old)

instance Random UnixDate where
    random        = first toEnum . randomR (0,2932896)
    randomR (a,b) = first toEnum . randomR (fromEnum a, fromEnum b)

instance Random UnixTime where
    random        = first toEnum . randomR (0,86399)
    randomR (a,b) = first toEnum . randomR (fromEnum a, fromEnum b)

instance Random UnixTimeMillis where
    random        = first toEnum . randomR (0,86399999)
    randomR (a,b) = first toEnum . randomR (fromEnum a, fromEnum b)

instance Random UnixTimeMicros where
    random        = first toEnum . randomR (0,86399999999)
    randomR (a,b) = first toEnum . randomR (fromEnum a, fromEnum b)

instance Random UnixTimeNanos where
    random        = first toEnum . randomR (0,86399999999999)
    randomR (a,b) = first toEnum . randomR (fromEnum a, fromEnum b)

instance Random UnixTimePicos where
    random        = first toEnum . randomR (0,86399999999999999)
    randomR (a,b) = first toEnum . randomR (fromEnum a, fromEnum b)

instance Random UnixDateTime where
    random        = first toEnum . randomR (0,253402300799)
    randomR (a,b) = first toEnum . randomR (fromEnum a, fromEnum b)

instance Random UnixDateTimeMillis where
    random        = first toEnum . randomR (0,253402300799999)
    randomR (a,b) = first toEnum . randomR (fromEnum a, fromEnum b)

instance Random UnixDateTimeMicros where
    random        = first toEnum . randomR (0,253402300799999999)
    randomR (a,b) = first toEnum . randomR (fromEnum a, fromEnum b)

instance Random UnixDateTimeNanos where
    random        = first toNanos . randomR (0,253402300799999999999)
    randomR (a,b) = first toNanos . randomR (fromNanos a, fromNanos b)

instance Random UnixDateTimePicos where
    random        = first toPicos . randomR (0,253402300799999999999999)
    randomR (a,b) = first toPicos . randomR (fromPicos a, fromPicos b)

-- | Show a Unix date as a pretty string.
--
-- > >>> prettyUnixDate $ createUnixDate 2014 August 16
-- > "Saturday, August 16th, 2014"
--
prettyUnixDate :: (Unix d, Date d) => d -> String
prettyUnixDate date =
   printf "%s, %s %s, %04d" wday mon mday _d_year
   where DateStruct{..} = toDateStruct date
         wday = show _d_wday
         mon  = show _d_mon
         mday = show _d_mday ++ showSuffix _d_mday

-- | Show a Unix time as a pretty string.
--
-- > >>> getCurrentUnixTime >>= putStrLn . prettyUnixTime 
-- > 9:12 AM
--
prettyUnixTime :: (Unix t, Time t) => t -> String
prettyUnixTime time =
   printf "%d:%02d %s" hour _t_min ampm 
   where TimeStruct{..} = toTimeStruct time
         ampm = showPeriod _t_hour
         hour | _t_hour == 00 = 12
              | _t_hour <= 12 = _t_hour
              | otherwise     = _t_hour - 12

-- | Show a Unix date and time as a pretty string.
--
-- > >>> getCurrentUnixDateTime >>= return . prettyUnixDateTime 
-- > "6:44 AM, Tuesday, December 31st, 2013"
--
prettyUnixDateTime :: (Unix dt, DateTime dt) => dt -> String
prettyUnixDateTime time =
   printf str hour _dt_min ampm wday mon mday _dt_year
   where DateTimeStruct{..} = toDateTimeStruct time
         str  = "%d:%02d %s, %s, %s %s, %04d"
         wday = show _dt_wday
         mon  = show _dt_mon
         mday = show _dt_mday ++ showSuffix _dt_mday
         ampm = showPeriod _dt_hour
         hour | _dt_hour == 00 = 12
              | _dt_hour <= 12 = _dt_hour
              | otherwise      = _dt_hour - 12

-- | Perform a bounds check on the given Unix timestamp.
check :: forall a . Bounded a => Ord a => Unix a => String -> a -> a
check f x =
   if minBound <= x && x <= maxBound then x
   else error $ f ++ ": base (" ++ base ++ ") out of bounds (" ++ bounds ++ ")"
   where base   = show (unixBase x)
         bounds = show (unixBase (minBound::a)) ++ "," ++ show (unixBase (maxBound::a))
