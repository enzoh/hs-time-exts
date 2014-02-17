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
     , UnixDateTime(..)
     , UnixDateTimeMillis(..)
     , UnixDateTimeMicros(..)
     , UnixDateTimeNanos(..)
     , UnixDateTimePicos(..)

 -- ** Create Unix Timestamps
     , createUnixDate
     , createUnixDateTime
     , createUnixDateTimeMillis
     , createUnixDateTimeMicros
     , createUnixDateTimeNanos
     , createUnixDateTimePicos

 -- ** Get Current Unix Timestamps
     , getCurrentUnixDate
     , getCurrentUnixDateTime
     , getCurrentUnixDateTimeMillis
     , getCurrentUnixDateTimeMicros
     , getCurrentUnixDateTimeNanos
     , getCurrentUnixDateTimePicos

 -- ** Pretty Unix Timestamps
     , prettyUnixDate
     , prettyUnixDateTime

     ) where

import Control.Arrow ((***), first)
import Control.DeepSeq (NFData)
import Data.Aeson (FromJSON, ToJSON)
import Data.Convertible (Convertible(..), convert)
import Data.Int (Int16, Int32, Int64)
import Data.Label (get, mkLabels, modify)
import Data.Time.Exts.Base
import Data.Time.Exts.C
import Data.Typeable (Typeable)
import Foreign.C.Types (CLong(..))
import Foreign.Marshal.Utils (with)
import Foreign.Ptr (castPtr, nullPtr, plusPtr)
import Foreign.Storable (Storable(..))
import GHC.Generics (Generic)
import System.Random (Random(..))
import Text.Printf (printf)

-- | The Unix timestamp type class.
class Unix u where

    -- | Get the base component of a Unix timestamp.
    unixBase :: u -> Int64

    -- | Get the normalized base component of a Unix timestamp.
    unixNorm :: u -> Int64

-- | Days since Unix epoch.
newtype UnixDate = UnixDate {
    _uni_day_base :: Int32
  } deriving (Eq,FromJSON,Generic,NFData,Ord,Storable,ToJSON,Typeable)

-- | Seconds since Unix epoch (excluding leap seconds).
newtype UnixDateTime = UnixDateTime {
    _uni_sec_base :: Int64
  } deriving (Eq,FromJSON,Generic,NFData,Ord,Storable,ToJSON,Typeable)

-- | Milliseconds since Unix epoch (excluding leap seconds).
newtype UnixDateTimeMillis = UnixDateTimeMillis {
    _uni_mil_base :: Int64
  } deriving (Eq,FromJSON,Generic,NFData,Ord,Storable,ToJSON,Typeable)

-- | Microseconds since Unix epoch (excluding leap seconds).
newtype UnixDateTimeMicros = UnixDateTimeMicros {
    _uni_mic_base :: Int64
  } deriving (Eq,FromJSON,Generic,NFData,Ord,Storable,ToJSON,Typeable)

-- | Nanoseconds since Unix epoch (excluding leap seconds).
data UnixDateTimeNanos = UnixDateTimeNanos {
    _uni_nan_base :: {-# UNPACK #-} !Int64
  , _uni_nan_nano :: {-# UNPACK #-} !Int16
  } deriving (Eq,Generic,Ord,Typeable)

-- | Picoseconds since Unix epoch (excluding leap seconds).
data UnixDateTimePicos = UnixDateTimePicos {
    _uni_pic_base :: {-# UNPACK #-} !Int64
  , _uni_pic_pico :: {-# UNPACK #-} !Int32
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
      poke (plusPtr ptr $ off    ) _uni_nan_base
      poke (plusPtr ptr $ off + 8) _uni_nan_nano

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
      poke (plusPtr ptr $ off    ) _uni_pic_base
      poke (plusPtr ptr $ off + 8) _uni_pic_pico

instance ToJSON UnixDateTimeNanos
instance ToJSON UnixDateTimePicos

mkLabels [ ''DateTimeStruct
         , ''UnixDate
         , ''UnixDateTime
         , ''UnixDateTimeMillis
         , ''UnixDateTimeMicros
         , ''UnixDateTimeNanos
         , ''UnixDateTimePicos
         ]

instance Bounded UnixDate where
    minBound = UnixDate 0
    maxBound = UnixDate 2932896

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
    unixBase = fromIntegral . get uni_day_base
    unixNorm = unixBase

instance Unix UnixDateTime where
    unixBase = get uni_sec_base
    unixNorm = unixBase

instance Unix UnixDateTimeMillis where
    unixBase = get uni_mil_base
    unixNorm = flip div 1000 . unixBase

instance Unix UnixDateTimeMicros where
    unixBase = get uni_mic_base
    unixNorm = flip div 1000000 . unixBase

instance Unix UnixDateTimeNanos where
    unixBase = get uni_nan_base
    unixNorm = flip div 1000000 . unixBase

instance Unix UnixDateTimePicos where
    unixBase = get uni_pic_base
    unixNorm = flip div 1000000 . unixBase

instance DateTimeMath UnixDate Day where
    date `plus` Day day =
      check "plus{UnixDate,Day}" $
        modify uni_day_base (+ day) date

instance DateTimeMath UnixDateTime Day where
    time `plus` Day day =
      check "plus{UnixDateTime,Day}" $
        modify uni_sec_base (+ fromIntegral day * 86400) time

instance DateTimeMath UnixDateTimeMillis Day where
    time `plus` Day day =
      check "plus{UnixDateTimeMillis,Day}" $
        modify uni_mil_base (+ fromIntegral day * 86400000) time

instance DateTimeMath UnixDateTimeMicros Day where
    time `plus` Day day =
      check "plus{UnixDateTimeMicros,Day}" $
        modify uni_mic_base (+ fromIntegral day * 86400000000) time

instance DateTimeMath UnixDateTimeNanos Day where
    time `plus` Day day =
      check "plus{UnixDateTimeNanos,Day}" $
        modify uni_nan_base (+ fromIntegral day * 86400000000) time

instance DateTimeMath UnixDateTimePicos Day where
    time `plus` Day day =
      check "plus{UnixDateTimePicos,Day}" $
        modify uni_pic_base (+ fromIntegral day * 86400000000) time

instance DateTimeMath UnixDateTime Hour where
    time `plus` Hour hour =
      check "plus{UnixDateTime,Hour}" $
        modify uni_sec_base (+ hour * 3600) time

instance DateTimeMath UnixDateTimeMillis Hour where
    time `plus` Hour hour =
      check "plus{UnixDateTimeMillis,Hour}" $
        modify uni_mil_base (+ hour * 3600000) time

instance DateTimeMath UnixDateTimeMicros Hour where
    time `plus` Hour hour =
      check "plus{UnixDateTimeMicros,Hour}" $
        modify uni_mic_base (+ hour * 3600000000) time

instance DateTimeMath UnixDateTimeNanos Hour where
    time `plus` Hour hour =
      check "plus{UnixDateTimeNanos,Hour}" $
        modify uni_nan_base (+ hour * 3600000000) time

instance DateTimeMath UnixDateTimePicos Hour where
    time `plus` Hour hour =
      check "plus{UnixDateTimePicos,Hour}" $
        modify uni_pic_base (+ hour * 3600000000) time

instance DateTimeMath UnixDateTime Minute where
    time `plus` Minute minute =
      check "plus{UnixDateTime,Minute}" $
        modify uni_sec_base (+ minute * 60) time

instance DateTimeMath UnixDateTimeMillis Minute where
    time `plus` Minute minute =
      check "plus{UnixDateTimeMillis,Minute}" $
        modify uni_mil_base (+ minute * 60000) time

instance DateTimeMath UnixDateTimeMicros Minute where
    time `plus` Minute minute =
      check "plus{UnixDateTimeMicros,Minute}" $
        modify uni_mic_base (+ minute * 60000000) time

instance DateTimeMath UnixDateTimeNanos Minute where
    time `plus` Minute minute =
      check "plus{UnixDateTimeNanos,Minute}" $
        modify uni_nan_base (+ minute * 60000000) time

instance DateTimeMath UnixDateTimePicos Minute where
    time `plus` Minute minute =
      check "plus{UnixDateTimePicos,Minute}" $
        modify uni_pic_base (+ minute * 60000000) time

instance DateTimeMath UnixDateTime Second where
    time `plus` Second second =
      check "plus{UnixDateTime,Second}" $
        modify uni_sec_base (+ second) time

instance DateTimeMath UnixDateTimeMillis Second where
    time `plus` Second second =
      check "plus{UnixDateTimeMillis,Second}" $
        modify uni_mil_base (+ second * 1000) time

instance DateTimeMath UnixDateTimeMicros Second where
    time `plus` Second second =
      check "plus{UnixDateTimeMicros,Second}" $
        modify uni_mic_base (+ second * 1000000) time

instance DateTimeMath UnixDateTimeNanos Second where
    time `plus` Second second =
      check "plus{UnixDateTimeNanos,Second}" $
        modify uni_nan_base (+ second * 1000000) time

instance DateTimeMath UnixDateTimePicos Second where
    time `plus` Second second =
      check "plus{UnixDateTimePicos,Second}" $
        modify uni_pic_base (+ second * 1000000) time

instance DateTimeMath UnixDateTimeMillis Millis where
    time `plus` Millis millis =
      check "plus{UnixDateTimeMillis,Millis}" $
        modify uni_mil_base (+ millis) time

instance DateTimeMath UnixDateTimeMicros Millis where
    time `plus` Millis millis =
      check "plus{UnixDateTimeMicros,Millis}" $
        modify uni_mic_base (+ millis * 1000) time

instance DateTimeMath UnixDateTimeNanos Millis where
    time `plus` Millis millis =
      check "plus{UnixDateTimeNanos,Millis}" $
        modify uni_nan_base (+ millis * 1000) time

instance DateTimeMath UnixDateTimePicos Millis where
    time `plus` Millis millis =
      check "plus{UnixDateTimePicos,Millis}" $
        modify uni_pic_base (+ millis * 1000) time

instance DateTimeMath UnixDateTimeMicros Micros where
    time `plus` Micros micros =
      check "plus{UnixDateTimeMicros,Micros}" $
        modify uni_mic_base (+ micros) time

instance DateTimeMath UnixDateTimeNanos Micros where
    time `plus` Micros micros =
      check "plus{UnixDateTimeNanos,Micros}" $
        modify uni_nan_base (+ micros) time

instance DateTimeMath UnixDateTimePicos Micros where
    time `plus` Micros micros =
      check "plus{UnixDateTimePicos,Micros}" $
        modify uni_pic_base (+ micros) time

instance DateTimeMath UnixDateTimeNanos Nanos where
    UnixDateTimeNanos{..} `plus` Nanos nanos =
      check "plus{UnixDateTimeNanos,Nanos}" .
        uncurry UnixDateTimeNanos .
          both (+ _uni_nan_base) fromIntegral .
            flip divMod 1000 $
              fromIntegral _uni_nan_nano + nanos

instance DateTimeMath UnixDateTimePicos Nanos where
    UnixDateTimePicos{..} `plus` Nanos nanos =
      check "plus{UnixDateTimePicos,Nanos}" .
        uncurry UnixDateTimePicos .
          both (+ _uni_pic_base) fromIntegral .
            flip divMod 1000000 $
              fromIntegral _uni_pic_pico + nanos * 1000

instance DateTimeMath UnixDateTimePicos Picos where
    UnixDateTimePicos{..} `plus` Picos picos =
      check "plus{UnixDateTimePicos,Picos}" .
        uncurry UnixDateTimePicos .
          both (+ _uni_pic_base) fromIntegral .
            flip divMod 1000000 $
              fromIntegral _uni_pic_pico + picos

instance Enum UnixDate where
    succ = flip plus $ Day 1
    pred = flip plus . Day $ - 1
    toEnum   = check "toEnum{UnixDate}" . UnixDate . fromIntegral
    fromEnum = fromIntegral . _uni_day_base

instance Enum UnixDateTime where
    succ = flip plus $ Second 1
    pred = flip plus . Second $ - 1
    toEnum   = check "toEnum{UnixDateTime}" . UnixDateTime . fromIntegral
    fromEnum = fromIntegral . _uni_sec_base

instance Enum UnixDateTimeMillis where
    succ = flip plus $ Millis 1
    pred = flip plus . Millis $ - 1
    toEnum   = check "toEnum{UnixDateTimeMillis}" . UnixDateTimeMillis . fromIntegral
    fromEnum = fromIntegral . _uni_mil_base

instance Enum UnixDateTimeMicros where
    succ = flip plus $ Micros 1
    pred = flip plus . Micros $ - 1
    toEnum   = check "toEnum{UnixDateTimeMicros}" . UnixDateTimeMicros . fromIntegral
    fromEnum = fromIntegral . _uni_mic_base

-- | Create a Unix date.
--
-- > >>> createUnixDate 2013 November 03
-- > 2013-11-03
--
createUnixDate :: Year -> Month -> Day -> UnixDate
createUnixDate year month day =
   check "createUnixDate" $ UnixDate base
   where Day base = epochToDate year month day

-- | Create a Unix date and time.
--
-- > >>> createUnixDateTime 2012 April 27 07 37 30
-- > 2012-04-27 07:37:30
--
createUnixDateTime :: Year -> Month -> Day -> Hour -> Minute -> Second -> UnixDateTime
createUnixDateTime year month day hour minute second =
   check "createUnixDateTime" $ UnixDateTime base
   where Second base = epochToTime year month day hour minute second

-- | Create a Unix date and time with millisecond granularity.
--
-- > >>> createUnixDateTimeMillis 2014 February 02 08 52 37 983
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
-- > >>> createUnixDateTimeNanos 2012 June 28 01 30 35 688279651
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
-- > >>> createUnixDateTimePicos 2014 August 02 10 57 54 809479393286
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

-- | Decompose a Unix date and time into a human-readable format.
decompUnixDateTime :: UnixDateTime -> DateTimeStruct
decompUnixDateTime (UnixDateTime base) =
   DateTimeStruct _d_year _d_mon _d_mday _d_wday hour mn sec
   where DateStruct{..} = decompUnixDate $ UnixDate date
         (date, mod1)   = both fromIntegral Hour $ divMod base 86400
         (hour, mod2)   = fmap fromIntegral      $ divMod mod1 03600
         (mn  , sec )   = fmap realToFrac        $ divMod mod2 00060

-- | Decompose a Unix date and time with millisecond granularity into a human-readable format.
decompUnixDateTimeMillis :: UnixDateTimeMillis -> DateTimeStruct
decompUnixDateTimeMillis (UnixDateTimeMillis base) =
   DateTimeStruct _d_year _d_mon _d_mday _d_wday hour mn $ sec + mill / 0001000
   where DateStruct{..} = decompUnixDate $ UnixDate date
         (date, mod1)   = both fromIntegral Hour     $ divMod base 86400000
         (hour, mod2)   = fmap fromIntegral          $ divMod mod1 03600000
         (mn  , mod3)   =                              divMod mod2 00060000
         (sec , mill)   = both realToFrac realToFrac $ divMod mod3 00001000

-- | Decompose a Unix date and time with microsecond granularity into a human-readable format.
decompUnixDateTimeMicros :: UnixDateTimeMicros -> DateTimeStruct
decompUnixDateTimeMicros (UnixDateTimeMicros base) =
   DateTimeStruct _d_year _d_mon _d_mday _d_wday hour mn $ sec + micr / 1000000
   where DateStruct{..} = decompUnixDate $ UnixDate date
         (date, mod1)   = both fromIntegral Hour     $ divMod base 86400000000
         (hour, mod2)   = fmap fromIntegral          $ divMod mod1 03600000000
         (mn  , mod3)   =                              divMod mod2 00060000000
         (sec , micr)   = both realToFrac realToFrac $ divMod mod3 00001000000

-- | Decompose a Unix date and time with nanosecond granularity into a human-readable format.
decompUnixDateTimeNanos :: UnixDateTimeNanos -> DateTimeStruct
decompUnixDateTimeNanos (UnixDateTimeNanos base nano) =
   modify dt_sec (+ fromIntegral nano / 0001000000000) . decompUnixDateTimeMicros $ UnixDateTimeMicros base

-- | Decompose a Unix date and time with picosecond granularity into a human-readable format.
decompUnixDateTimePicos :: UnixDateTimePicos -> DateTimeStruct
decompUnixDateTimePicos (UnixDateTimePicos base pico) =
   modify dt_sec (+ fromIntegral pico / 1000000000000) . decompUnixDateTimeMicros $ UnixDateTimeMicros base

instance Convertible UnixDateTime UnixDate where
    safeConvert = Right . UnixDate . fromIntegral . flip div 00000086400 . _uni_sec_base

instance Convertible UnixDateTimeMillis UnixDate where
    safeConvert = Right . UnixDate . fromIntegral . flip div 00086400000 . _uni_mil_base

instance Convertible UnixDateTimeMicros UnixDate where
    safeConvert = Right . UnixDate . fromIntegral . flip div 86400000000 . _uni_mic_base

instance Convertible UnixDateTimeNanos UnixDate where
    safeConvert = Right . UnixDate . fromIntegral . flip div 86400000000 . _uni_nan_base

instance Convertible UnixDateTimePicos UnixDate where
    safeConvert = Right . UnixDate . fromIntegral . flip div 86400000000 . _uni_pic_base

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

instance Show UnixDateTime where
    show time = printf "%04d-%02d-%02d %02d:%02d:%02d" _dt_year mon _dt_mday _dt_hour _dt_min sec
      where DateTimeStruct{..} = toDateTimeStruct time
            mon = fromEnum _dt_mon + 1
            sec = round _dt_sec :: Second

instance Show UnixDateTimeMillis where
    show time = printf "%04d-%02d-%02d %02d:%02d:%02d.%03d" _dt_year mon _dt_mday _dt_hour _dt_min sec mill
      where DateTimeStruct{..} = toDateTimeStruct time
            mon = fromEnum _dt_mon + 1
            (sec, mill) = properFracMillis _dt_sec

instance Show UnixDateTimeMicros where
    show time = printf "%04d-%02d-%02d %02d:%02d:%02d.%06d" _dt_year mon _dt_mday _dt_hour _dt_min sec micr
      where DateTimeStruct{..} = toDateTimeStruct time
            mon = fromEnum _dt_mon + 1
            (sec, micr) = properFracMicros _dt_sec

instance Show UnixDateTimeNanos where
    show time = printf "%04d-%02d-%02d %02d:%02d:%02d.%09d" _dt_year mon _dt_mday _dt_hour _dt_min sec nano
      where DateTimeStruct{..} = toDateTimeStruct time
            mon = fromEnum _dt_mon + 1
            (sec, nano) = properFracNanos _dt_sec

instance Show UnixDateTimePicos where
    show time = printf "%04d-%02d-%02d %02d:%02d:%02d.%012d" _dt_year mon _dt_mday _dt_hour _dt_min sec pico
      where DateTimeStruct{..} = toDateTimeStruct time
            mon = fromEnum _dt_mon + 1
            (sec, pico) = properFracPicos _dt_sec

-- | Get the current Unix date from the system clock.
--
-- > >>> getCurrentUnixDate
-- > 2013-11-03
--
getCurrentUnixDate :: IO UnixDate
getCurrentUnixDate = getCurrentUnixDateTime >>= return . convert

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
--   Note that this functions calls @gettimeofday@ behind the scenes. Therefore, the resultant
--   timestamp will have nanosecond granularity, but only microsecond resolution.
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
--   Note that this functions calls @gettimeofday@ behind the scenes. Therefore, the resultant
--   timestamp will have picosecond granularity, but only microsecond resolution.
getCurrentUnixDateTimePicos :: IO UnixDateTimePicos
getCurrentUnixDateTimePicos =
   with (C'timeval 0 0) $ \ ptr ->
   c'gettimeofday ptr nullPtr >>= getResult ptr
   where getResult ptr 0 = peek ptr >>= \ (C'timeval (CLong base) (CLong micr)) ->
           return $! UnixDateTimePicos (base * 1000000 + micr) 0
         getResult _   _ = error "getCurrentUnixDateTimePicos: unknown"

-- | Convert a Unix date and time with nanosecond granularity into an integer.
fromNano :: UnixDateTimeNanos -> Integer
fromNano (UnixDateTimeNanos base nano) = toInteger base * 0001000 + toInteger nano

-- | Convert a Unix date and time with picosecond granularity into an integer.
fromPico :: UnixDateTimePicos -> Integer
fromPico (UnixDateTimePicos base pico) = toInteger base * 1000000 + toInteger pico

-- | Convert an integer into a Unix date and time with nanosecond granularity.
toNano :: Integer -> UnixDateTimeNanos
toNano = uncurry UnixDateTimeNanos . both fromInteger fromInteger . flip divMod 1000

-- | Convert an integer into a Unix date and time with picosecond granularity.
toPico :: Integer -> UnixDateTimePicos
toPico = uncurry UnixDateTimePicos . both fromInteger fromInteger . flip divMod 1000000

instance Duration UnixDate Day where
    duration (UnixDate old) (UnixDate new) = Day (new - old)

instance Duration UnixDateTime Second where
    duration (UnixDateTime old) (UnixDateTime new) = Second (new - old)

instance Duration UnixDateTimeMillis Second where
    duration (UnixDateTimeMillis old  ) (UnixDateTimeMillis new  ) = Second (new - old) `div` 1000

instance Duration UnixDateTimeMicros Second where
    duration (UnixDateTimeMicros old  ) (UnixDateTimeMicros new  ) = Second (new - old) `div` 1000000

instance Duration UnixDateTimeNanos Second where
    duration (UnixDateTimeNanos  old _) (UnixDateTimeNanos  new _) = Second (new - old) `div` 1000000

instance Duration UnixDateTimePicos Second where
    duration (UnixDateTimePicos  old _) (UnixDateTimePicos  new _) = Second (new - old) `div` 1000000

instance Duration UnixDateTimeMillis Millis where
    duration (UnixDateTimeMillis old  ) (UnixDateTimeMillis new  ) = Millis (new - old)

instance Duration UnixDateTimeMicros Millis where
    duration (UnixDateTimeMicros old  ) (UnixDateTimeMicros new  ) = Millis (new - old) `div` 1000

instance Duration UnixDateTimeNanos Millis where
    duration (UnixDateTimeNanos  old _) (UnixDateTimeNanos  new _) = Millis (new - old) `div` 1000

instance Duration UnixDateTimePicos Millis where
    duration (UnixDateTimePicos  old _) (UnixDateTimePicos  new _) = Millis (new - old) `div` 1000

instance Duration UnixDateTimeMicros Micros where
    duration (UnixDateTimeMicros old  ) (UnixDateTimeMicros new  ) = Micros (new - old)

instance Duration UnixDateTimeNanos Micros where
    duration (UnixDateTimeNanos  old _) (UnixDateTimeNanos  new _) = Micros (new - old)

instance Duration UnixDateTimePicos Micros where
    duration (UnixDateTimePicos  old _) (UnixDateTimePicos  new _) = Micros (new - old)

instance Duration UnixDateTimeNanos Nanos where
    duration old new =
      if res < toInteger (maxBound::Int64) then Nanos $ fromInteger res
      else error "duration{UnixDateTimeNanos,Nanos}: integer overflow"
      where res = fromNano new - fromNano old

instance Duration UnixDateTimePicos Nanos where
    duration old new =
      if res < toInteger (maxBound::Int64) then Nanos $ fromInteger res `div` 1000
      else error "duration{UnixDateTimePicos,Nanos}: integer overflow"
      where res = fromPico new - fromPico old

instance Duration UnixDateTimePicos Picos where
    duration old new =
      if res < toInteger (maxBound::Int64) then Picos $ fromInteger res
      else error "duration{UnixDateTimePicos,Picos}: integer overflow"
      where res = fromPico new - fromPico old

instance Random UnixDate where
    random        = first toEnum . randomR (0,000000000000000002932896)
    randomR (a,b) = first toEnum . randomR (fromEnum a, fromEnum b)

instance Random UnixDateTime where
    random        = first toEnum . randomR (0,000000000000253402300799)
    randomR (a,b) = first toEnum . randomR (fromEnum a, fromEnum b)

instance Random UnixDateTimeMillis where
    random        = first toEnum . randomR (0,000000000253402300799999)
    randomR (a,b) = first toEnum . randomR (fromEnum a, fromEnum b)

instance Random UnixDateTimeMicros where
    random        = first toEnum . randomR (0,000000253402300799999999)
    randomR (a,b) = first toEnum . randomR (fromEnum a, fromEnum b)

instance Random UnixDateTimeNanos where
    random        = first toNano . randomR (0,000253402300799999999999)
    randomR (a,b) = first toNano . randomR (fromNano a, fromNano b)

instance Random UnixDateTimePicos where
    random        = first toPico . randomR (0,253402300799999999999999)
    randomR (a,b) = first toPico . randomR (fromPico a, fromPico b)

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

-- | A convenient synonym.
both :: forall a b c d. (a -> b) -> (c -> d) -> (a, c) -> (b, d)
both = (***)
