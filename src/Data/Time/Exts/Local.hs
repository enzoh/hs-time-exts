---------------------------------------------------------------
-- Copyright (c) 2014, Enzo Haussecker. All rights reserved. --
---------------------------------------------------------------

{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TupleSections              #-}
{-# OPTIONS -Wall                       #-}

-- | Local timestamps of varying granularity.
module Data.Time.Exts.Local (

 -- ** Local Class
       Local(..)

 -- ** Local Timestamps
     , LocalDate(..)
     , LocalDateTime(..)
     , LocalDateTimeMillis(..)
     , LocalDateTimeMicros(..)
     , LocalDateTimeNanos(..)
     , LocalDateTimePicos(..)

 -- ** Create Local Timestamps
     , createLocalDate
     , createLocalDateTime
     , createLocalDateTimeMillis
     , createLocalDateTimeMicros
     , createLocalDateTimeNanos
     , createLocalDateTimePicos

 -- ** Get Current Local Timestamps
     , getCurrentLocalDate
     , getCurrentLocalDateTime
     , getCurrentLocalDateTimeMillis
     , getCurrentLocalDateTimeMicros
     , getCurrentLocalDateTimeNanos
     , getCurrentLocalDateTimePicos

 -- ** Time Zone Transition Times
     , Transition(..)
     , getTransitions
     , lastTransition

 -- ** Get Current Local Timestamps Using Preloaded Time Zone Transitions Times
     , getCurrentLocalDate'
     , getCurrentLocalDateTime'
     , getCurrentLocalDateTimeMillis'
     , getCurrentLocalDateTimeMicros'
     , getCurrentLocalDateTimeNanos'
     , getCurrentLocalDateTimePicos'

 -- ** Pretty Local Timestamps
     , prettyLocalDate
     , prettyLocalDateTime

 -- ** Base Conversions
     , baseUnixToUTC
     , baseUTCToUnix

     ) where

import Control.Arrow ((***), first)
import Control.DeepSeq (NFData)
import Data.Aeson (FromJSON, ToJSON)
import Data.Convertible (Convertible(..), convert)
import Data.Function (on)
import Data.Int (Int16, Int32, Int64)
import Data.Label (get, mkLabels, modify, set)
import Data.List (groupBy, sortBy)
import Data.Maybe (listToMaybe)
import Data.Monoid ((<>))
import Data.Ord (comparing)
import Data.Time (DiffTime, UTCTime(..))
import qualified Data.Time.Calendar as Cal (Day(..))
import Data.Time.Exts.Base
import Data.Time.Exts.Unix
import Data.Time.Exts.Zone
import qualified Data.Time.LocalTime.TimeZone.Olson as Olson
import Data.Tuple (swap)
import Data.Typeable (Typeable)
import Data.Word (Word8)
import Foreign.Ptr (plusPtr)
import Foreign.Storable (Storable(..))
import GHC.Generics (Generic)
import System.Random (Random(..))
import Text.Printf (printf)

-- | The local timestamp type class.
class Local u where

    -- | Get the base component of a local timestamp.
    localBase :: u -> Int64

    -- | Get the zone component of a local timestamp.
    localZone :: u -> Word8

-- | Days since Unix epoch.
data LocalDate = LocalDate {
    _loc_day_base :: {-# UNPACK #-} !Int32
  , _loc_day_zone :: {-# UNPACK #-} !Word8
  } deriving (Eq,Generic,Typeable)

-- | Seconds since Unix epoch (including leap seconds).
data LocalDateTime = LocalDateTime {
    _loc_sec_base :: {-# UNPACK #-} !Int64
  , _loc_sec_zone :: {-# UNPACK #-} !Word8
  } deriving (Eq,Generic,Typeable)

-- | Milliseconds since Unix epoch (including leap seconds).
data LocalDateTimeMillis = LocalDateTimeMillis {
    _loc_mil_base :: {-# UNPACK #-} !Int64
  , _loc_mil_zone :: {-# UNPACK #-} !Word8
  } deriving (Eq,Generic,Typeable)

-- | Microseconds since Unix epoch (including leap seconds).
data LocalDateTimeMicros = LocalDateTimeMicros {
    _loc_mic_base :: {-# UNPACK #-} !Int64
  , _loc_mic_zone :: {-# UNPACK #-} !Word8
  } deriving (Eq,Generic,Typeable)

-- | Nanoseconds since Unix epoch (including leap seconds).
data LocalDateTimeNanos = LocalDateTimeNanos {
    _loc_nan_base :: {-# UNPACK #-} !Int64
  , _loc_nan_nano :: {-# UNPACK #-} !Int16
  , _loc_nan_zone :: {-# UNPACK #-} !Word8
  } deriving (Eq,Generic,Typeable)

-- | Picoseconds since Unix epoch (including leap seconds).
data LocalDateTimePicos = LocalDateTimePicos {
    _loc_pic_base :: {-# UNPACK #-} !Int64
  , _loc_pic_pico :: {-# UNPACK #-} !Int32
  , _loc_pic_zone :: {-# UNPACK #-} !Word8
  } deriving (Eq,Generic,Typeable)

-- | Time zone transition time.
newtype Transition = Transition {
    unboxTrans :: LocalDateTime
  } deriving (Eq,Typeable,Generic,ToJSON,FromJSON,NFData,Storable)

instance FromJSON LocalDate
instance FromJSON LocalDateTime
instance FromJSON LocalDateTimeMillis
instance FromJSON LocalDateTimeMicros
instance FromJSON LocalDateTimeNanos
instance FromJSON LocalDateTimePicos

instance NFData LocalDate
instance NFData LocalDateTime
instance NFData LocalDateTimeMillis
instance NFData LocalDateTimeMicros
instance NFData LocalDateTimeNanos
instance NFData LocalDateTimePicos

instance Storable LocalDate where
    sizeOf  _ = 5
    alignment = sizeOf
    peekElemOff ptr n = do
      let off = 5 * n
      base <- peek . plusPtr ptr $ off
      zone <- peek . plusPtr ptr $ off + 4
      return $! LocalDate base zone
    pokeElemOff ptr n LocalDate{..} = do
      let off = 5 * n
      poke (plusPtr ptr $ off    ) _loc_day_base
      poke (plusPtr ptr $ off + 4) _loc_day_zone

instance Storable LocalDateTime where
    sizeOf  _ = 9
    alignment = sizeOf
    peekElemOff ptr n = do
      let off = 9 * n
      base <- peek . plusPtr ptr $ off
      zone <- peek . plusPtr ptr $ off + 8
      return $! LocalDateTime base zone
    pokeElemOff ptr n LocalDateTime{..} = do
      let off = 9 * n
      poke (plusPtr ptr $ off    ) _loc_sec_base
      poke (plusPtr ptr $ off + 8) _loc_sec_zone

instance Storable LocalDateTimeMillis where
    sizeOf  _ = 9
    alignment = sizeOf
    peekElemOff ptr n = do
      let off = 9 * n
      base <- peek . plusPtr ptr $ off
      zone <- peek . plusPtr ptr $ off + 8
      return $! LocalDateTimeMillis base zone
    pokeElemOff ptr n LocalDateTimeMillis{..} = do
      let off = 9 * n
      poke (plusPtr ptr $ off    ) _loc_mil_base
      poke (plusPtr ptr $ off + 8) _loc_mil_zone

instance Storable LocalDateTimeMicros where
    sizeOf  _ = 9
    alignment = sizeOf
    peekElemOff ptr n = do
      let off = 9 * n
      base <- peek . plusPtr ptr $ off
      zone <- peek . plusPtr ptr $ off + 8
      return $! LocalDateTimeMicros base zone
    pokeElemOff ptr n LocalDateTimeMicros{..} = do
      let off = 9 * n
      poke (plusPtr ptr $ off    ) _loc_mic_base
      poke (plusPtr ptr $ off + 8) _loc_mic_zone

instance Storable LocalDateTimeNanos where
    sizeOf  _ = 11
    alignment = sizeOf
    peekElemOff ptr  n = do
      let off = 11 * n
      base <- peek . plusPtr ptr $ off
      nano <- peek . plusPtr ptr $ off + 08
      zone <- peek . plusPtr ptr $ off + 10
      return $! LocalDateTimeNanos base nano zone
    pokeElemOff ptr  n LocalDateTimeNanos{..} = do
      let off = 11 * n
      poke (plusPtr ptr $ off     ) _loc_nan_base
      poke (plusPtr ptr $ off + 08) _loc_nan_nano
      poke (plusPtr ptr $ off + 10) _loc_nan_zone

instance Storable LocalDateTimePicos where
    sizeOf  _ = 13
    alignment = sizeOf
    peekElemOff ptr  n = do
      let off = 13 * n
      base <- peek . plusPtr ptr $ off
      pico <- peek . plusPtr ptr $ off + 08
      zone <- peek . plusPtr ptr $ off + 12
      return $! LocalDateTimePicos base pico zone
    pokeElemOff ptr  n LocalDateTimePicos{..} = do
      let off = 13 * n
      poke (plusPtr ptr $ off     ) _loc_pic_base
      poke (plusPtr ptr $ off + 08) _loc_pic_pico
      poke (plusPtr ptr $ off + 12) _loc_pic_zone

instance ToJSON LocalDate
instance ToJSON LocalDateTime
instance ToJSON LocalDateTimeMillis
instance ToJSON LocalDateTimeMicros
instance ToJSON LocalDateTimeNanos
instance ToJSON LocalDateTimePicos

mkLabels [ ''LocalDate
         , ''LocalDateTime
         , ''LocalDateTimeMillis
         , ''LocalDateTimeMicros
         , ''LocalDateTimeNanos
         , ''LocalDateTimePicos
         ]

instance Bounded LocalDate where
    minBound = LocalDate 0 0
    maxBound = LocalDate 2932896 maxZone

instance Bounded LocalDateTime where
    minBound = LocalDateTime 43200 0
    maxBound = LocalDateTime 253402257624 maxZone

instance Bounded LocalDateTimeMillis where
    minBound = LocalDateTimeMillis 43200000 0
    maxBound = LocalDateTimeMillis 253402257624999 maxZone

instance Bounded LocalDateTimeMicros where
    minBound = LocalDateTimeMicros 43200000000 0
    maxBound = LocalDateTimeMicros 253402257624999999 maxZone

instance Bounded LocalDateTimeNanos where
    minBound = LocalDateTimeNanos 43200000000 0 0
    maxBound = LocalDateTimeNanos 253402257624999999 999 maxZone

instance Bounded LocalDateTimePicos where
    minBound = LocalDateTimePicos 43200000000 0 0
    maxBound = LocalDateTimePicos 253402257624999999 999999 maxZone

deriving instance Bounded Transition

instance Local LocalDate where
    localBase = fromIntegral . get loc_day_base
    localZone = get loc_day_zone

instance Local LocalDateTime where
    localBase = get loc_sec_base
    localZone = get loc_sec_zone

instance Local LocalDateTimeMillis where
    localBase = get loc_mil_base
    localZone = get loc_mil_zone

instance Local LocalDateTimeMicros where
    localBase = get loc_mic_base
    localZone = get loc_mic_zone

instance Local LocalDateTimeNanos where
    localBase = get loc_nan_base
    localZone = get loc_nan_zone

instance Local LocalDateTimePicos where
    localBase = get loc_pic_base
    localZone = get loc_pic_zone

deriving instance Local Transition

instance Ord LocalDate where
    compare = comparing localBase

instance Ord LocalDateTime where
    compare = comparing localBase

instance Ord LocalDateTimeMillis where
    compare = comparing localBase

instance Ord LocalDateTimeMicros where
    compare = comparing localBase

instance Ord LocalDateTimeNanos where
    compare = comparing localBase <> comparing (get loc_nan_nano)

instance Ord LocalDateTimePicos where
    compare = comparing localBase <> comparing (get loc_pic_pico)

deriving instance Ord Transition

instance DateTimeMath LocalDate Day where
    date `plus` Day day =
      check "plus{LocalDate,Day}" $
        modify loc_day_base (+ day) date

instance DateTimeMath LocalDateTime Second where
    time `plus` Second second =
      check "plus{LocalDateTime,Second}" $
        modify loc_sec_base (+ second) time

instance DateTimeMath LocalDateTimeMillis Second where
    time `plus` Second second =
      check "plus{LocalDateTimeMillis,Second}" $
        modify loc_mil_base (+ second * 1000) time

instance DateTimeMath LocalDateTimeMicros Second where
    time `plus` Second second =
      check "plus{LocalDateTimeMicros,Second}" $
        modify loc_mic_base (+ second * 1000000) time

instance DateTimeMath LocalDateTimeNanos Second where
    time `plus` Second second =
      check "plus{LocalDateTimeNanos,Second}" $
        modify loc_nan_base (+ second * 1000000) time

instance DateTimeMath LocalDateTimePicos Second where
    time `plus` Second second =
      check "plus{LocalDateTimePicos,Second}" $
        modify loc_pic_base (+ second * 1000000) time

instance DateTimeMath LocalDateTimeMillis Millis where
    time `plus` Millis millis =
      check "plus{LocalDateTimeMillis,Millis}" $
        modify loc_mil_base (+ millis) time

instance DateTimeMath LocalDateTimeMicros Millis where
    time `plus` Millis millis =
      check "plus{LocalDateTimeMicros,Millis}" $
        modify loc_mic_base (+ millis * 1000) time

instance DateTimeMath LocalDateTimeNanos Millis where
    time `plus` Millis millis =
      check "plus{LocalDateTimeNanos,Millis}" $
        modify loc_nan_base (+ millis * 1000) time

instance DateTimeMath LocalDateTimePicos Millis where
    time `plus` Millis millis =
      check "plus{LocalDateTimePicos,Millis}" $
        modify loc_pic_base (+ millis * 1000) time

instance DateTimeMath LocalDateTimeMicros Micros where
    time `plus` Micros micros =
      check "plus{LocalDateTimeMicros,Micros}" $
        modify loc_mic_base (+ micros) time

instance DateTimeMath LocalDateTimeNanos Micros where
    time `plus` Micros micros =
      check "plus{LocalDateTimeNanos,Micros}" $
        modify loc_nan_base (+ micros) time

instance DateTimeMath LocalDateTimePicos Micros where
    time `plus` Micros micros =
      check "plus{LocalDateTimePicos,Micros}" $
        modify loc_pic_base (+ micros) time

instance DateTimeMath LocalDateTimeNanos Nanos where
    time `plus` Nanos nanos =
      check "plus{UnixDateTimeNanos,Nanos}" .
        modify loc_nan_base (+ micros) $
           set loc_nan_nano nano time
           where nsum = fromIntegral (get loc_nan_nano time) + nanos
                 (micros, nano) = fmap fromIntegral $ divMod nsum 1000

instance DateTimeMath LocalDateTimePicos Nanos where
    time `plus` Nanos nanos =
      check "plus{LocalDateTimePicos,Nanos}" .
        modify loc_pic_base (+ micros) $
           set loc_pic_pico pico time
           where psum = fromIntegral (get loc_pic_pico time) + nanos * 1000
                 (micros, pico) = fmap fromIntegral $ divMod psum 1000000

instance DateTimeMath LocalDateTimePicos Picos where
    time `plus` Picos picos =
      check "plus{LocalDateTimePicos,Picos}" .
        modify loc_pic_base (+ micros) $
           set loc_pic_pico pico time
           where psum = fromIntegral (get loc_pic_pico time) + picos
                 (micros, pico) = fmap fromIntegral $ divMod psum 1000000

instance Enum LocalDate where
    succ = flip plus $ Day 1
    pred = flip plus . Day $ - 1
    toEnum = check "toEnum{LocalDate}" . uncurry LocalDate . doubleIntegral . flip divMod 1000
    fromEnum LocalDate{..} = fromIntegral _loc_day_base * 1000 + fromIntegral _loc_day_zone
    enumFrom v = [t | t <- v : enumFrom (succ v)]
    enumFromTo v1 v2 
      | v1 == v2  = [v1]
      | v1  < v2  = [t | t <- v1 : enumFromTo (succ v1) v2]
      | otherwise = [t | t <- v1 : enumFromTo (pred v1) v2]

instance Enum LocalDateTime where
    succ = flip plus $ Second 1
    pred = flip plus . Second $ - 1
    toEnum = check "toEnum{LocalDateTime}" . uncurry LocalDateTime . doubleIntegral . flip divMod 1000
    fromEnum LocalDateTime{..} = fromIntegral _loc_sec_base * 1000 + fromIntegral _loc_sec_zone
    enumFrom v = [t | t <- v : enumFrom (succ v)]
    enumFromTo v1 v2 
      | v1 == v2  = [v1]
      | v1  < v2  = [t | t <- v1 : enumFromTo (succ v1) v2]
      | otherwise = [t | t <- v1 : enumFromTo (pred v1) v2]

instance Enum LocalDateTimeMillis where
    succ = flip plus $ Millis 1
    pred = flip plus . Millis $ - 1
    toEnum = check "toEnum{LocalDateTimeMillis}" . uncurry LocalDateTimeMillis . doubleIntegral . flip divMod 1000
    fromEnum LocalDateTimeMillis{..} = fromIntegral _loc_mil_base * 1000 + fromIntegral _loc_mil_zone
    enumFrom v = [t | t <- v : enumFrom (succ v)]
    enumFromTo v1 v2 
      | v1 == v2  = [v1]
      | v1  < v2  = [t | t <- v1 : enumFromTo (succ v1) v2]
      | otherwise = [t | t <- v1 : enumFromTo (pred v1) v2]

deriving instance Enum Transition

-- | Convert Unix seconds into a UTC seconds.
baseUnixToUTC :: Int64 -> Int64
baseUnixToUTC base
   | base >= 1341100800 = base + 25
   | base >= 1230768000 = base + 24
   | base >= 1136073600 = base + 23
   | base >= 0915148800 = base + 22
   | base >= 0867715200 = base + 21
   | base >= 0820454400 = base + 20
   | base >= 0773020800 = base + 19
   | base >= 0741484800 = base + 18
   | base >= 0709948800 = base + 17
   | base >= 0662688000 = base + 16
   | base >= 0631152000 = base + 15
   | base >= 0567993600 = base + 14
   | base >= 0489024000 = base + 13
   | base >= 0425865600 = base + 12
   | base >= 0394329600 = base + 11
   | base >= 0362793600 = base + 10
   | base >= 0315532800 = base + 09
   | base >= 0283996800 = base + 08
   | base >= 0252460800 = base + 07
   | base >= 0220924800 = base + 06
   | base >= 0189302400 = base + 05
   | base >= 0157766400 = base + 04
   | base >= 0126230400 = base + 03
   | base >= 0094694400 = base + 02
   | base >= 0078796800 = base + 01
   | otherwise          = base + 00

-- | Create a local date.
-- 
-- > >>> createLocalDate 2013 November 03 Pacific_Standard_Time
-- > 2013-11-03 PST
--
createLocalDate :: Year -> Month -> Day -> TimeZone -> LocalDate
createLocalDate year month day zone =
   check "createLocalDate" . LocalDate base $ fromZone zone
   where Day base = epochToDate year month day

-- | Create a local date and time.
--
-- > >>> createLocalDateTime 2013 November 03 22 55 52 South_Africa_Standard_Time
-- > 2013-11-03 22:55:52 SAST
--
createLocalDateTime :: Year -> Month -> Day -> Hour -> Minute -> Second -> TimeZone -> LocalDateTime
createLocalDateTime year month day hour minute (Second second) zone =
   check "createLocalDateTime" . LocalDateTime base $ fromZone zone
   where base = baseUnixToUTC (unix - offset) + second
         Second unix = epochToTime year month day hour minute 0
         offset = getUTCOffset zone * 60

-- | Create a local date and time with millisecond granularity.
--
-- > >>> createLocalDateTimeMillis 2013 November 03 13 57 43 830 Mountain_Standard_Time
-- > 2013-11-03 13:57:43.830 MST
--
createLocalDateTimeMillis :: Year -> Month -> Day -> Hour -> Minute -> Second -> Millis -> TimeZone -> LocalDateTimeMillis
createLocalDateTimeMillis year month day hour minute (Second second) (Millis millis) zone =
   check "createLocalDateTimeMillis" . LocalDateTimeMillis base $ fromZone zone
   where base = 1000 * (baseUnixToUTC (unix - offset) + second) + millis
         Second unix = epochToTime year month day hour minute 0
         offset = getUTCOffset zone * 60

-- | Create a local date and time with microsecond granularity.
--
-- > >>> createLocalDateTimeMicros 2013 November 03 21 01 42 903539 Coordinated_Universal_Time 
-- > 2013-11-03 21:01:42.903539 UTC
--
createLocalDateTimeMicros :: Year -> Month -> Day -> Hour -> Minute -> Second -> Micros -> TimeZone -> LocalDateTimeMicros
createLocalDateTimeMicros year month day hour minute (Second second) (Micros micros) zone =
   check "createLocalDateTimeMicros" . LocalDateTimeMicros base $ fromZone zone
   where base = 1000000 * (baseUnixToUTC (unix - offset) + second) + micros
         Second unix = epochToTime year month day hour minute 0
         offset = getUTCOffset zone * 60

-- | Create a local date and time with nanosecond granularity.
--
-- > >>> createLocalDateTimeNanos 2013 November 04 06 05 07 016715087 Japan_Standard_Time
-- > 2013-11-04 06:05:07.016715087 JST
--
createLocalDateTimeNanos :: Year -> Month -> Day -> Hour -> Minute -> Second -> Nanos -> TimeZone -> LocalDateTimeNanos
createLocalDateTimeNanos year month day hour minute (Second second) (Nanos nanos) zone =
   check "createLocalDateTimeNanos" . LocalDateTimeNanos base nano $ fromZone zone
   where base = 1000000 * (baseUnixToUTC (unix - offset) + second) + micros
         (micros, nano) = fmap fromIntegral $ divMod nanos 0001000
         Second unix = epochToTime year month day hour minute 0
         offset = getUTCOffset zone * 60

-- | Create a local date and time with picosecond granularity.
--
-- > >>> createLocalDateTimePicos 2013 November 03 23 13 56 838238648311 Eastern_European_Time
-- > 2013-11-03 23:13:56.838238648311 EET
--
createLocalDateTimePicos :: Year -> Month -> Day -> Hour -> Minute -> Second -> Picos -> TimeZone -> LocalDateTimePicos
createLocalDateTimePicos year month day hour minute (Second second) (Picos picos) zone =
   check "createLocalDateTimePicos" . LocalDateTimePicos base pico $ fromZone zone
   where base = 1000000 * (baseUnixToUTC (unix - offset) + second) + micros
         (micros, pico) = fmap fromIntegral $ divMod picos 1000000
         Second unix = epochToTime year month day hour minute 0
         offset = getUTCOffset zone * 60

-- | Convert UTC seconds into Unix and leap seconds.
baseUTCToUnix :: Int64 -> (Int64, Double)
baseUTCToUnix base
   | base >= 1341100825 = (base - 0025, 0)
   | base == 1341100824 = (01341100799, 1)
   | base >= 1230768024 = (base - 0024, 0)
   | base == 1230768023 = (01230767999, 1)
   | base >= 1136073623 = (base - 0023, 0)
   | base == 1136073622 = (01136073599, 1)
   | base >= 0915148822 = (base - 0022, 0)
   | base == 0915148821 = (00915148799, 1)
   | base >= 0867715221 = (base - 0021, 0)
   | base == 0867715220 = (00867715199, 1)
   | base >= 0820454420 = (base - 0020, 0)
   | base == 0820454419 = (00820454399, 1)
   | base >= 0773020819 = (base - 0019, 0)
   | base == 0773020818 = (00773020799, 1)
   | base >= 0741484818 = (base - 0018, 0)
   | base == 0741484817 = (00741484799, 1)
   | base >= 0709948817 = (base - 0017, 0)
   | base == 0709948816 = (00709948799, 1)
   | base >= 0662688016 = (base - 0016, 0)
   | base == 0662688015 = (00662687999, 1)
   | base >= 0631152015 = (base - 0015, 0)
   | base == 0631152014 = (00631151999, 1)
   | base >= 0567993614 = (base - 0014, 0)
   | base == 0567993613 = (00567993599, 1)
   | base >= 0489024013 = (base - 0013, 0)
   | base == 0489024012 = (00489023999, 1)
   | base >= 0425865612 = (base - 0012, 0)
   | base == 0425865611 = (00425865599, 1)
   | base >= 0394329611 = (base - 0011, 0)
   | base == 0394329610 = (00394329599, 1)
   | base >= 0362793610 = (base - 0010, 0)
   | base == 0362793609 = (00362793599, 1)
   | base >= 0315532809 = (base - 0009, 0)
   | base == 0315532808 = (00315532799, 1)
   | base >= 0283996808 = (base - 0008, 0)
   | base == 0283996807 = (00283996799, 1)
   | base >= 0252460807 = (base - 0007, 0)
   | base == 0252460806 = (00252460799, 1)
   | base >= 0220924806 = (base - 0006, 0)
   | base == 0220924805 = (00220924799, 1)
   | base >= 0189302405 = (base - 0005, 0)
   | base == 0189302404 = (00189302399, 1)
   | base >= 0157766404 = (base - 0004, 0)
   | base == 0157766403 = (00157766399, 1)
   | base >= 0126230403 = (base - 0003, 0)
   | base == 0126230402 = (00126230399, 1)
   | base >= 0094694402 = (base - 0002, 0)
   | base == 0094694401 = (00094694399, 1)
   | base >= 0078796801 = (base - 0001, 0)
   | base == 0078796800 = (00078796799, 1)
   | otherwise          = (base - 0000, 0)

-- | Decompose a local date into a human-readable format.
decompLocalDate :: LocalDate -> DateZoneStruct
decompLocalDate LocalDate{..} =
   DateZoneStruct _d_year _d_mon _d_mday _d_wday zone
   where DateStruct{..} = toDateStruct $ UnixDate _loc_day_base
         zone = toZone _loc_day_zone

-- | Decompose a local date and time into a human-readable format.
decompLocalDateTime :: LocalDateTime -> DateTimeZoneStruct
decompLocalDateTime LocalDateTime{..} =
   DateTimeZoneStruct _dt_year _dt_mon _dt_mday _dt_wday _dt_hour _dt_min sec zone
   where DateTimeStruct{..} = toDateTimeStruct $ UnixDateTime base `plus` offset
         (base, leap) = baseUTCToUnix _loc_sec_base
         sec = _dt_sec + leap
         offset = getUTCOffset zone :: Minute
         zone = toZone _loc_sec_zone

-- | Decompose a local date and time with millisecond granularity into a human-readable format.
decompLocalDateTimeMillis :: LocalDateTimeMillis -> DateTimeZoneStruct
decompLocalDateTimeMillis LocalDateTimeMillis{..} =
   DateTimeZoneStruct _dt_year _dt_mon _dt_mday _dt_wday _dt_hour _dt_min sec zone
   where DateTimeStruct{..} = toDateTimeStruct $ UnixDateTime base `plus` offset
         ((base, leap), millis) = baseUTCToUnix *** realToFrac $ divMod _loc_mil_base 0001000
         sec = _dt_sec + leap + millis / 0001000
         offset = getUTCOffset zone :: Minute
         zone = toZone _loc_mil_zone

-- | Decompose a local date and time with microsecond granularity into a human-readable format.
decompLocalDateTimeMicros :: LocalDateTimeMicros -> DateTimeZoneStruct
decompLocalDateTimeMicros LocalDateTimeMicros{..} =
   DateTimeZoneStruct _dt_year _dt_mon _dt_mday _dt_wday _dt_hour _dt_min sec zone
   where DateTimeStruct{..} = toDateTimeStruct $ UnixDateTime base `plus` offset
         ((base, leap), micros) = baseUTCToUnix *** realToFrac $ divMod _loc_mic_base 1000000
         sec = _dt_sec + leap + micros / 1000000
         offset = getUTCOffset zone :: Minute
         zone = toZone _loc_mic_zone

-- | Decompose a local date and time with nanosecond granularity into a human-readable format.
decompLocalDateTimeNanos :: LocalDateTimeNanos -> DateTimeZoneStruct
decompLocalDateTimeNanos LocalDateTimeNanos{..} =
   DateTimeZoneStruct _dt_year _dt_mon _dt_mday _dt_wday _dt_hour _dt_min sec zone
   where DateTimeStruct{..} = toDateTimeStruct $ UnixDateTime base `plus` offset
         ((base, leap), micros) = baseUTCToUnix *** realToFrac $ divMod _loc_nan_base 1000000
         sec = _dt_sec + leap + micros / 1000000 + realToFrac _loc_nan_nano / 1000000000
         offset = getUTCOffset zone :: Minute
         zone = toZone _loc_nan_zone

-- | Decompose a local date and time with picosecond granularity into a human-readable format.
decompLocalDateTimePicos :: LocalDateTimePicos -> DateTimeZoneStruct
decompLocalDateTimePicos LocalDateTimePicos{..} =
   DateTimeZoneStruct _dt_year _dt_mon _dt_mday _dt_wday _dt_hour _dt_min sec zone
   where DateTimeStruct{..} = toDateTimeStruct $ UnixDateTime base `plus` offset
         ((base, leap), micros) = baseUTCToUnix *** realToFrac $ divMod _loc_pic_base 1000000
         sec = _dt_sec + leap + micros / 1000000 + realToFrac _loc_pic_pico / 1000000000000
         offset = getUTCOffset zone :: Minute
         zone = toZone _loc_pic_zone

-- | Decompose a local base (in seconds) into day and second components.
decompLocalBase :: (Int64, TimeZone) -> (Int32, DiffTime)
decompLocalBase = uncurry (flip f) . first (uncurry g . baseUTCToUnix)
   where f x = first $ fromIntegral . flip div 86400  . (+ 60 * getUTCOffset x)
         g x = (x, ) . fromIntegral . (+ mod x 86400) . truncate

instance Convertible LocalDateTime LocalDate where
    safeConvert LocalDateTime{..} = Right $ LocalDate base _loc_sec_zone
      where base = fst $ decompLocalBase (_loc_sec_base, toZone _loc_sec_zone)

instance Convertible LocalDateTimeMillis LocalDate where
    safeConvert LocalDateTimeMillis{..} = Right $ LocalDate base _loc_mil_zone
      where base = fst (decompLocalBase (div _loc_mil_base 0001000, toZone _loc_mil_zone))

instance Convertible LocalDateTimeMicros LocalDate where
    safeConvert LocalDateTimeMicros{..} = Right $ LocalDate base _loc_mic_zone
      where base = fst (decompLocalBase (div _loc_mic_base 1000000, toZone _loc_mic_zone))

instance Convertible LocalDateTimeNanos LocalDate where
    safeConvert LocalDateTimeNanos{..} = Right $ LocalDate base _loc_nan_zone
      where base = fst (decompLocalBase (div _loc_nan_base 1000000, toZone _loc_nan_zone))

instance Convertible LocalDateTimePicos LocalDate where
    safeConvert LocalDateTimePicos{..} = Right $ LocalDate base _loc_pic_zone
      where base = fst (decompLocalBase (div _loc_pic_base 1000000, toZone _loc_pic_zone))

instance Convertible LocalDate Cal.Day where
    safeConvert = Right . Cal.ModifiedJulianDay . toInteger . (+ 40587) . _loc_day_base

instance Convertible LocalDateTime UTCTime where
    safeConvert LocalDateTime{..} = Right $ UTCTime julian pico
      where julian = Cal.ModifiedJulianDay $ toInteger day + 40587
            (day, pico) = decompLocalBase (_loc_sec_base, toZone _loc_sec_zone)

instance Convertible LocalDateTimeMillis UTCTime where
    safeConvert LocalDateTimeMillis{..} = Right $ UTCTime julian pico
      where julian = Cal.ModifiedJulianDay $ toInteger day + 40587
            frac = millis / 1000
            (base, millis) = fmap fromIntegral $ _loc_mil_base `divMod` 0001000
            (day , pico  ) = fmap (+ frac) $ decompLocalBase (base, toZone _loc_mil_zone)

instance Convertible LocalDateTimeMicros UTCTime where
    safeConvert LocalDateTimeMicros{..} = Right $ UTCTime julian pico
      where julian = Cal.ModifiedJulianDay $ toInteger day + 40587
            frac = micros / 1000000
            (base, micros) = fmap fromIntegral $ _loc_mic_base `divMod` 1000000
            (day , pico  ) = fmap (+ frac) $ decompLocalBase (base, toZone _loc_mic_zone)

instance Convertible LocalDateTimeNanos UTCTime where
    safeConvert LocalDateTimeNanos{..} = Right $ UTCTime julian pico
      where julian = Cal.ModifiedJulianDay $ toInteger day + 40587
            frac = micros / 1000000 + fromIntegral _loc_nan_nano / 0001000000000
            (base, micros) = fmap fromIntegral $ _loc_nan_base `divMod` 1000000
            (day , pico  ) = fmap (+ frac) $ decompLocalBase (base, toZone _loc_nan_zone)

instance Convertible LocalDateTimePicos UTCTime where
    safeConvert LocalDateTimePicos{..} = Right $ UTCTime julian pico
      where julian = Cal.ModifiedJulianDay $ toInteger day + 40587
            frac = micros / 1000000 + fromIntegral _loc_pic_pico / 1000000000000
            (base, micros) = fmap fromIntegral $ _loc_pic_base `divMod` 1000000
            (day , pico  ) = fmap (+ frac) $ decompLocalBase (base, toZone _loc_pic_zone)

instance Convertible Cal.Day LocalDate where
    safeConvert = Right . check "safeConvert{Data.Time.Calendar.Day,LocalDate}" .
      flip LocalDate (fromZone utc) . fromInteger . (subtract 40587) . Cal.toModifiedJulianDay

instance Convertible UTCTime LocalDateTime where
    safeConvert UTCTime{..} = Right . check "safeConvert{Data.Time.Clock.UTCTime,LocalDateTime}" $
      LocalDateTime base (fromZone utc)
      where base = baseUnixToUTC $ day * 86400 + truncate utctDayTime
            day  = fromInteger (Cal.toModifiedJulianDay utctDay) - 40587

instance Convertible UTCTime LocalDateTimeMillis where
    safeConvert UTCTime{..} = Right . check "safeConvert{Data.Time.Clock.UTCTime,LocalDateTimeMillis}" $
      LocalDateTimeMillis base (fromZone utc)
      where base = baseUnixToUTC (day * 86400 + sec) * 0001000 + millis
            day  = fromInteger (Cal.toModifiedJulianDay utctDay) - 40587
            (sec , millis) = fmap (truncate . (* 0000001000)) $ properFraction utctDayTime

instance Convertible UTCTime LocalDateTimeMicros where
    safeConvert UTCTime{..} = Right . check "safeConvert{Data.Time.Clock.UTCTime,LocalDateTimeMicros}" $
      LocalDateTimeMicros base (fromZone utc)
      where base = baseUnixToUTC (day * 86400 + sec) * 1000000 + micros
            day  = fromInteger (Cal.toModifiedJulianDay utctDay) - 40587
            (sec , micros) = fmap (truncate . (* 0001000000)) $ properFraction utctDayTime

instance Convertible UTCTime LocalDateTimeNanos where
    safeConvert UTCTime{..} = Right . check "safeConvert{Data.Time.Clock.UTCTime,LocalDateTimeNanos}"  $
      LocalDateTimeNanos base nano (fromZone utc)
      where base = baseUnixToUTC (day * 86400 + sec) * 1000000 + micros
            day  = fromInteger (Cal.toModifiedJulianDay utctDay) - 40587
            (sec , nanos ) = fmap (truncate . (* 1000000000)) $ properFraction utctDayTime
            (nano, micros) = swap . fmap fromIntegral $ divMod nanos 1000

instance Convertible UTCTime LocalDateTimePicos where
    safeConvert UTCTime{..} = Right . check "safeConvert{Data.Time.Clock.UTCTime,LocalDateTimePicos}"  $
      LocalDateTimePicos base pico (fromZone utc)
      where base = baseUnixToUTC (day * 86400 + sec) * 1000000 + micros
            day  = fromInteger (Cal.toModifiedJulianDay utctDay) - 40587
            (sec , picos ) = fmap (truncate . (* 1000000000000)) $ properFraction utctDayTime
            (pico, micros) = swap . fmap fromIntegral $ divMod picos 1000000

instance DateZone LocalDate where
    toDateZoneStruct = decompLocalDate
    fromDateZoneStruct DateZoneStruct{..} =
      createLocalDate _dz_year _dz_mon _dz_mday _dz_zone

instance DateZone LocalDateTime where
    toDateZoneStruct = decompLocalDate . convert
    fromDateZoneStruct DateZoneStruct{..} =
      createLocalDateTime _dz_year _dz_mon _dz_mday 0 0 0 _dz_zone

instance DateZone LocalDateTimeMillis where
    toDateZoneStruct = decompLocalDate . convert
    fromDateZoneStruct DateZoneStruct{..} =
      createLocalDateTimeMillis _dz_year _dz_mon _dz_mday 0 0 0 0 _dz_zone

instance DateZone LocalDateTimeMicros where
    toDateZoneStruct = decompLocalDate . convert
    fromDateZoneStruct DateZoneStruct{..} =
      createLocalDateTimeMicros _dz_year _dz_mon _dz_mday 0 0 0 0 _dz_zone

instance DateZone LocalDateTimeNanos where
    toDateZoneStruct = decompLocalDate . convert
    fromDateZoneStruct DateZoneStruct{..} =
      createLocalDateTimeNanos _dz_year _dz_mon _dz_mday 0 0 0 0 _dz_zone

instance DateZone LocalDateTimePicos where
    toDateZoneStruct = decompLocalDate . convert
    fromDateZoneStruct DateZoneStruct{..} =
      createLocalDateTimePicos _dz_year _dz_mon _dz_mday 0 0 0 0 _dz_zone

deriving instance DateZone Transition

instance DateTimeZone LocalDateTime where
    toDateTimeZoneStruct = decompLocalDateTime
    fromDateTimeZoneStruct DateTimeZoneStruct{..} =
      createLocalDateTime _dtz_year _dtz_mon _dtz_mday _dtz_hour _dtz_min sec _dtz_zone
      where sec = round _dtz_sec :: Second

instance DateTimeZone LocalDateTimeMillis where
    toDateTimeZoneStruct = decompLocalDateTimeMillis
    fromDateTimeZoneStruct DateTimeZoneStruct{..} =
      createLocalDateTimeMillis _dtz_year _dtz_mon _dtz_mday _dtz_hour _dtz_min sec mil _dtz_zone
      where (sec, mil) = properFracMillis _dtz_sec

instance DateTimeZone LocalDateTimeMicros where
    toDateTimeZoneStruct = decompLocalDateTimeMicros
    fromDateTimeZoneStruct DateTimeZoneStruct{..} =
      createLocalDateTimeMicros _dtz_year _dtz_mon _dtz_mday _dtz_hour _dtz_min sec mic _dtz_zone
      where (sec, mic) = properFracMicros _dtz_sec

instance DateTimeZone LocalDateTimeNanos where
    toDateTimeZoneStruct = decompLocalDateTimeNanos
    fromDateTimeZoneStruct DateTimeZoneStruct{..} =
      createLocalDateTimeNanos _dtz_year _dtz_mon _dtz_mday _dtz_hour _dtz_min sec nan _dtz_zone
      where (sec, nan) = properFracNanos _dtz_sec

instance DateTimeZone LocalDateTimePicos where
    toDateTimeZoneStruct = decompLocalDateTimePicos
    fromDateTimeZoneStruct DateTimeZoneStruct{..} =
      createLocalDateTimePicos _dtz_year _dtz_mon _dtz_mday _dtz_hour _dtz_min sec pic _dtz_zone
      where (sec, pic) = properFracPicos _dtz_sec

deriving instance DateTimeZone Transition

instance Show LocalDate where
    show date = printf str _dz_year mon _dz_mday abbr
      where DateZoneStruct{..} = toDateZoneStruct date
            str  = "%04d-%02d-%02d %s"
            mon  = fromEnum _dz_mon + 1
            abbr = show $ abbreviate _dz_zone

instance Show LocalDateTime where
    show time = printf str _dtz_year mon _dtz_mday _dtz_hour _dtz_min sec abbr
      where DateTimeZoneStruct{..} = toDateTimeZoneStruct time
            str  = "%04d-%02d-%02d %02d:%02d:%02d %s"
            abbr = show $ abbreviate _dtz_zone
            mon  = fromEnum _dtz_mon + 1
            sec  = round _dtz_sec :: Second

instance Show LocalDateTimeMillis where
    show time = printf str _dtz_year mon _dtz_mday _dtz_hour _dtz_min sec mil abbr
      where DateTimeZoneStruct{..} = toDateTimeZoneStruct time
            str  = "%04d-%02d-%02d %02d:%02d:%02d.%03d %s"
            abbr = show $ abbreviate _dtz_zone
            mon  = fromEnum _dtz_mon + 1
            (sec, mil) = properFracMillis _dtz_sec

instance Show LocalDateTimeMicros where
    show time = printf str _dtz_year mon _dtz_mday _dtz_hour _dtz_min sec mic abbr
      where DateTimeZoneStruct{..} = toDateTimeZoneStruct time
            str  = "%04d-%02d-%02d %02d:%02d:%02d.%06d %s"
            abbr = show $ abbreviate _dtz_zone
            mon  = fromEnum _dtz_mon + 1
            (sec, mic) = properFracMicros _dtz_sec

instance Show LocalDateTimeNanos where
    show time = printf str _dtz_year mon _dtz_mday _dtz_hour _dtz_min sec nan abbr
      where DateTimeZoneStruct{..} = toDateTimeZoneStruct time
            str  = "%04d-%02d-%02d %02d:%02d:%02d.%09d %s"
            abbr = show $ abbreviate _dtz_zone
            mon  = fromEnum _dtz_mon + 1
            (sec, nan) = properFracNanos _dtz_sec

instance Show LocalDateTimePicos where
    show time = printf str _dtz_year mon _dtz_mday _dtz_hour _dtz_min sec pic abbr
      where DateTimeZoneStruct{..} = toDateTimeZoneStruct time
            str  = "%04d-%02d-%02d %02d:%02d:%02d.%012d %s"
            abbr = show $ abbreviate _dtz_zone
            mon  = fromEnum _dtz_mon + 1
            (sec, pic) = properFracPicos _dtz_sec

instance Show Transition where
    show = show . unboxTrans

-- | The next leap second insertion date.
nextLeap :: Maybe UnixDate
nextLeap = Nothing

-- | Get a list of time zone transition times for the given city.
getTransitions :: City -> IO [Transition]
getTransitions city = do
   let file = getOlsonFile city
   Olson.OlsonData{..} <- Olson.getOlsonFromFile file
   let ttimes = uniquetimes $ sortBy future2past olsonTransitions
   return $! foldr (step olsonTypes) [] $ map last ttimes
   where uniquetimes = groupBy $ on (==) Olson.transTime
         future2past = comparing $ negate . Olson.transTime
         step types Olson.Transition{..} accum =
           if transTime < 0
           then [Transition (LocalDateTime 43200 zone)]
           else  Transition (LocalDateTime  base zone) : accum
           where Olson.TtInfo{..} = types !! transIndex
                 abbr = TimeZoneAbbr city tt_abbr
                 base = baseUnixToUTC $ fromIntegral transTime
                 zone = fromZone $ unabbreviate abbr

-- | Get the last time zone transition time for the given city and time.
lastTransition :: (DateTime dt, Unix dt) => City -> dt -> IO (Maybe Transition)
lastTransition city time = do
   ttimes <- getTransitions city
   return $! listToMaybe $ dropWhile f ttimes
   where base = baseUnixToUTC $ unixNorm time
         f tt = localBase tt > base

-- | Get the current local date from the system clock.
--
-- > >>> getCurrentLocalDate London 
-- > 2013-11-03 GMT
--
getCurrentLocalDate :: City -> IO LocalDate
getCurrentLocalDate city = getTransitions city >>= getCurrentLocalDateTime' >>= return . convert

-- | Get the current local date from the system clock using preloaded transition times.
--
-- > >>> ttimes <- getTransitions Tokyo 
-- > >>> getCurrentLocalDate' ttimes
-- > 2013-11-04 JST
--
--   Use this function if you need to get the current local date more than once. The
--   use of preloaded transition times will avoid unnecessary parsing of Olson files. 
getCurrentLocalDate' :: [Transition] -> IO LocalDate
getCurrentLocalDate' ttimes = getCurrentLocalDateTime' ttimes >>= return . convert

-- | Get the current local date and time from the system clock.
--
-- > >>> getCurrentLocalDateTime New_York 
-- > 2013-11-03 16:38:16 EST
--
getCurrentLocalDateTime :: City -> IO LocalDateTime
getCurrentLocalDateTime city = getTransitions city >>= getCurrentLocalDateTime'

-- | Get the current local date and time from the system clock using preloaded transition
--   times.
--
-- > >>> ttimes <- getTransitions Moscow
-- > >>> getCurrentLocalDateTime' ttimes
-- > 2013-11-04 01:41:50 MSK
--
--   Use this function if you need to get the current local date and time more than once.
--   The use of preloaded transition times will avoid unnecessary parsing of Olson files.
getCurrentLocalDateTime' :: [Transition] -> IO LocalDateTime
getCurrentLocalDateTime' ttimes = do
   time@UnixDateTime{..} <- getCurrentUnixDateTime
   let  base = baseUnixToUTC _uni_sec_base
        f tt = localBase tt > base
        mval = listToMaybe $ dropWhile f ttimes
        zone = maybe (fromZone utc) localZone mval
   if   maybe True (/= convert time) nextLeap
   then return $! LocalDateTime base zone
   else let leap = round (realToFrac (_uni_sec_base `mod` 86400) / 86400 :: Double)
        in  return $! LocalDateTime base zone `plus` Second leap

-- | Get the current local date and time with millisecond granularity from the system clock.
--
-- > >>> getCurrentLocalDateTimeMillis Auckland
-- > 2013-11-04 10:46:13.123 NZDT
--
getCurrentLocalDateTimeMillis :: City -> IO LocalDateTimeMillis
getCurrentLocalDateTimeMillis city = getTransitions city >>= getCurrentLocalDateTimeMillis'

-- | Get the current local date and time with millisecond granularity from the system clock
--   using preloaded transition times.
--
-- > >>> ttimes <- getTransitions Tehran
-- > >>> getCurrentLocalDateTimeMillis' ttimes
-- > 2013-11-04 01:20:49.435 IRST
--
--   Use this function if you need to get the current local date and time with millisecond
--   granularity more than once. The use of preloaded transition times will avoid unnecessary
--   parsing of Olson files.
getCurrentLocalDateTimeMillis' :: [Transition] -> IO LocalDateTimeMillis
getCurrentLocalDateTimeMillis' ttimes = do
   time@UnixDateTimeMillis{..} <- getCurrentUnixDateTimeMillis
   let  (seconds, millis) = first baseUnixToUTC $ _uni_mil_base `divMod` 1000
        f tt = localBase tt > seconds
        mval = listToMaybe $ dropWhile f ttimes
        zone = maybe (fromZone utc) localZone mval
        base = seconds * 1000 + millis
   if   maybe True (/= convert time) nextLeap
   then return $! LocalDateTimeMillis base zone
   else let leap = round (realToFrac (_uni_mil_base `mod` 86400) / 86.4 :: Double)
        in  return $! LocalDateTimeMillis base zone `plus` Millis leap

-- | Get the current local date and time with microsecond granularity from the system clock.
--
-- > >>> getCurrentLocalDateTimeMicros Tel_Aviv 
-- > 2013-11-03 23:55:30.935387 IST
--
getCurrentLocalDateTimeMicros :: City -> IO LocalDateTimeMicros
getCurrentLocalDateTimeMicros city = getTransitions city >>= getCurrentLocalDateTimeMicros'

-- | Get the current local date and time with microsecond granularity from the system clock
--   using preloaded transition times.
--
-- > >>> ttimes <- getTransitions Sao_Paulo
-- > >>> getCurrentLocalDateTimeMicros' ttimes
-- > 2013-11-03 19:58:50.405806 BRST
--
--   Use this function if you need to get the current local date and time with microsecond
--   granularity more than once. The use of preloaded transition times will avoid unnecessary
--   parsing of Olson files.
getCurrentLocalDateTimeMicros' :: [Transition] -> IO LocalDateTimeMicros
getCurrentLocalDateTimeMicros' ttimes = do
   time@UnixDateTimeMicros{..} <- getCurrentUnixDateTimeMicros
   let  (seconds, micros) = first baseUnixToUTC $ _uni_mic_base `divMod` 1000000
        f tt = localBase tt > seconds
        mval = listToMaybe $ dropWhile f ttimes
        zone = maybe (fromZone utc) localZone mval
        base = seconds * 1000000 + micros
   if   maybe True (/= convert time) nextLeap
   then return $! LocalDateTimeMicros base zone
   else let leap = round (realToFrac (_uni_mic_base `mod` 86400) / 0.0864 :: Double)
        in  return $! LocalDateTimeMicros base zone `plus` Micros leap

-- | Get the current local date and time with nanosecond granularity from the system clock.
--
-- > >>> getCurrentLocalDateTimeNanos Brussels 
-- > 2013-11-03 23:01:07.337488000 CET
--
--   Note that this functions calls @gettimeofday@ behind the scenes. Therefore, the resultant
--   timestamp will have nanosecond granularity, but only microsecond resolution.
getCurrentLocalDateTimeNanos :: City -> IO LocalDateTimeNanos
getCurrentLocalDateTimeNanos city = getTransitions city >>= getCurrentLocalDateTimeNanos'

-- | Get the current local date and time with nanosecond granularity from the system clock
--   using preloaded transition times.
--
-- > >>> ttimes <- getTransitions Mogadishu
-- > >>> getCurrentLocalDateTimeNanos' ttimes
-- > 2013-11-04 01:15:08.664426000 EAT
--
--   Use this function if you need to get the current local date and time with nanosecond
--   granularity more than once. The use of preloaded transition times will avoid unnecessary
--   parsing of Olson files.
--
--   Note that this functions calls @gettimeofday@ behind the scenes. Therefore, the resultant
--   timestamp will have nanosecond granularity, but only microsecond resolution.
getCurrentLocalDateTimeNanos' :: [Transition] -> IO LocalDateTimeNanos
getCurrentLocalDateTimeNanos' ttimes = do
   time@UnixDateTimeNanos{..} <- getCurrentUnixDateTimeNanos
   let  (seconds, micros) = first baseUnixToUTC $ _uni_nan_base `divMod` 1000000
        f tt = localBase tt > seconds
        mval = listToMaybe $ dropWhile f ttimes
        zone = maybe (fromZone utc) localZone mval
        base = seconds * 1000000 + micros
   if   maybe True (/= convert time) nextLeap
   then return $! LocalDateTimeNanos base _uni_nan_nano zone
   else let leap = round (realToFrac (_uni_nan_base `mod` 86400) / 0.0000864 :: Double)
        in  return $! LocalDateTimeNanos base _uni_nan_nano zone `plus` Nanos leap

-- | Get the current local date and time with picosecond granularity from the system clock.
--
-- > >>> getCurrentLocalDateTimePicos Karachi
-- > 2013-11-04 22:05:17.556043000000 PKT
--
--   Note that this functions calls @gettimeofday@ behind the scenes. Therefore, the resultant
--   timestamp will have picosecond granularity, but only microsecond resolution.
getCurrentLocalDateTimePicos :: City -> IO LocalDateTimePicos
getCurrentLocalDateTimePicos city = getTransitions city >>= getCurrentLocalDateTimePicos'

-- | Get the current local date and time with picosecond granularity from the system clock using
--   preloaded transition times.
--
-- > >>> ttimes <- getTransitions Baghdad
-- > >>> getCurrentLocalDateTimePicos' ttimes
-- > 2013-11-04 01:20:57.502906000000 AST
--
--   Use this function if you need to get the current local date and time with picosecond
--   granularity more than once. The use of preloaded transition times will avoid unnecessary
--   parsing of Olson files.
--
--   Note that this functions calls @gettimeofday@ behind the scenes. Therefore, the resultant
--   timestamp will have picosecond granularity, but only microsecond resolution.
getCurrentLocalDateTimePicos' :: [Transition] -> IO LocalDateTimePicos
getCurrentLocalDateTimePicos' ttimes = do
   time@UnixDateTimePicos{..} <- getCurrentUnixDateTimePicos
   let  (seconds, micros) = first baseUnixToUTC $ _uni_pic_base `divMod` 1000000
        f tt = localBase tt > seconds
        mval = listToMaybe $ dropWhile f ttimes
        zone = maybe (fromZone utc) localZone mval
        base = seconds * 1000000 + micros
   if   maybe True (/= convert time) nextLeap
   then return $! LocalDateTimePicos base _uni_pic_pico zone
   else let picos = round (realToFrac (_uni_pic_base `mod` 86400) / 0.0000000864 :: Double)
        in  return $! LocalDateTimePicos base _uni_pic_pico zone `plus` Picos picos

-- | Convert a local date and time with nanosecond granularity into an integer.
fromLocalDateTimeNanos :: LocalDateTimeNanos -> Integer
fromLocalDateTimeNanos (LocalDateTimeNanos base nano _) = toInteger base * 1000 + toInteger nano

-- | Convert an integer into a local date and time with nanosecond granularity.
toLocalDateTimeNanos :: Integer -> (Word8 -> LocalDateTimeNanos)
toLocalDateTimeNanos n = LocalDateTimeNanos base nano
   where (base, nano) = doubleIntegral $ n `divMod` 1000

-- | Convert a local date and time with picosecond granularity into an integer.
fromLocalDateTimePicos :: LocalDateTimePicos -> Integer
fromLocalDateTimePicos (LocalDateTimePicos base pico _) = toInteger base * 1000000 + toInteger pico

-- | Convert an integer into a local date and time with picosecond granularity.
toLocalDateTimePicos :: Integer -> (Word8 -> LocalDateTimePicos)
toLocalDateTimePicos n = LocalDateTimePicos base pico
   where (base, pico) = doubleIntegral $ n `divMod` 1000000

instance Duration LocalDate Day where
    duration (LocalDate old _) (LocalDate new _) = Day (new - old)

instance Duration LocalDateTime Second where
    duration (LocalDateTime old _) (LocalDateTime new _) = Second (new - old)

instance Duration LocalDateTimeMillis Second where
    duration (LocalDateTimeMillis old _  ) (LocalDateTimeMillis new _  ) = Second (new - old) `div` 1000

instance Duration LocalDateTimeMicros Second where
    duration (LocalDateTimeMicros old _  ) (LocalDateTimeMicros new _  ) = Second (new - old) `div` 1000000

instance Duration LocalDateTimeNanos Second where
    duration (LocalDateTimeNanos  old _ _) (LocalDateTimeNanos  new _ _) = Second (new - old) `div` 1000000

instance Duration LocalDateTimePicos Second where
    duration (LocalDateTimePicos  old _ _) (LocalDateTimePicos  new _ _) = Second (new - old) `div` 1000000

instance Duration LocalDateTimeMillis Millis where
    duration (LocalDateTimeMillis old _  ) (LocalDateTimeMillis new _  ) = Millis (new - old)

instance Duration LocalDateTimeMicros Millis where
    duration (LocalDateTimeMicros old _  ) (LocalDateTimeMicros new _  ) = Millis (new - old) `div` 1000

instance Duration LocalDateTimeNanos Millis where
    duration (LocalDateTimeNanos  old _ _) (LocalDateTimeNanos  new _ _) = Millis (new - old) `div` 1000

instance Duration LocalDateTimePicos Millis where
    duration (LocalDateTimePicos  old _ _) (LocalDateTimePicos  new _ _) = Millis (new - old) `div` 1000

instance Duration LocalDateTimeMicros Micros where
    duration (LocalDateTimeMicros old _  ) (LocalDateTimeMicros new _  ) = Micros (new - old)

instance Duration LocalDateTimeNanos Micros where
    duration (LocalDateTimeNanos  old _ _) (LocalDateTimeNanos  new _ _) = Micros (new - old)

instance Duration LocalDateTimePicos Micros where
    duration (LocalDateTimePicos  old _ _) (LocalDateTimePicos  new _ _) = Micros (new - old)

instance Duration LocalDateTimeNanos Nanos where
    duration old new =
      if res < toInteger (maxBound::Int64) then Nanos $ fromInteger res
      else error "duration{LocalDateTimeNanos,Nanos}: integer overflow"
      where res = fromLocalDateTimeNanos new - fromLocalDateTimeNanos old

instance Duration LocalDateTimePicos Nanos where
    duration old new =
      if res < toInteger (maxBound::Int64) then Nanos $ fromInteger res `div` 1000
      else error "duration{LocalDateTimePicos,Nanos}: integer overflow"
      where res = fromLocalDateTimePicos new - fromLocalDateTimePicos old

instance Duration LocalDateTimePicos Picos where
    duration old new =
      if res < toInteger (maxBound::Int64) then Picos $ fromInteger res
      else error "duration{LocalDateTimePicos,Picos}: integer overflow"
      where res = fromLocalDateTimePicos new - fromLocalDateTimePicos old

instance Random LocalDate where
    random g =
      case randomR (0,2932896) g  of { (base, g' ) ->
      case randomR (0,maxZone) g' of { (zone, g'') -> (LocalDate base zone, g'') } }
    randomR (a,b) g =
      case randomR (get loc_day_base a, get loc_day_base b) g  of { (base, g' ) ->
      case randomR (get loc_day_zone a, get loc_day_zone b) g' of { (zone, g'') -> (LocalDate base zone, g'') } }

instance Random LocalDateTime where
    random g =
      case randomR (43200,253402257624) g  of { (base, g' ) ->
      case randomR (    0,     maxZone) g' of { (zone, g'') -> (LocalDateTime base zone, g'') } }
    randomR (a,b) g =
      case randomR (get loc_sec_base a, get loc_sec_base b) g  of { (base, g' ) ->
      case randomR (get loc_sec_zone a, get loc_sec_zone b) g' of { (zone, g'') -> (LocalDateTime base zone, g'') } }

instance Random LocalDateTimeMillis where
    random g =
      case randomR (43200,253402257624999) g  of { (base, g' ) ->
      case randomR (    0,        maxZone) g' of { (zone, g'') -> (LocalDateTimeMillis base zone, g'') } }
    randomR (a,b) g =
      case randomR (get loc_mil_base a, get loc_mil_base b) g  of { (base, g' ) ->
      case randomR (get loc_mil_zone a, get loc_mil_zone b) g' of { (zone, g'') -> (LocalDateTimeMillis base zone, g'') } }

instance Random LocalDateTimeMicros where
    random g =
      case randomR (43200,253402257624999999) g  of { (base, g' ) ->
      case randomR (    0,           maxZone) g' of { (zone, g'') -> (LocalDateTimeMicros base zone, g'') } }
    randomR (a,b) g =
      case randomR (get loc_mic_base a, get loc_mic_base b) g  of { (base, g' ) ->
      case randomR (get loc_mic_zone a, get loc_mic_zone b) g' of { (zone, g'') -> (LocalDateTimeMicros base zone, g'') } }

instance Random LocalDateTimeNanos where
    random g =
      case randomR (43200,253402257624999999999) g  of { (base, g' ) ->
      case randomR (    0,              maxZone) g' of { (zone, g'') -> (toLocalDateTimeNanos base zone, g'') } }
    randomR (a,b) g =
      case randomR (fromLocalDateTimeNanos a, fromLocalDateTimeNanos b) g  of { (base, g' ) ->
      case randomR (      get loc_nan_zone a,       get loc_nan_zone b) g' of { (zone, g'') -> (toLocalDateTimeNanos base zone, g'') } }

instance Random LocalDateTimePicos where
    random g =
      case randomR (43200,253402257624999999999999) g  of { (base, g' ) ->
      case randomR (    0,                 maxZone) g' of { (zone, g'') -> (toLocalDateTimePicos base zone, g'') } }
    randomR (a,b) g =
      case randomR (fromLocalDateTimePicos a, fromLocalDateTimePicos b) g  of { (base, g' ) ->
      case randomR (      get loc_pic_zone a,       get loc_pic_zone b) g' of { (zone, g'') -> (toLocalDateTimePicos base zone, g'') } }

deriving instance Random Transition

-- | Show a local date as a pretty string.
--
-- > >>> prettyLocalDate $ createLocalDate 2014 September 25 Japan_Standard_Time 
-- > "Thursday, September 25th, 2014 (JST)"
--
prettyLocalDate :: LocalDate -> String
prettyLocalDate date =
   printf "%s, %s %s, %04d (%s)" wday mon mday _dz_year abbr
   where DateZoneStruct{..} = toDateZoneStruct date
         wday = show _dz_wday
         mon  = show _dz_mon
         mday = show _dz_mday ++ showSuffix _dz_mday
         abbr = show $ abbreviate _dz_zone

-- | Show a local date and time as a pretty string.
--
-- > >>> getCurrentLocalDateTime Los_Angeles >>= return . prettyLocalDateTime 
-- > "2:17 AM, Wednesday, January 1st, 2014 (PST)"
--
prettyLocalDateTime :: DateTimeZone dtz => dtz -> String
prettyLocalDateTime time =
   printf str hour _dtz_min ampm wday mon mday _dtz_year abbr
   where DateTimeZoneStruct{..} = toDateTimeZoneStruct time
         str  = "%d:%02d %s, %s, %s %s, %04d (%s)"
         wday = show _dtz_wday
         mon  = show _dtz_mon
         mday = show _dtz_mday ++ showSuffix _dtz_mday
         abbr = show $ abbreviate _dtz_zone
         ampm = showPeriod _dtz_hour
         hour | _dtz_hour == 00 = 12
              | _dtz_hour <= 12 = _dtz_hour
              | otherwise       = _dtz_hour - 12

instance Zone LocalDate where
    rezone time = flip (set loc_day_zone) time . fromZone

instance Zone LocalDateTime where
    rezone time = flip (set loc_sec_zone) time . fromZone

instance Zone LocalDateTimeMillis where
    rezone time = flip (set loc_mil_zone) time . fromZone

instance Zone LocalDateTimeMicros where
    rezone time = flip (set loc_mic_zone) time . fromZone

instance Zone LocalDateTimeNanos where
    rezone time = flip (set loc_nan_zone) time . fromZone

instance Zone LocalDateTimePicos where
    rezone time = flip (set loc_pic_zone) time . fromZone

-- | Maximum enumerated time zone value.
maxZone :: Word8
maxZone = fromZone maxBound

-- | Convert an integral type into a time zone.
toZone ::  Word8 -> TimeZone
toZone = toEnum . fromIntegral

-- | Convert a time zone into a numeric value.
fromZone :: TimeZone -> Word8
fromZone = fromIntegral . fromEnum

-- | Perform a bounds check on the given local timestamp.
check :: forall a . Bounded a => Local a => Ord a => String -> a -> a
check f x =
   if minBound <= x && x <= maxBound then x
   else error $ f ++ ": base (" ++ base ++ ") out of bounds (" ++ bounds ++ ")"
   where base   = show (localBase x)
         bounds = show (localBase (minBound::a)) ++ "," ++ show (localBase (maxBound::a))

-- | Coerce a tuple of integral types.
doubleIntegral :: (Integral a, Integral b, Num c, Num d) => (a, b) -> (c, d)
doubleIntegral = fromIntegral *** fromIntegral
