-- |
-- Module     : Data.Time.Exts.Unix
-- Copyright  : 2013-2016 Enzo Haussecker
-- License    : BSD3
-- Maintainer : Enzo Haussecker <enzo@sovereign.io>
-- Stability  : Stable

{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE ViewPatterns               #-}

{-# OPTIONS -fno-warn-name-shadowing #-}

module Data.Time.Exts.Unix (

  -- * Timestamps
       UnixDate
     , UnixDateTime
     , UnixDateTimeNanos

  -- * Create Timestamps
     , createUnixDate
     , createUnixDateTime
     , createUnixDateTimeNanos

  -- * Get Current Timestamps
     , getCurrentUnixDate
     , getCurrentUnixDateTime
     , getCurrentUnixDateTimeNanos

  -- * Parse Timestamps
     , parseUnixDate
     , parseUnixDateTime
     , parseUnixDateTimeNanos

     ) where

import Control.Arrow    ((***), first)
import Control.DeepSeq  (NFData)
import Data.Data        (Data, Typeable)
import Data.Int         (Int32, Int64)
import Data.Text        (Text)
import Data.Time        (utc)
import Foreign.C.Time   (C'timeval(..), getTimeOfDay)
import Foreign.C.Types  (CLong(..))
import Foreign.Ptr      (plusPtr)
import Foreign.Storable (Storable(..))
import GHC.Generics     (Generic)
import Lens.Simple      (over)
import System.Locale    (TimeLocale)
import System.Random    (Random(..))
import Text.Printf      (printf)

import Data.Time.Exts.Base
import Data.Time.Exts.Format
import Data.Time.Exts.Lens
import Data.Time.Exts.Parser
import Data.Time.Exts.Util

-- |
-- Days since Unix epoch.
newtype UnixDate (cal :: Calendar) = UnixDate Int32
   deriving (Data, Eq, Generic, NFData, Ord, Storable, Typeable)

-- |
-- Seconds since Unix epoch (excluding leap seconds).
newtype UnixDateTime (cal :: Calendar) = UnixDateTime Int64
   deriving (Data, Eq, Generic, NFData, Ord, Storable, Typeable)

-- |
-- Nanoseconds since Unix epoch (excluding leap seconds).
data UnixDateTimeNanos (cal :: Calendar) = UnixDateTimeNanos {-# UNPACK #-} !Int64 {-# UNPACK #-} !Int32
   deriving (Data, Eq, Generic, Ord, Typeable)

instance Bounded (UnixDate 'Gregorian) where

   -- Thu Jan 01 1970.
   minBound = UnixDate 0

   -- Fri Dec 31 9999.
   maxBound = UnixDate 2932896

instance Bounded (UnixDateTime 'Gregorian) where

   -- 12:00:00 AM Thu Jan 01 1970.
   minBound = UnixDateTime 0

   -- 11:59:59 PM Fri Dec 31 9999.
   maxBound = UnixDateTime 253402300799

instance Bounded (UnixDateTimeNanos 'Gregorian) where

   -- 12:00:00.000000000 AM Thu Jan 01 1970.
   minBound = UnixDateTimeNanos 0 0

   -- 11:59:59.999999999 PM Fri Dec 31 9999.
   maxBound = UnixDateTimeNanos 253402300799 999999999

instance Enum (UnixDate 'Gregorian) where

   -- Next day.
   succ = flip plus (Day 1)

   -- Previous day.
   pred = flip plus (- Day 1)

   -- Unenumerate a Unix datestamp.
   fromEnum (UnixDate base) = fromIntegral base

   -- Enumerate a Unix datestamp.
   toEnum base =
      if minBound <= date && date <= maxBound then date
      else error "toEnum{UnixDate 'Gregorian}: out of bounds"
      where date = UnixDate $ fromIntegral base

instance Enum (UnixDateTime 'Gregorian) where

   -- Next second.
   succ = flip plus (Second 1)

   -- Previous second.
   pred = flip plus (- Second 1)

   -- Unenumerate a Unix timestamp.
   fromEnum (UnixDateTime base) = fromIntegral base

   -- Enumerate a Unix timestamp.
   toEnum base =
      if minBound <= time && time <= maxBound then time
      else error "toEnum{UnixDateTime 'Gregorian}: out of bounds"
      where time = UnixDateTime $ fromIntegral base

instance Human (UnixDate 'Gregorian) where

   -- Define the Gregorian components of a Unix datestamp.
   type Components (UnixDate 'Gregorian) = DateStruct 'Gregorian

   -- Pack a Unix datestamp from Gregorian components.
   pack DateStruct {..} =
      createUnixDate _d_year _d_mon _d_mday

   -- Unpack a Unix datestamp to Gregorian components.
   unpack (UnixDate base) =
      rec 1970 (Day base) where
      rec !year !day =
         if day >= size
         then rec (year + 1) (day - size)
         else DateStruct year mon mday wday
         where
            wday = toEnum (1 + mod (fromIntegral base + 4) 7)
            leap = isLeapYear year
            size = if leap then 366 else 365
            (mon, mday) =
               if leap
               then if day >= 182
                  then if day >= 274
                     then if day >= 335
                        then (December, day - 334)
                        else if day >= 305
                           then (November, day - 304)
                           else (October, day - 273)
                     else if day >= 244
                        then (September, day - 243)
                        else if day >= 213
                           then (August, day - 212)
                           else (July, day - 181)
                  else if day >= 091
                     then if day >= 152
                        then (June, day - 151)
                        else if day >= 121
                           then (May, day - 120)
                           else (April, day - 090)
                     else if day >= 060
                        then (March, day - 059)
                        else if day >= 031
                           then (February, day - 030)
                           else (January, day + 001)
               else if day >= 181
                  then if day >= 273
                     then if day >= 334
                        then (December, day - 333)
                        else if day >= 304
                           then (November, day - 303)
                           else (October, day - 272)
                     else if day >= 243
                        then (September, day - 242)
                        else if day >= 212
                           then (August, day - 211)
                           else (July, day - 180)
                  else if day >= 090
                     then if day >= 151
                        then (June, day - 150)
                        else if day >= 120
                           then (May, day - 119)
                           else (April, day - 089)
                     else if day >= 059
                        then (March, day - 058)
                        else if day >= 031
                           then (February, day - 030)
                           else (January, day + 001)

instance Human (UnixDateTime 'Gregorian) where

   -- Define the Gregorian components of a Unix timestamp.
   type Components (UnixDateTime 'Gregorian) = DateTimeStruct 'Gregorian

   -- Pack a Unix timestamp from Gregorian components.
   pack DateTimeStruct {..} =
      createUnixDateTime _dt_year _dt_mon _dt_mday _dt_hour _dt_min sec
      where sec = round _dt_sec

   -- Unpack a Unix timestamp to Gregorian components.
   unpack (UnixDateTime base) =
      DateTimeStruct _d_year _d_mon _d_mday _d_wday hour min sec where
      DateStruct {..} = unpack (UnixDate day :: UnixDate 'Gregorian)
      (day, hms) = fromIntegral *** fromIntegral $ divMod base 86400
      (hour, ms) = fromIntegral <$> divMod hms 3600
      (min, sec) = realToFrac   <$> divMod  ms 0060

instance Human (UnixDateTimeNanos 'Gregorian) where

   -- Define the Gregorian components of a Unix timestamp with nanosecond granularity.
   type Components (UnixDateTimeNanos 'Gregorian) = DateTimeStruct 'Gregorian

   -- Pack a Unix timestamp with nanosecond granularity from Gregorian components.
   pack DateTimeStruct {..} =
      createUnixDateTimeNanos _dt_year _dt_mon _dt_mday _dt_hour _dt_min sec nsec
      where (sec, nsec) = properFracNanos _dt_sec

   -- Unpack a Unix timestamp with nanosecond granularity to Gregorian components.
   unpack (UnixDateTimeNanos base nsec) =
      over dt_sec (+ frac) $ unpack time
      where time = UnixDateTime base :: UnixDateTime 'Gregorian
            frac = realToFrac nsec / 1000000000

instance Math (UnixDate 'Gregorian) Day where

   -- Compute the day duration between two Unix datestamps.
   duration (UnixDate old) (UnixDate new) = fromIntegral (new - old)

   -- Add days to a Unix datestamp.
   plus (UnixDate base) days =
      if minBound <= date && date <= maxBound then date
      else error "plus{UnixDate 'Gregorian, Day}: out of bounds"
      where date = UnixDate (base + fromIntegral days)

instance Math (UnixDateTime 'Gregorian) Day where

   -- Compute the day duration between two Unix timestamps.
   duration (UnixDateTime old) (UnixDateTime new) = fromIntegral (div (new - old) 86400)

   -- Add days to a Unix timestamp.
   plus (UnixDateTime base) days =
      if minBound <= time && time <= maxBound then time
      else error "plus{UnixDateTime 'Gregorian, Day}: out of bounds"
      where time = UnixDateTime (base + fromIntegral days * 86400)

instance Math (UnixDateTime 'Gregorian) Hour where

   -- Compute the hour duration between two Unix timestamps.
   duration (UnixDateTime old) (UnixDateTime new) = fromIntegral (div (new - old) 3600)

   -- Add hours to a Unix timestamp.
   plus (UnixDateTime base) hours =
      if minBound <= time && time <= maxBound then time
      else error "plus{UnixDateTime 'Gregorian, Hour}: out of bounds"
      where time = UnixDateTime (base + fromIntegral hours * 3600)

instance Math (UnixDateTime 'Gregorian) Minute where

   -- Compute the minute duration between two Unix timestamps.
   duration (UnixDateTime old) (UnixDateTime new) = fromIntegral (div (new - old) 60)

   -- Add minutes to a Unix timestamp.
   plus (UnixDateTime base) minutes =
      if minBound <= time && time <= maxBound then time
      else error "plus{UnixDateTime 'Gregorian, Minute}: out of bounds"
      where time = UnixDateTime (base + fromIntegral minutes * 60)

instance Math (UnixDateTime 'Gregorian) Second where

   -- Compute the second duration between two Unix timestamps.
   duration (UnixDateTime old) (UnixDateTime new) = fromIntegral (new - old)

   -- Add seconds to a Unix timestamp.
   plus (UnixDateTime base) seconds =
      if minBound <= time && time <= maxBound then time
      else error "plus{UnixDateTime 'Gregorian, Second}: out of bounds"
      where time = UnixDateTime (base + fromIntegral seconds)

instance Math (UnixDateTimeNanos 'Gregorian) Day where

   -- Compute the day duration between two Unix timestamps with nanosecond granularity.
   duration (UnixDateTimeNanos old _) (UnixDateTimeNanos new _) = fromIntegral (div (new - old) 86400)

   -- Add days to a Unix timestamp with nanosecond granularity.
   plus (UnixDateTimeNanos base nsec) days =
      if minBound <= time && time <= maxBound then time
      else error "plus{UnixDateTimeNanos 'Gregorian, Day}: out of bounds"
      where time = UnixDateTimeNanos (base + fromIntegral days * 86400) nsec

instance Math (UnixDateTimeNanos 'Gregorian) Hour where

   -- Compute the hour duration between two Unix timestamps with nanosecond granularity.
   duration (UnixDateTimeNanos old _) (UnixDateTimeNanos new _) = fromIntegral (div (new - old) 3600)

   -- Add hours to a Unix timestamp with nanosecond granularity.
   plus (UnixDateTimeNanos base nsec) hours =
      if minBound <= time && time <= maxBound then time
      else error "plus{UnixDateTimeNanos 'Gregorian, Hour}: out of bounds"
      where time = UnixDateTimeNanos (base + fromIntegral hours * 3600) nsec

instance Math (UnixDateTimeNanos 'Gregorian) Minute where

   -- Compute the minute duration between two Unix timestamps with nanosecond granularity.
   duration (UnixDateTimeNanos old _) (UnixDateTimeNanos new _) = fromIntegral (div (new - old) 60)

   -- Add minutes to a Unix timestamp with nanosecond granularity.
   plus (UnixDateTimeNanos base nsec) minutes =
      if minBound <= time && time <= maxBound then time
      else error "plus{UnixDateTimeNanos 'Gregorian, Minute}: out of bounds"
      where time = UnixDateTimeNanos (base + fromIntegral minutes * 60) nsec

instance Math (UnixDateTimeNanos 'Gregorian) Second where

   -- Compute the second duration between two Unix timestamps with nanosecond granularity.
   duration (UnixDateTimeNanos old _) (UnixDateTimeNanos new _) = fromIntegral (new - old)

   -- Add seconds to a Unix timestamp with nanosecond granularity.
   plus (UnixDateTimeNanos base nsec) seconds =
      if minBound <= time && time <= maxBound then time
      else error "plus{UnixDateTimeNanos 'Gregorian, Second}: out of bounds"
      where time = UnixDateTimeNanos (base + fromIntegral seconds) nsec

instance Math (UnixDateTimeNanos 'Gregorian) Millis where

   -- Compute the millisecond duration between two Unix timestamps with nanosecond granularity.
   duration old new = fold new - fold old
      where fold (UnixDateTimeNanos base nsec) =
               fromIntegral base * 1000 + fromIntegral (div nsec 1000000)

   -- Add milliseconds to a Unix timestamp with nanosecond granularity.
   plus (UnixDateTimeNanos base nsec) millis =
      if minBound <= time && time <= maxBound then time
      else error "plus{UnixDateTimeNanos 'Gregorian, Millis}: out of bounds"
      where nsum = fromIntegral nsec + fromIntegral millis * 1000000
            time = uncurry UnixDateTimeNanos ((***) (+ base) fromIntegral (divMod nsum 1000000000))

instance Math (UnixDateTimeNanos 'Gregorian) Micros where

   -- Compute the microsecond duration between two Unix timestamps with nanosecond granularity.
   duration old new = fold new - fold old
      where fold (UnixDateTimeNanos base nsec) =
               fromIntegral base * 1000000 + fromIntegral (div nsec 1000)

   -- Add microseconds to a Unix timestamp with nanosecond granularity.
   plus (UnixDateTimeNanos base nsec) micros =
      if minBound <= time && time <= maxBound then time
      else error "plus{UnixDateTimeNanos 'Gregorian, Micros}: out of bounds"
      where nsum = fromIntegral nsec + fromIntegral micros * 1000
            time = uncurry UnixDateTimeNanos ((***) (+ base) fromIntegral (divMod nsum 1000000000))

instance Math (UnixDateTimeNanos 'Gregorian) Nanos where

   -- Compute the nanosecond duration between two Unix timestamps with nanosecond granularity.
   duration old new =
      if toInteger (minBound :: Int64) <= res &&
         toInteger (maxBound :: Int64) >= res then fromInteger res
      else error "duration{UnixDateTimeNanos 'Gregorian, Nanos}: integer overflow"
      where res = fold new - fold old
            fold (UnixDateTimeNanos base nsec) =
               toInteger base * 1000000000 + toInteger nsec

   -- Add nanoseconds to a Unix timestamp with nanosecond granularity.
   plus (UnixDateTimeNanos base nsec) nanos =
      if minBound <= time && time <= maxBound then time
      else error "plus{UnixDateTimeNanos 'Gregorian, Nanos}: out of bounds"
      where nsum = fromIntegral nsec + fromIntegral nanos
            time = uncurry UnixDateTimeNanos ((***) (+ base) fromIntegral (divMod nsum 1000000000))

instance NFData (UnixDateTimeNanos cal)

instance Random (UnixDate 'Gregorian) where

   -- Generate a random Unix datestamp.
   random = first toEnum . randomR (fromEnum a, fromEnum b)
      where a = minBound :: UnixDate 'Gregorian
            b = maxBound :: UnixDate 'Gregorian

   -- Generate a random Unix datestamp uniformly distributed on the closed interval.
   randomR (a, b) = first toEnum . randomR (fromEnum a, fromEnum b)

instance Random (UnixDateTime 'Gregorian) where

   -- Generate a random Unix timestamp.
   random = first toEnum . randomR (fromEnum a, fromEnum b)
      where a = minBound :: UnixDateTime 'Gregorian
            b = maxBound :: UnixDateTime 'Gregorian

   -- Generate a random Unix timestamp uniformly distributed on the closed interval.
   randomR (a, b) = first toEnum . randomR (fromEnum a, fromEnum b)

instance Random (UnixDateTimeNanos 'Gregorian) where

   -- Generate a random Unix timestamp with nanosecond granularity.
   random = first toNano . randomR (fromNano a, fromNano b)
      where a = minBound :: UnixDateTimeNanos 'Gregorian
            b = maxBound :: UnixDateTimeNanos 'Gregorian

   -- Generate a random Unix timestamp with nanosecond granularity uniformly distributed on the closed interval.
   randomR (a, b) = first toNano . randomR (fromNano a, fromNano b)

instance Show (UnixDate 'Gregorian) where

   -- Show a Unix datestamp.
   show (unpack -> DateStruct {..}) =
      printf "%.3s %.3s %02d %4d" (show _d_wday) (show _d_mon) _d_mday _d_year

instance Show (UnixDateTime 'Gregorian) where

   -- Show a Unix timestamp.
   show (unpack -> DateTimeStruct {..}) =
      printf "%02d:%02d:%02d %s %.3s %.3s %02d %4d" hour _dt_min sec ampm wday mon _dt_mday _dt_year
      where wday = show _dt_wday
            mon  = show _dt_mon
            sec  = round _dt_sec :: Second
            (,) ampm hour = getPeriod _dt_hour

instance Show (UnixDateTimeNanos 'Gregorian) where

   -- Show a Unix timestamp with nanosecond granularity.
   show (unpack -> DateTimeStruct {..}) =
      printf "%02d:%02d:%02d.%09d %s %.3s %.3s %02d %4d" hour _dt_min sec nsec ampm wday mon _dt_mday _dt_year
      where wday = show _dt_wday
            mon  = show _dt_mon
            (,) sec  nsec = properFracNanos _dt_sec
            (,) ampm hour = getPeriod _dt_hour

instance Storable (UnixDateTimeNanos cal) where

   -- Size of Unix timestamp with nanosecond granularity.
   sizeOf = const 12

   -- Alignment of Unix timestamp with nanosecond granularity.
   alignment = sizeOf

   -- Read a Unix timestamp with nanosecond granularity from memory.
   peekElemOff ptr n = do
      let off = 12 * n
      base <- peek (plusPtr ptr (off + 0))
      nsec <- peek (plusPtr ptr (off + 8))
      return $! UnixDateTimeNanos base nsec

   -- Write a Unix timestamp with nanosecond granularity to memory.
   pokeElemOff ptr n (UnixDateTimeNanos base nsec) = do
      let off = 12 * n
      poke (plusPtr ptr (off + 0)) base
      poke (plusPtr ptr (off + 8)) nsec

-- |
-- Create a Unix datestamp.
createUnixDate
   :: Year
   -> Month 'Gregorian
   -> Day
   -> UnixDate 'Gregorian
createUnixDate year mon mday =
   if minBound <= date && date <= maxBound then date
   else error "createUnixDate: out of bounds"
   where Day base = unsafeEpochToDate year mon mday
         date = UnixDate base

-- |
-- Create a Unix timestamp.
createUnixDateTime
   :: Year
   -> Month 'Gregorian
   -> Day
   -> Hour
   -> Minute
   -> Second
   -> UnixDateTime 'Gregorian
createUnixDateTime year mon mday hour min sec =
   if minBound <= time && time <= maxBound then time
   else error "createUnixDateTime: out of bounds"
   where Second base = unsafeEpochToDateTime year mon mday hour min sec
         time = UnixDateTime base

-- |
-- Create a Unix timestamp with nanosecond granularity.
createUnixDateTimeNanos
   :: Year
   -> Month 'Gregorian
   -> Day
   -> Hour
   -> Minute
   -> Second
   -> Nanos
   -> UnixDateTimeNanos 'Gregorian
createUnixDateTimeNanos year mon mday hour min sec nanos =
   if minBound <= time && time <= maxBound then time
   else error "createUnixDateTimeNanos: out of bounds"
   where Second base = unsafeEpochToDateTime year mon mday hour min sec
         nsum = fromIntegral nanos
         time = uncurry UnixDateTimeNanos ((***) (+ base) fromIntegral (divMod nsum 1000000000))

-- |
-- Get the current Unix datestamp from the system clock.
getCurrentUnixDate :: IO (UnixDate 'Gregorian)
getCurrentUnixDate =
   getTimeOfDay >>= \ (C'timeval (CLong base) _) ->
      return $! UnixDate (fromIntegral (div base 86400))

-- |
-- Get the current Unix timestamp from the system clock.
getCurrentUnixDateTime :: IO (UnixDateTime 'Gregorian)
getCurrentUnixDateTime =
   getTimeOfDay >>= \ (C'timeval (CLong base) _) ->
      return $! UnixDateTime base

-- |
-- Get the current Unix timestamp with nanosecond granularity from the system clock.
getCurrentUnixDateTimeNanos :: IO (UnixDateTimeNanos 'Gregorian)
getCurrentUnixDateTimeNanos =
   getTimeOfDay >>= \ (C'timeval (CLong base) (CLong usec)) ->
      return $! UnixDateTimeNanos base (fromIntegral usec * 1000)

-- |
-- Default parser state.
state :: ParserState 'Gregorian
state =  ParserState 1970 January 1 Thursday 0 0 0.0 id id (const (return utc))

-- |
-- Parse a Unix datestamp.
parseUnixDate
   :: TimeLocale
   -> Format
   -> Text
   -> Either String (UnixDate 'Gregorian)
parseUnixDate locale format input =
   build <$> runParser locale Nothing state format input
   where build ParserState {..} =
            createUnixDate _ps_year _ps_mon _ps_mday

-- |
-- Parse a Unix timestamp.
parseUnixDateTime
   :: TimeLocale
   -> Format
   -> Text
   -> Either String (UnixDateTime 'Gregorian)
parseUnixDateTime locale format input =
   build <$> runParser locale Nothing state format input
   where build ParserState {..} =
            createUnixDateTime _ps_year _ps_mon _ps_mday hour _ps_min sec
            where hour = _ps_ampm _ps_hour
                  sec  = truncate _ps_sec

-- |
-- Parse a Unix timestamp with nanosecond granularity.
parseUnixDateTimeNanos
   :: TimeLocale
   -> Format
   -> Text
   -> Either String (UnixDateTimeNanos 'Gregorian)
parseUnixDateTimeNanos locale format input =
   build <$> runParser locale Nothing state format input
   where build ParserState {..} =
            createUnixDateTimeNanos _ps_year _ps_mon _ps_mday hour _ps_min sec nsec
            where hour = _ps_ampm _ps_hour
                  (,) sec nsec = properFracNanos $ _ps_frac _ps_sec

-- |
-- Convert an integer into a Unix timestamp with nanosecond granularity.
toNano :: Integer -> UnixDateTimeNanos 'Gregorian
toNano = uncurry UnixDateTimeNanos . (***) fromInteger fromInteger . flip divMod 1000000000

-- |
-- Convert a Unix timestamp with nanosecond granularity into an integer.
fromNano :: UnixDateTimeNanos 'Gregorian -> Integer
fromNano (UnixDateTimeNanos base nsec) = toInteger base * 1000000000 + toInteger nsec

-- |
-- Check if the given year is a leap year.
isLeapYear :: Year -> Bool
isLeapYear year = mod year 400 == 0 || (mod year 100 /= 0 && mod year 4 == 0)

-- |
-- Calculate the number of days that have elapsed between Unix epoch and the given year without performing any bounds check.
unsafeEpochToYear :: Year -> Day
unsafeEpochToYear Year {..} = Day (365 * (getYear - 1970) + div (getYear - 1969) 004 - div (getYear - 1901) 100 + div (getYear - 1601) 400)

-- |
-- Calculate the number of days that have elapsed between Unix epoch and the given Unix datestamp without performing any bounds check.
unsafeEpochToDate :: Year -> Month 'Gregorian -> Day -> Day
unsafeEpochToDate year mon mday =
   unsafeEpochToYear year + yearToMonth leap mon + mday - 1
   where leap = isLeapYear year

-- |
-- Calculate the number of seconds that have elapsed between Unix epoch and the given Unix timestamp without performing any bounds check.
unsafeEpochToDateTime :: Year -> Month 'Gregorian -> Day -> Hour -> Minute -> Second -> Second
unsafeEpochToDateTime year mon mday hour min sec =
   fromIntegral day * 86400 + fromIntegral hour * 3600 + fromIntegral min * 60 + sec
   where day = unsafeEpochToDate year mon mday

-- |
-- Calculate the number of days that have elapsed between January 1st and the given month.
yearToMonth :: Bool -> Month 'Gregorian -> Day
yearToMonth leap =
   if leap
   then \ case
      January   -> 000
      February  -> 031
      March     -> 060
      April     -> 091
      May       -> 121
      June      -> 152
      July      -> 182
      August    -> 213
      September -> 244
      October   -> 274
      November  -> 305
      December  -> 335
   else \ case
      January   -> 000
      February  -> 031
      March     -> 059
      April     -> 090
      May       -> 120
      June      -> 151
      July      -> 181
      August    -> 212
      September -> 243
      October   -> 273
      November  -> 304
      December  -> 334
