-- |
-- Module     : Data.Time.Exts.UTC
-- Copyright  : 2013-2016 Enzo Haussecker
-- License    : BSD3
-- Maintainer : Enzo Haussecker <enzo@sovereign.io>
-- Stability  : Stable
--
-- A native implementation of Coordinated Universal Time.

{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE ViewPatterns               #-}

{-# OPTIONS -fno-warn-name-shadowing #-}

module Data.Time.Exts.UTC (

  -- * Timestamps
       UTCDate(..)
     , UTCDateTime(..)
     , UTCDateTimeNanos(..)

  -- * Create
     , createUTCDate
     , createUTCDateTime
     , createUTCDateTimeNanos

  -- * Get
     , getCurrentUTCDate
     , getCurrentUTCDateTime
     , getCurrentUTCDateTimeNanos

  -- * Parse
     , parseUTCDate
     , parseUTCDateTime
     , parseUTCDateTimeNanos

     ) where

import Control.Arrow    ((***), first)
import Control.DeepSeq  (NFData)
import Control.Monad    (join)
import Data.Data        (Data, Typeable)
import Data.Int         (Int32, Int64)
import Data.Text        (Text)
import Data.Time.Zones  (utcTZ)
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
import Data.Time.Exts.Unix
import Data.Time.Exts.Util

-- |
-- Days since Unix epoch.
newtype UTCDate cal = UTCDate (UnixDate cal)
   deriving (Data, Eq, Generic, NFData, Ord, Storable, Typeable)

-- |
-- Seconds since Unix epoch (including leap seconds).
newtype UTCDateTime (cal :: Calendar) = UTCDateTime Int64
   deriving (Data, Eq, Generic, NFData, Ord, Storable, Typeable)

-- |
-- Nanoseconds since Unix epoch (including leap seconds).
data UTCDateTimeNanos (cal :: Calendar) = UTCDateTimeNanos {-# UNPACK #-} !Int64 {-# UNPACK #-} !Int32
   deriving (Data, Eq, Generic, Ord, Typeable)

deriving instance Bounded (UTCDate 'Gregorian)

instance Bounded (UTCDateTime 'Gregorian) where

   -- 12:00:00 AM Thu Jan 01 1970 UTC.
   minBound = UTCDateTime 0

   -- 11:59:59 PM Fri Dec 31 9999 UTC.
   maxBound = UTCDateTime 253402300827

instance Bounded (UTCDateTimeNanos 'Gregorian) where

   -- 12:00:00.000000000 AM Thu Jan 01 1970 UTC.
   minBound = UTCDateTimeNanos 0 0

   -- 11:59:59.999999999 PM Fri Dec 31 9999 UTC.
   maxBound = UTCDateTimeNanos 253402300827 999999999

deriving instance Enum (UTCDate 'Gregorian)

instance Enum (UTCDateTime 'Gregorian) where

   -- Next second.
   succ = flip plus (Second 1)

   -- Previous second.
   pred = flip plus (- Second 1)

   -- Denumerate a UTC timestamp.
   fromEnum (UTCDateTime base) = fromIntegral base

   -- Enumerate a UTC timestamp.
   toEnum base =
      if minBound <= time && time <= maxBound then time
      else error "toEnum{UTCDateTime 'Gregorian}: out of bounds"
      where time = UTCDateTime $ fromIntegral base

instance Human (UTCDate 'Gregorian) where

   -- Define the Gregorian components of a UTC datestamp.
   type Components (UTCDate 'Gregorian) = DateStruct 'Gregorian

   -- Pack a UTC datestamp from Gregorian components.
   pack DateStruct {..} =
      createUTCDate _d_year _d_mon _d_mday

   -- Unpack a UTC datestamp to Gregorian components.
   unpack (UTCDate date) = unpack date

instance Human (UTCDateTime 'Gregorian) where

   -- Define the Gregorian components of a UTC timestamp.
   type Components (UTCDateTime 'Gregorian) = DateTimeStruct 'Gregorian

   -- Pack a UTC timestamp from Gregorian components.
   pack DateTimeStruct {..} =
      createUTCDateTime _dt_year _dt_mon _dt_mday _dt_hour _dt_min sec
      where sec = round _dt_sec

   -- Unpack a UTC timestamp to Gregorian components.
   unpack (UTCDateTime base) =
      over dt_sec (+ leap) (unpack time)
      where time = UnixDateTime unix :: UnixDateTime 'Gregorian
            (,) unix leap = baseUTCToUnix base

instance Human (UTCDateTimeNanos 'Gregorian) where

   -- Define the Gregorian components of a UTC timestamp with nanosecond granularity.
   type Components (UTCDateTimeNanos 'Gregorian) = DateTimeStruct 'Gregorian

   -- Pack a UTC timestamp with nanosecond granularity from Gregorian components.
   pack DateTimeStruct {..} =
      createUTCDateTimeNanos _dt_year _dt_mon _dt_mday _dt_hour _dt_min sec nsec
      where (,) sec nsec = properFracNanos _dt_sec

   -- Unpack a UTC timestamp with nanosecond granularity to Gregorian components.
   unpack (UTCDateTimeNanos base nsec) =
      over dt_sec (+ leap) (unpack time)
      where time = UnixDateTimeNanos unix nsec :: UnixDateTimeNanos 'Gregorian
            (,) unix leap = baseUTCToUnix base

instance Math (UTCDate 'Gregorian) Day where

   -- Compute the day duration between two UTC datestamps.
   duration (UTCDate old) (UTCDate new) = duration old new

   -- Add days to a UTC datestamp.
   plus (UTCDate date) days = UTCDate (plus date days)

instance Math (UTCDateTime 'Gregorian) Second where

   -- Compute the second duration between two UTC timestamps.
   duration (UTCDateTime old) (UTCDateTime new) = fromIntegral (new - old)

   -- Add seconds to a UTC timestamp.
   plus (UTCDateTime base) seconds =
      if minBound <= time && time <= maxBound then time
      else error "plus{UTCDateTime 'Gregorian, Second}: out of bounds"
      where time = UTCDateTime (base + fromIntegral seconds)

instance Math (UTCDateTimeNanos 'Gregorian) Second where

   -- Compute the second duration between two UTC timestamps with nanosecond granularity.
   duration (UTCDateTimeNanos old _) (UTCDateTimeNanos new _) = fromIntegral (new - old)

   -- Add seconds to a UTC timestamp with nanosecond granularity.
   plus (UTCDateTimeNanos base nsec) seconds =
      if minBound <= time && time <= maxBound then time
      else error "plus{UTCDateTimeNanos 'Gregorian, Second}: out of bounds"
      where time = UTCDateTimeNanos (base + fromIntegral seconds) nsec

instance Math (UTCDateTimeNanos 'Gregorian) Millis where

   -- Compute the millisecond duration between two UTC timestamps with nanosecond granularity.
   duration old new = fold new - fold old
      where fold (UTCDateTimeNanos base nsec) =
               fromIntegral base * 1000 + fromIntegral (div nsec 1000000)

   -- Add milliseconds to a UTC timestamp with nanosecond granularity.
   plus (UTCDateTimeNanos base nsec) millis =
      if minBound <= time && time <= maxBound then time
      else error "plus{UTCDateTimeNanos 'Gregorian, Millis}: out of bounds"
      where nsum = fromIntegral nsec + fromIntegral millis * 1000000
            time = uncurry UTCDateTimeNanos ((***) (+ base) fromIntegral (divMod nsum 1000000000))

instance Math (UTCDateTimeNanos 'Gregorian) Micros where

   -- Compute the microsecond duration between two UTC timestamps with nanosecond granularity.
   duration old new = fold new - fold old
      where fold (UTCDateTimeNanos base nsec) =
               fromIntegral base * 1000000 + fromIntegral (div nsec 1000)

   -- Add microseconds to a UTC timestamp with nanosecond granularity.
   plus (UTCDateTimeNanos base nsec) micros =
      if minBound <= time && time <= maxBound then time
      else error "plus{UTCDateTimeNanos 'Gregorian, Micros}: out of bounds"
      where nsum = fromIntegral nsec + fromIntegral micros * 1000
            time = uncurry UTCDateTimeNanos ((***) (+ base) fromIntegral (divMod nsum 1000000000))

instance Math (UTCDateTimeNanos 'Gregorian) Nanos where

   -- Compute the nanosecond duration between two UTC timestamps with nanosecond granularity.
   duration old new =
      if toInteger (minBound :: Int64) <= res &&
         toInteger (maxBound :: Int64) >= res then fromInteger res
      else error "duration{UTCDateTimeNanos 'Gregorian, Nanos}: integer overflow"
      where res = fold new - fold old
            fold (UTCDateTimeNanos base nsec) =
               toInteger base * 1000000000 + toInteger nsec

   -- Add nanoseconds to a UTC timestamp with nanosecond granularity.
   plus (UTCDateTimeNanos base nsec) nanos =
      if minBound <= time && time <= maxBound then time
      else error "plus{UTCDateTimeNanos 'Gregorian, Nanos}: out of bounds"
      where nsum = fromIntegral nsec + fromIntegral nanos
            time = uncurry UTCDateTimeNanos ((***) (+ base) fromIntegral (divMod nsum 1000000000))

instance NFData (UTCDateTimeNanos cal)

deriving instance Random (UTCDate 'Gregorian)

instance Random (UTCDateTime 'Gregorian) where

   -- Generate a random UTC timestamp.
   random = first toEnum . randomR (fromEnum a, fromEnum b)
      where a = minBound :: UTCDateTime 'Gregorian
            b = maxBound :: UTCDateTime 'Gregorian

   -- Generate a random UTC timestamp uniformly distributed on the closed interval.
   randomR (a, b) = first toEnum . randomR (fromEnum a, fromEnum b)

instance Random (UTCDateTimeNanos 'Gregorian) where

   -- Generate a random UTC timestamp with nanosecond granularity.
   random = first toNano . randomR (fromNano a, fromNano b)
      where a = minBound :: UTCDateTimeNanos 'Gregorian
            b = maxBound :: UTCDateTimeNanos 'Gregorian

   -- Generate a random UTC timestamp with nanosecond granularity uniformly distributed on the closed interval.
   randomR (a, b) = first toNano . randomR (fromNano a, fromNano b)

instance Show (UTCDate 'Gregorian) where

   -- Show a UTC datestamp.
   show (unpack -> DateStruct {..}) =
      printf "%.3s %.3s %02d %4d UTC" wday mon _d_mday _d_year
      where wday = show _d_wday
            mon  = show _d_mon

instance Show (UTCDateTime 'Gregorian) where

   -- Show a UTC timestamp.
   show (unpack -> DateTimeStruct {..}) =
      printf "%02d:%02d:%02d %s %.3s %.3s %02d %4d UTC" hour _dt_min sec ampm wday mon _dt_mday _dt_year
      where wday = show _dt_wday
            mon  = show _dt_mon
            sec  = round _dt_sec :: Second
            (,) ampm hour = getPeriod _dt_hour

instance Show (UTCDateTimeNanos 'Gregorian) where

   -- Show a UTC timestamp with nanosecond granularity.
   show (unpack -> DateTimeStruct {..}) =
      printf "%02d:%02d:%02d.%09d %s %.3s %.3s %02d %4d UTC" hour _dt_min sec nsec ampm wday mon _dt_mday _dt_year
      where wday = show _dt_wday
            mon  = show _dt_mon
            (,) sec  nsec = properFracNanos _dt_sec
            (,) ampm hour = getPeriod _dt_hour

instance Storable (UTCDateTimeNanos cal) where

   -- Size of UTC timestamp with nanosecond granularity.
   sizeOf = const 12

   -- Alignment of UTC timestamp with nanosecond granularity.
   alignment = sizeOf

   -- Read a UTC timestamp with nanosecond granularity from memory.
   peekElemOff ptr n = do
      let off = 12 * n
      base <- peek (plusPtr ptr (off + 0))
      nsec <- peek (plusPtr ptr (off + 8))
      return $! UTCDateTimeNanos base nsec

   -- Write a UTC timestamp with nanosecond granularity to memory.
   pokeElemOff ptr n (UTCDateTimeNanos base nsec) = do
      let off = 12 * n
      poke (plusPtr ptr (off + 0)) base
      poke (plusPtr ptr (off + 8)) nsec

-- |
-- Create a UTC datestamp.
createUTCDate
   :: Year
   -> Month 'Gregorian
   -> Day
   -> UTCDate 'Gregorian
createUTCDate year mon mday =
   UTCDate $ createUnixDate year mon mday

-- |
-- Create a UTC timestamp.
createUTCDateTime
   :: Year
   -> Month 'Gregorian
   -> Day
   -> Hour
   -> Minute
   -> Second
   -> UTCDateTime 'Gregorian
createUTCDateTime year mon mday hour min sec =
   if (minBound :: UTCDateTime 'Gregorian) <= time && time <= (maxBound :: UTCDateTime 'Gregorian) then time
   else error "createUTCDateTime: out of bounds"
   where UnixDateTime unix = createUnixDateTime year mon mday hour min 0
         time = UTCDateTime base
         base = baseUnixToUTC unix + fromIntegral sec

-- |
-- Create a UTC timestamp with nanosecond granularity.
createUTCDateTimeNanos
   :: Year
   -> Month 'Gregorian
   -> Day
   -> Hour
   -> Minute
   -> Second
   -> Nanos
   -> UTCDateTimeNanos 'Gregorian
createUTCDateTimeNanos year mon mday hour min sec nanos =
   if (minBound :: UTCDateTimeNanos 'Gregorian) <= time && time <= (maxBound :: UTCDateTimeNanos 'Gregorian) then time
   else error "createUTCDateTimeNanos: out of bounds"
   where UnixDateTime unix = createUnixDateTime year mon mday hour min 0
         time = UTCDateTimeNanos base nsec
         base = baseUnixToUTC unix + fromIntegral sec + extra
         (,) extra nsec = (***) fromIntegral fromIntegral (divMod nanos 1000000000)

-- |
-- Get the current UTC datestamp from the system clock.
getCurrentUTCDate :: IO (UTCDate 'Gregorian)
getCurrentUTCDate = UTCDate <$> getCurrentUnixDate

-- |
-- Get the current UTC timestamp from the system clock.
getCurrentUTCDateTime :: IO (UTCDateTime 'Gregorian)
getCurrentUTCDateTime = do
   UnixDateTime unix <- getCurrentUnixDateTime
   return $! UTCDateTime $ baseUnixToUTC unix

-- |
-- Get the current UTC timestamp with nanosecond granularity from the system clock. Any observed leap second will be spread out over the day to ensure nanosecond continuity at midnight.
getCurrentUTCDateTimeNanos :: IO (UTCDateTimeNanos 'Gregorian)
getCurrentUTCDateTimeNanos = do
   UnixDateTimeNanos unix nsec <- getCurrentUnixDateTimeNanos
   let date = UnixDate (fromIntegral (div base 86400))
       base = baseUnixToUTC unix
       time = UTCDateTimeNanos base nsec
       leap = round (11574.074074074073 * realToFrac (mod unix 86400) :: Double) :: Nanos
   return $! case nextLeap of
      Just ((==) date -> True) -> time `plus` leap
      _ -> time

-- |
-- Parse a UTC datestamp.
parseUTCDate
   :: TimeLocale
   -> Format
   -> Text
   -> Either String (UTCDate 'Gregorian)
parseUTCDate locale format input =
   join $ build <$> runParser locale tzdata defaultParserState format input
   where tzdata = Just utcTZ
         build ParserState {..} = _ps_zone 0 *> do
            return $! createUTCDate _ps_year _ps_mon _ps_mday

-- |
-- Parse a UTC timestamp.
parseUTCDateTime
   :: TimeLocale
   -> Format
   -> Text
   -> Either String (UTCDateTime 'Gregorian)
parseUTCDateTime locale format input =
   join $ build <$> runParser locale tzdata defaultParserState format input
   where tzdata = Just utcTZ
         build ParserState {..} = _ps_zone 0 *> do
            return $! createUTCDateTime _ps_year _ps_mon _ps_mday hour _ps_min sec
            where hour = _ps_ampm _ps_hour
                  sec  = truncate _ps_sec

-- |
-- Parse a UTC timestamp with nanosecond granularity.
parseUTCDateTimeNanos
   :: TimeLocale
   -> Format
   -> Text
   -> Either String (UTCDateTimeNanos 'Gregorian)
parseUTCDateTimeNanos locale format input =
   join $ build <$> runParser locale tzdata defaultParserState format input
   where tzdata = Just utcTZ
         build ParserState {..} = _ps_zone 0 *> do
            return $! createUTCDateTimeNanos _ps_year _ps_mon _ps_mday hour _ps_min sec nsec
            where hour = _ps_ampm _ps_hour
                  (,) sec nsec = properFracNanos $ _ps_frac _ps_sec

-- |
-- Convert an integer into a UTC timestamp with nanosecond granularity.
toNano :: Integer -> UTCDateTimeNanos 'Gregorian
toNano = uncurry UTCDateTimeNanos . (***) fromInteger fromInteger . flip divMod 1000000000

-- |
-- Convert a UTC timestamp with nanosecond granularity into an integer.
fromNano :: UTCDateTimeNanos 'Gregorian -> Integer
fromNano (UTCDateTimeNanos base nsec) = toInteger base * 1000000000 + toInteger nsec

-- |
-- The next leap second insertion date.
nextLeap :: Maybe (UnixDate 'Gregorian)
nextLeap = Just (UnixDate 17166)
