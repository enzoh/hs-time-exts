---------------------------------------------------------------
-- Copyright (c) 2014, Enzo Haussecker. All rights reserved. --
---------------------------------------------------------------

{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# OPTIONS -Wall                       #-}

-- | Basic definitions, including type classes, datatypes and functions.
module Data.Time.Exts.Base (

 -- ** Classes
       Date(..)
     , DateTime(..)
     , DateZone(..)
     , DateTimeZone(..)
     , DateTimeMath(..)
     , Duration(..)
     , Zone(..)

 -- ** Structs
     , DateStruct(..)
     , DateTimeStruct(..)
     , DateZoneStruct(..)
     , DateTimeZoneStruct(..)

 -- ** Components
     , Year(..)
     , Month(..)
     , Day(..)
     , DayOfWeek(..)
     , Hour(..)
     , Minute(..)
     , Second(..)
     , Millis(..)
     , Micros(..)
     , Nanos(..)
     , Picos(..)

 -- ** Fractions
     , properFracMillis
     , properFracMicros
     , properFracNanos
     , properFracPicos

 -- ** Durations
     , epochToDate
     , epochToTime

 -- ** Utilities
     , isLeapYear
     , showPeriod
     , showSuffix

     ) where

import Control.Arrow (first)
import Data.Aeson (FromJSON, ToJSON)
import Data.Int (Int32, Int64)
import Data.Time.Exts.Zone (TimeZone)
import Data.Typeable (Typeable)
import GHC.Generics (Generic)
import Text.Printf (PrintfArg)
import System.Random (Random(..))

class Date d where

   -- | Compose a timestamp from date components.
   fromDateStruct :: DateStruct -> d

   -- | Decompose a timestamp into date components.
   toDateStruct :: d -> DateStruct

class Date dt => DateTime dt where

   -- | Compose a timestamp from date and time components.
   fromDateTimeStruct :: DateTimeStruct -> dt

   -- | Decompose a timestamp into date and time components.
   toDateTimeStruct :: dt -> DateTimeStruct

class DateZone dz where

   -- | Compose a timestamp from date and time zone components.
   fromDateZoneStruct :: DateZoneStruct -> dz

   -- | Decompose a timestamp into date and time zone components.
   toDateZoneStruct :: dz -> DateZoneStruct

class DateZone dtz => DateTimeZone dtz where

   -- | Compose a timestamp from date, time and time zone components.
   fromDateTimeZoneStruct :: DateTimeZoneStruct -> dtz

   -- | Decompose a timestamp into date, time and time zone components.
   toDateTimeZoneStruct :: dtz -> DateTimeZoneStruct

class Duration x c where

   -- | Compute the date or time component duration between two timestamps.
   duration :: x -> x -> c

class DateTimeMath x c where

   -- | Add a timestamp with a date or time component.
   plus :: x -> c -> x

class Zone x where

   -- | Change the time zone of a timestamp.
   rezone :: x -> TimeZone -> x

-- | A struct with date components.
data DateStruct = DateStruct {
     _d_year :: {-# UNPACK #-} !Year
   , _d_mon  ::                !Month
   , _d_mday :: {-# UNPACK #-} !Day
   , _d_wday ::                !DayOfWeek
   } deriving (Eq,Generic,Ord,Show,Typeable)

-- | A struct with date and time components.
data DateTimeStruct = DateTimeStruct {
     _dt_year :: {-# UNPACK #-} !Year
   , _dt_mon  ::                !Month
   , _dt_mday :: {-# UNPACK #-} !Day
   , _dt_wday ::                !DayOfWeek
   , _dt_hour :: {-# UNPACK #-} !Hour
   , _dt_min  :: {-# UNPACK #-} !Minute
   , _dt_sec  :: {-# UNPACK #-} !Double
   } deriving (Eq,Generic,Ord,Show,Typeable)

-- | A struct with date and time zone components.
data DateZoneStruct = DateZoneStruct {
     _dz_year :: {-# UNPACK #-} !Year
   , _dz_mon  ::                !Month
   , _dz_mday :: {-# UNPACK #-} !Day
   , _dz_wday ::                !DayOfWeek
   , _dz_zone ::                !TimeZone
   } deriving (Eq,Generic,Ord,Show,Typeable)

-- | A struct with date, time and time zone components.
data DateTimeZoneStruct = DateTimeZoneStruct {
    _dtz_year :: {-# UNPACK #-} !Year
  , _dtz_mon  ::                !Month
  , _dtz_mday :: {-# UNPACK #-} !Day
  , _dtz_wday ::                !DayOfWeek
  , _dtz_hour :: {-# UNPACK #-} !Hour
  , _dtz_min  :: {-# UNPACK #-} !Minute
  , _dtz_sec  :: {-# UNPACK #-} !Double
  , _dtz_zone ::                !TimeZone
  } deriving (Eq,Generic,Ord,Show,Typeable)

-- | Year.
newtype Year = Year {getYear :: Int32}
   deriving (Bounded,Enum,Eq,FromJSON,Generic,Integral,Num,Ord,PrintfArg,Random,Real,ToJSON)

-- | Month.
data Month =
     January
   | February
   | March
   | April
   | May
   | June
   | July
   | August
   | September
   | October
   | November
   | December
   deriving (Eq,Enum,Generic,Ord,Show,Typeable)

-- | Day.
newtype Day = Day {getDay :: Int32}
   deriving (Bounded,Enum,Eq,FromJSON,Generic,Integral,Num,Ord,PrintfArg,Random,Real,ToJSON)

-- | Day of week.
data DayOfWeek =
     Sunday
   | Monday
   | Tuesday
   | Wednesday
   | Thursday
   | Friday
   | Saturday
   deriving (Eq,Enum,Generic,Ord,Show,Typeable)

-- | Hour.
newtype Hour = Hour {getHour :: Int64}
   deriving (Bounded,Enum,Eq,FromJSON,Generic,Integral,Num,Ord,PrintfArg,Random,Real,ToJSON)

-- | Minute.
newtype Minute = Minute {getMinute :: Int64}
   deriving (Bounded,Enum,Eq,FromJSON,Generic,Integral,Num,Ord,PrintfArg,Random,Real,ToJSON)

-- | Second.
newtype Second = Second {getSecond :: Int64}
   deriving (Bounded,Enum,Eq,FromJSON,Generic,Integral,Num,Ord,PrintfArg,Random,Real,ToJSON)

-- | Millisecond.
newtype Millis = Millis {getMillis :: Int64}
   deriving (Bounded,Enum,Eq,FromJSON,Generic,Integral,Num,Ord,PrintfArg,Random,Real,ToJSON)

-- | Microsecond.
newtype Micros = Micros {getMicros :: Int64}
   deriving (Bounded,Enum,Eq,FromJSON,Generic,Integral,Num,Ord,PrintfArg,Random,Real,ToJSON)

-- | Nanosecond.
newtype Nanos = Nanos {getNanos :: Int64}
   deriving (Bounded,Enum,Eq,FromJSON,Generic,Integral,Num,Ord,PrintfArg,Random,Real,ToJSON)

-- | Picosecond.
newtype Picos = Picos {getPicos :: Int64}
   deriving (Bounded,Enum,Eq,FromJSON,Generic,Integral,Num,Ord,PrintfArg,Random,Real,ToJSON)

instance FromJSON DateStruct
instance FromJSON DateTimeStruct
instance FromJSON DateZoneStruct
instance FromJSON DateTimeZoneStruct
instance FromJSON DayOfWeek
instance FromJSON Month

instance Random Month where
  random        = first toEnum . randomR (0, 11)
  randomR (a,b) = first toEnum . randomR (fromEnum a, fromEnum b)

instance Random DayOfWeek where
  random        = first toEnum . randomR (0, 6)
  randomR (a,b) = first toEnum . randomR (fromEnum a, fromEnum b)

instance Show Year   where show Year   {getYear  } = show getYear
instance Show Day    where show Day    {getDay   } = show getDay
instance Show Hour   where show Hour   {getHour  } = show getHour
instance Show Minute where show Minute {getMinute} = show getMinute
instance Show Second where show Second {getSecond} = show getSecond
instance Show Millis where show Millis {getMillis} = show getMillis
instance Show Micros where show Micros {getMicros} = show getMicros
instance Show Nanos  where show Nanos  {getNanos } = show getNanos
instance Show Picos  where show Picos  {getPicos } = show getPicos

instance ToJSON DateStruct
instance ToJSON DateTimeStruct
instance ToJSON DateZoneStruct
instance ToJSON DateTimeZoneStruct
instance ToJSON DayOfWeek
instance ToJSON Month

-- | Decompose a floating point number into second and millisecond components.
properFracMillis :: Floating a => RealFrac a => a -> (Second, Millis)
properFracMillis millis = if res == 1000 then (sec + 1, 0) else result
  where result@(sec, res) = fmap (round . (*) 1000) $ properFraction millis

-- | Decompose a floating point number into second and microsecond components.
properFracMicros :: Floating a => RealFrac a => a -> (Second, Micros)
properFracMicros micros = if res == 1000000 then (sec + 1, 0) else result
  where result@(sec, res) = fmap (round . (*) 1000000) $ properFraction micros

-- | Decompose a floating point number into second and nanosecond components.
properFracNanos :: Floating a => RealFrac a => a -> (Second, Nanos)
properFracNanos nanos = if res == 1000000000 then (sec + 1, 0) else result
  where result@(sec, res) = fmap (round . (*) 1000000000) $ properFraction nanos

-- | Decompose a floating point number into second and picosecond components.
properFracPicos :: Floating a => RealFrac a => a -> (Second, Picos)
properFracPicos picos = if res == 1000000000000 then (sec + 1, 0) else result
  where result@(sec, res) = fmap (round . (*) 1000000000000) $ properFraction picos

-- | Calculate the number of days that have
--   elapsed between Unix epoch and the given date.
epochToDate :: Year -> Month -> Day -> Day
epochToDate year month day =
  epochToYear year + yearToMonth month leap + day - 1
  where leap = isLeapYear year

-- | Calculate the number of days that have
--   elapsed between Unix epoch and the given year.
epochToYear :: Year -> Day
epochToYear (Year year) =
  Day $ (year - 1970)   *   365 + (year - 1969) `div` 004 -
        (year - 1901) `div` 100 + (year - 1601) `div` 400

-- | Calculate the number of days that have
--   elapsed between January 1st and the given month.
yearToMonth :: Month -> Bool -> Day
yearToMonth month leap =
  if leap
  then
    case month of
      January   -> 000; February -> 031; March    -> 060; April    -> 091
      May       -> 121; June     -> 152; July     -> 182; August   -> 213
      September -> 244; October  -> 274; November -> 305; December -> 335
  else
    case month of
      January   -> 000; February -> 031; March    -> 059; April    -> 090
      May       -> 120; June     -> 151; July     -> 181; August   -> 212
      September -> 243; October  -> 273; November -> 304; December -> 334

-- | Calculate the number of seconds (excluding leap seconds)
--   that have elapsed between Unix epoch and the given time.
epochToTime :: Year -> Month -> Day -> Hour -> Minute -> Second -> Second
epochToTime year month day (Hour hour) (Minute minute) (Second second) =
  Second $ (days * 86400) + (hour * 3600) + (minute * 60) + second
  where days = fromIntegral $ epochToDate year month day

-- | Check if the given year is a leap year.
isLeapYear :: Year -> Bool
isLeapYear year = year `mod` 400 == 0 || (year `mod` 100 /= 0 && year `mod` 4 == 0)

-- | Show the pariod (ante or post meridiem) of the given hour.
showPeriod :: Hour -> String
showPeriod hour = if hour < 12 then "AM" else "PM"

-- | Show the suffix of the given day of the month.
showSuffix :: Day -> String
showSuffix (Day day) =
  if day < 1 || 31 < day
  then error $ "showSuffix: unknown day of month"
  else case day `mod` 10 of
        1 | day /= 11 -> "st"
        2 | day /= 12 -> "nd"
        3 | day /= 13 -> "rd"
        _             -> "th"
