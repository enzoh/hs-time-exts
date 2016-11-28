-- |
-- Module     : Data.Time.Exts.Base
-- Copyright  : 2013-2016 Enzo Haussecker
-- License    : BSD3
-- Maintainer : Enzo Haussecker <enzo@sovereign.io>
-- Stability  : Stable
--
-- Basic definitions, including type classes, data types and type families.

{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}

module Data.Time.Exts.Base (

  -- * Classes
       Human(..)
     , Math(..)

  -- * Chronologies
     , Calendar(..)
     , Epoch(..)
     , Era

  -- * Components
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

  -- * Structs
     , DateStruct(..)
     , TimeStruct(..)
     , DateTimeStruct(..)
     , LocalDateStruct(..)
     , LocalTimeStruct(..)
     , LocalDateTimeStruct(..)

  -- * Fractions
     , properFracMillis
     , properFracMicros
     , properFracNanos
     , properFracPicos

     ) where

import Control.DeepSeq (NFData(..))
import Data.Data       (Data, Typeable)
import Data.Int        (Int32, Int64)
import Data.Time       (TimeZone)
import GHC.Generics    (Generic)
import Text.Printf     (PrintfArg)

class Human x where

   -- |
   -- Define the human-readable components of a timestamp.
   type Components x :: *

   -- |
   -- Pack a timestamp from human-readable components.
   pack :: Components x -> x

   -- |
   -- Unpack a timestamp to human-readable components.
   unpack :: x -> Components x

class Math x c where

   -- |
   -- Calculate the duration between two timestamps.
   duration :: x -> x -> c

   -- |
   -- Add a duration to a timestamp.
   plus :: x -> c -> x

-- |
-- System for organizing dates.
data Calendar =
     Chinese
   | Gregorian
   | Hebrew
   | Islamic
   | Julian
   deriving (Bounded, Data, Enum, Eq, Generic, Ord, Read, Show, Typeable)

instance NFData Calendar

-- |
-- System origin.
data Epoch =
     Unix
   deriving (Bounded, Data, Enum, Eq, Generic, Ord, Read, Show, Typeable)

instance NFData Epoch

-- |
-- System for numbering years.
data family Era (cal :: Calendar) :: *

data instance Era 'Gregorian =
     BeforeChrist
   | AnnoDomini
   deriving (Bounded, Data, Enum, Eq, Generic, Ord, Read, Show, Typeable)

instance NFData (Era 'Gregorian)

-- |
-- Year.
newtype Year = Year {getYear :: Int32}
   deriving (Bounded, Data, Enum, Eq, Generic, Integral, NFData, Num, Ord, PrintfArg, Read, Real, Typeable)

instance Show Year where
   show Year {..} = show getYear

-- |
-- Month.
data family Month (cal :: Calendar) :: *

data instance Month 'Gregorian =
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
   deriving (Bounded, Data, Eq, Generic, Ord, Read, Show, Typeable)

instance Enum (Month 'Gregorian) where

   fromEnum January   = 01
   fromEnum February  = 02
   fromEnum March     = 03
   fromEnum April     = 04
   fromEnum May       = 05
   fromEnum June      = 06
   fromEnum July      = 07
   fromEnum August    = 08
   fromEnum September = 09
   fromEnum October   = 10
   fromEnum November  = 11
   fromEnum December  = 12

   toEnum 01 = January
   toEnum 02 = February
   toEnum 03 = March
   toEnum 04 = April
   toEnum 05 = May
   toEnum 06 = June
   toEnum 07 = July
   toEnum 08 = August
   toEnum 09 = September
   toEnum 10 = October
   toEnum 11 = November
   toEnum 12 = December
   toEnum __ = error "toEnum{Month 'Gregorian}: out of range"

instance NFData (Month 'Gregorian)

-- |
-- Day.
newtype Day = Day {getDay :: Int32}
   deriving (Bounded, Data, Enum, Eq, Generic, Integral, NFData, Num, Ord, PrintfArg, Read, Real, Typeable)

instance Show Day where
   show Day {..} = show getDay

-- |
-- Day of week.
data family DayOfWeek (cal :: Calendar) :: *

data instance DayOfWeek 'Gregorian =
     Sunday
   | Monday
   | Tuesday
   | Wednesday
   | Thursday
   | Friday
   | Saturday
   deriving (Bounded, Data, Eq, Generic, Ord, Read, Show, Typeable)

instance Enum (DayOfWeek 'Gregorian) where

   fromEnum Sunday    = 1
   fromEnum Monday    = 2
   fromEnum Tuesday   = 3
   fromEnum Wednesday = 4
   fromEnum Thursday  = 5
   fromEnum Friday    = 6
   fromEnum Saturday  = 7

   toEnum 1 = Sunday
   toEnum 2 = Monday
   toEnum 3 = Tuesday
   toEnum 4 = Wednesday
   toEnum 5 = Thursday
   toEnum 6 = Friday
   toEnum 7 = Saturday
   toEnum _ = error "toEnum{DayOfWeek 'Gregorian}: out of range"

instance NFData (DayOfWeek 'Gregorian)

-- |
-- Hour.
newtype Hour = Hour {getHour :: Int64}
   deriving (Bounded, Data, Enum, Eq, Generic, Integral, NFData, Num, Ord, PrintfArg, Read, Real, Typeable)

instance Show Hour where
   show Hour {..} = show getHour

-- |
-- Minute.
newtype Minute = Minute {getMinute :: Int64}
   deriving (Bounded, Data, Enum, Eq, Generic, Integral, NFData, Num, Ord, PrintfArg, Read, Real, Typeable)

instance Show Minute where
   show Minute {..} = show getMinute

-- |
-- Second.
newtype Second = Second {getSecond :: Int64}
   deriving (Bounded, Data, Enum, Eq, Generic, Integral, NFData, Num, Ord, PrintfArg, Read, Real, Typeable)

instance Show Second where
   show Second {..} = show getSecond

-- |
-- Millisecond.
newtype Millis = Millis {getMillis :: Int64}
   deriving (Bounded, Data, Enum, Eq, Generic, Integral, NFData, Num, Ord, PrintfArg, Read, Real, Typeable)

instance Show Millis where
   show Millis {..} = show getMillis

-- |
-- Microsecond.
newtype Micros = Micros {getMicros :: Int64}
   deriving (Bounded, Data, Enum, Eq, Generic, Integral, NFData, Num, Ord, PrintfArg, Read, Real, Typeable)

instance Show Micros where
   show Micros {..} = show getMicros

-- |
-- Nanosecond.
newtype Nanos = Nanos {getNanos :: Int64}
   deriving (Bounded, Data, Enum, Eq, Generic, Integral, NFData, Num, Ord, PrintfArg, Read, Real, Typeable)

instance Show Nanos where
   show Nanos {..} = show getNanos

-- |
-- Picosecond.
newtype Picos = Picos {getPicos :: Int64}
   deriving (Bounded, Data, Enum, Eq, Generic, Integral, NFData, Num, Ord, PrintfArg, Read, Real, Typeable)

instance Show Picos where
   show Picos {..} = show getPicos

-- |
-- A struct with date components.
data DateStruct (cal :: Calendar) =
     DateStruct
     { _d_year :: {-# UNPACK #-} !Year
     , _d_mon  ::                !(Month cal)
     , _d_mday :: {-# UNPACK #-} !Day
     , _d_wday ::                !(DayOfWeek cal)
     } deriving (Generic, Typeable)

-- |
-- A struct with time components.
data TimeStruct =
     TimeStruct
     { _t_hour :: {-# UNPACK #-} !Hour
     , _t_min  :: {-# UNPACK #-} !Minute
     , _t_sec  :: {-# UNPACK #-} !Double
     } deriving (Data, Eq, Generic, Show, Typeable)

-- |
-- A struct with date and time components.
data DateTimeStruct (cal :: Calendar) =
     DateTimeStruct
     { _dt_year :: {-# UNPACK #-} !Year
     , _dt_mon  ::                !(Month cal)
     , _dt_mday :: {-# UNPACK #-} !Day
     , _dt_wday ::                !(DayOfWeek cal)
     , _dt_hour :: {-# UNPACK #-} !Hour
     , _dt_min  :: {-# UNPACK #-} !Minute
     , _dt_sec  :: {-# UNPACK #-} !Double
     } deriving (Generic, Typeable)

-- |
-- A struct with date and time zone components.
data LocalDateStruct (cal :: Calendar) =
     LocalDateStruct
     { _ld_year :: {-# UNPACK #-} !Year
     , _ld_mon  ::                !(Month cal)
     , _ld_mday :: {-# UNPACK #-} !Day
     , _ld_wday ::                !(DayOfWeek cal)
     , _ld_zone :: {-# UNPACK #-} !TimeZone
     } deriving (Generic, Typeable)

-- |
-- A struct with time and time zone components.
data LocalTimeStruct =
     LocalTimeStruct
     { _lt_hour :: {-# UNPACK #-} !Hour
     , _lt_min  :: {-# UNPACK #-} !Minute
     , _lt_sec  :: {-# UNPACK #-} !Double
     , _lt_zone :: {-# UNPACK #-} !TimeZone
     } deriving (Data, Eq, Generic, Show, Typeable)

-- |
-- A struct with date, time, and time zone components.
data LocalDateTimeStruct (cal :: Calendar) =
     LocalDateTimeStruct
     { _ldt_year :: {-# UNPACK #-} !Year
     , _ldt_mon  ::                !(Month cal)
     , _ldt_mday :: {-# UNPACK #-} !Day
     , _ldt_wday ::                !(DayOfWeek cal)
     , _ldt_hour :: {-# UNPACK #-} !Hour
     , _ldt_min  :: {-# UNPACK #-} !Minute
     , _ldt_sec  :: {-# UNPACK #-} !Double
     , _ldt_zone :: {-# UNPACK #-} !TimeZone
     } deriving (Generic, Typeable)

deriving instance (Data (Month cal), Data (DayOfWeek cal), Typeable cal) => Data (DateStruct cal)
deriving instance (Data (Month cal), Data (DayOfWeek cal), Typeable cal) => Data (DateTimeStruct cal)
deriving instance (Data (Month cal), Data (DayOfWeek cal), Typeable cal) => Data (LocalDateStruct cal)
deriving instance (Data (Month cal), Data (DayOfWeek cal), Typeable cal) => Data (LocalDateTimeStruct cal)

deriving instance (Eq (Month cal), Eq (DayOfWeek cal)) => Eq (DateStruct cal)
deriving instance (Eq (Month cal), Eq (DayOfWeek cal)) => Eq (DateTimeStruct cal)
deriving instance (Eq (Month cal), Eq (DayOfWeek cal)) => Eq (LocalDateStruct cal)
deriving instance (Eq (Month cal), Eq (DayOfWeek cal)) => Eq (LocalDateTimeStruct cal)

deriving instance (Show (Month cal), Show (DayOfWeek cal)) => Show (DateStruct cal)
deriving instance (Show (Month cal), Show (DayOfWeek cal)) => Show (DateTimeStruct cal)
deriving instance (Show (Month cal), Show (DayOfWeek cal)) => Show (LocalDateStruct cal)
deriving instance (Show (Month cal), Show (DayOfWeek cal)) => Show (LocalDateTimeStruct cal)

instance (NFData (Month cal), NFData (DayOfWeek cal)) => NFData (DateStruct cal)
instance (NFData (Month cal), NFData (DayOfWeek cal)) => NFData (DateTimeStruct cal)
instance (NFData (Month cal), NFData (DayOfWeek cal)) => NFData (LocalDateStruct cal)
instance (NFData (Month cal), NFData (DayOfWeek cal)) => NFData (LocalDateTimeStruct cal)

instance NFData TimeStruct
instance NFData LocalTimeStruct

-- |
-- Decompose a floating point number into second and millisecond components.
properFracMillis :: RealFrac a => a -> (Second, Millis)
{-# SPECIALISE properFracMillis :: Double -> (Second, Millis) #-}
properFracMillis frac = if millis == 1000 then (sec + 1, 0) else res
   where res@(sec, millis) = fmap (round . (*) 1000) $ properFraction frac

-- |
-- Decompose a floating point number into second and microsecond components.
properFracMicros :: RealFrac a => a -> (Second, Micros)
{-# SPECIALISE properFracMicros :: Double -> (Second, Micros) #-}
properFracMicros frac = if micros == 1000000 then (sec + 1, 0) else res
   where res@(sec, micros) = fmap (round . (*) 1000000) $ properFraction frac

-- |
-- Decompose a floating point number into second and nanosecond components.
properFracNanos :: RealFrac a => a -> (Second, Nanos)
{-# SPECIALISE properFracNanos :: Double -> (Second, Nanos) #-}
properFracNanos frac = if nanos == 1000000000 then (sec + 1, 0) else res
   where res@(sec, nanos) = fmap (round . (*) 1000000000) $ properFraction frac

-- |
-- Decompose a floating point number into second and picosecond components.
properFracPicos :: RealFrac a => a -> (Second, Picos)
{-# SPECIALISE properFracPicos :: Double -> (Second, Picos) #-}
properFracPicos frac = if picos == 1000000000000 then (sec + 1, 0) else res
   where res@(sec, picos) = fmap (round . (*) 1000000000000) $ properFraction frac
