---------------------------------------------------------------
-- Copyright (c) 2013, Enzo Haussecker. All rights reserved. --
---------------------------------------------------------------

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards  #-}
{-# OPTIONS -Wall             #-}
{-# OPTIONS -fno-warn-orphans #-}

module Main where

import Data.Convertible
import Data.Time.Calendar as Calendar
import Data.Time.Clock
import Data.Time.Exts
import Foreign.C.Types
import Test.QuickCheck

instance Arbitrary City where
  arbitrary = choose (minBound, maxBound)

instance Arbitrary TimeZone where
  arbitrary = choose (minBound, maxBound)

instance Arbitrary UnixDate where
  arbitrary = choose (minBound, maxBound)

instance Arbitrary UnixTime where
  arbitrary = choose (minBound, maxBound)

instance Arbitrary UnixTimeMillis where
  arbitrary = choose (minBound, maxBound)

instance Arbitrary UnixTimeMicros where
  arbitrary = choose (minBound, maxBound)

instance Arbitrary UnixTimeNanos where
  arbitrary = choose (minBound, maxBound)

instance Arbitrary UnixTimePicos where
  arbitrary = choose (minBound, maxBound)

instance Arbitrary UnixDateTime where
  arbitrary = choose (minBound, maxBound)

instance Arbitrary UnixDateTimeMillis where
  arbitrary = choose (minBound, maxBound)

instance Arbitrary UnixDateTimeMicros where
  arbitrary = choose (minBound, maxBound)

instance Arbitrary UnixDateTimeNanos where
  arbitrary = choose (minBound, maxBound)

instance Arbitrary UnixDateTimePicos where
  arbitrary = choose (minBound, maxBound)

instance Arbitrary LocalDate where
  arbitrary = choose (minBound, maxBound)

instance Arbitrary LocalDateTime where
  arbitrary = choose (minBound, maxBound)

instance Arbitrary LocalDateTimeMillis where
  arbitrary = choose (minBound, maxBound)

instance Arbitrary LocalDateTimeMicros where
  arbitrary = choose (minBound, maxBound)

instance Arbitrary LocalDateTimeNanos where
  arbitrary = choose (minBound, maxBound)

instance Arbitrary LocalDateTimePicos where
  arbitrary = choose (minBound, maxBound)

-- | Test Unix date and time component equality.
test1 :: (Bounded x, DateTime x, Duration x Second, Show x, Unix x)  => x -> Bool
test1 x
    -- Testing year equality...
  | toInteger (c'tm'tm_year + 1900) /= 
    toInteger (    _dt_year       )  = unequal
    -- Testing month equality...
  | toInteger (     c'tm'tm_mon + 1) /=
    toInteger (fromEnum _dt_mon + 1)  = unequal
    -- Testing day of month equality...
  | toInteger c'tm'tm_mday /=
    toInteger     _dt_mday  = unequal
    -- Testing day of week equality...
  | toInteger (     c'tm'tm_wday) /=
    toInteger (fromEnum _dt_wday)  = unequal
    -- Testing hour equality...
  | toInteger c'tm'tm_hour /=
    toInteger     _dt_hour  = unequal
    -- Testing minute equality...
  | toInteger c'tm'tm_min /=
    toInteger     _dt_min  = unequal
    -- Testing second equality...
  | toInteger c'tm'tm_sec /=
    truncate      _dt_sec  = unequal
    -- Success!
  | otherwise = True
  where Second base = duration minBound x
        DateTimeStruct{..} = toDateTimeStruct x
        C'tm{..} = convert $ CTime base
        unequal = error $ "test1: " ++ show x

-- | Test Unix date struct conversions.
test2 :: (Eq x, Date x, Show x, Unix x) => x -> Bool
test2 x | x == fromDateStruct (toDateStruct x) = True
        | otherwise = error $ "test2: " ++ show x

-- | Test Unix date-time struct conversions.
test3 :: (Eq x, DateTime x, Show x, Unix x) => x -> Bool
test3 x | x == fromDateTimeStruct (toDateTimeStruct x) = True
        | otherwise = error $ "test3: " ++ show x

-- | Test local date struct conversions.
test4 :: (Eq x, DateZone x, Local x, Show x) => x -> Bool
test4 x | x == fromDateZoneStruct (toDateZoneStruct x) = True
        | otherwise = error $ "test4: " ++ show x

-- | Test local date-time struct conversions.
test5 :: (Eq x, DateTimeZone x, Local x, Show x) => x -> Bool
test5 x | x == fromDateTimeZoneStruct (toDateTimeZoneStruct x) = True
        | otherwise = error $ "test5: " ++ show x

-- | Test calendar day conversions.
test6 :: (Convertible x Calendar.Day, Convertible Calendar.Day x, Eq x, Show x, Zone x) => x -> Bool
test6 x | x' == convert (convert x' :: Calendar.Day) = True
        | otherwise = error $ "test6: " ++ show x'
        where x' = x `toTimeZone` utc

-- | Test utc time conversions.
test7 :: (Convertible x UTCTime, Convertible UTCTime x, Eq x, Show x, Zone x) => x -> Bool
test7 x | x' == convert (convert x' :: UTCTime) = True
        | otherwise = error $ "test7: " ++ show x'
        where x' = x `toTimeZone` utc

-- | Test Unix time struct conversions.
test8 :: (Eq x, Time x, Show x, Unix x) => x -> Bool
test8 x | x == fromTimeStruct (toTimeStruct x) = True
        | otherwise = error $ "test8: " ++ show x

-- | Test properties.
main :: IO ()
main = do
  quickCheck (test1 :: UnixDateTime        -> Bool)
  quickCheck (test1 :: UnixDateTimeMillis  -> Bool)
  quickCheck (test1 :: UnixDateTimeMicros  -> Bool)
  quickCheck (test1 :: UnixDateTimeNanos   -> Bool)
  quickCheck (test1 :: UnixDateTimePicos   -> Bool)
  quickCheck (test2 :: UnixDate            -> Bool)
  quickCheck (test3 :: UnixDateTime        -> Bool)
  quickCheck (test3 :: UnixDateTimeMillis  -> Bool)
  quickCheck (test3 :: UnixDateTimeMicros  -> Bool)
  quickCheck (test3 :: UnixDateTimeNanos   -> Bool)
  quickCheck (test3 :: UnixDateTimePicos   -> Bool)
  quickCheck (test4 :: LocalDate           -> Bool)
  quickCheck (test5 :: LocalDateTime       -> Bool)
  quickCheck (test5 :: LocalDateTimeMillis -> Bool)
  quickCheck (test5 :: LocalDateTimeMicros -> Bool)
  quickCheck (test5 :: LocalDateTimeNanos  -> Bool)
  quickCheck (test5 :: LocalDateTimePicos  -> Bool)
  quickCheck (test6 :: LocalDate           -> Bool)
  quickCheck (test7 :: LocalDateTime       -> Bool)
  quickCheck (test7 :: LocalDateTimeMillis -> Bool)
  quickCheck (test7 :: LocalDateTimeMicros -> Bool)
  quickCheck (test7 :: LocalDateTimeNanos  -> Bool)
  quickCheck (test7 :: LocalDateTimePicos  -> Bool)
  quickCheck (test8 :: UnixTime            -> Bool)
  quickCheck (test8 :: UnixTimeMillis      -> Bool)
  quickCheck (test8 :: UnixTimeMicros      -> Bool)
  quickCheck (test8 :: UnixTimeNanos       -> Bool)
  quickCheck (test8 :: UnixTimePicos       -> Bool)
