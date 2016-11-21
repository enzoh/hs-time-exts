{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

{-# OPTIONS -fno-warn-type-defaults #-}
{-# OPTIONS -fno-warn-orphans       #-}

module Main where

import Control.Arrow       (arr)
import Data.Char           (toLower)
import Data.Int            (Int64)
import Data.Time.LocalTime (TimeZone(..), utc)
import Data.Time.Zones     (TZ, loadTZFromDB)
import Numeric.IEEE        (epsilon)
import System.Exit         (ExitCode(..), exitWith)
import System.Locale       (defaultTimeLocale)
import Test.HUnit          (Counts(..), Test(..), assertBool, assertEqual, runTestTT)
import Test.QuickCheck     (Arbitrary(..), choose, sample')
import Text.Printf         (printf)

import qualified Data.Text as Text (pack)

import Data.Time.Exts.Base
import Data.Time.Exts.Format
import Data.Time.Exts.Parser
import Data.Time.Exts.Unix
import Data.Time.Exts.Util
import Foreign.C.Time

instance Arbitrary Year where
   arbitrary = Year <$> choose (1970, 9999)

instance Arbitrary Day where
   arbitrary = Day <$> choose (1, 31)

instance Arbitrary Hour where
   arbitrary = Hour <$> choose (0, 23)

instance Arbitrary Minute where
   arbitrary = Minute <$> choose (0, 59)

instance Arbitrary Second where
   arbitrary = Second <$> choose (0, 59)

instance Arbitrary Millis where
   arbitrary = Millis <$> choose (0, 999)

instance Arbitrary Micros where
   arbitrary = Micros <$> choose (0, 999999)

instance Arbitrary Nanos where
   arbitrary = Nanos <$> choose (0, 999999999)

instance Arbitrary (Month 'Gregorian) where
   arbitrary = toEnum <$> choose (jan, dec)
      where jan = fromEnum (minBound :: DayOfWeek 'Gregorian)
            dec = fromEnum (maxBound :: DayOfWeek 'Gregorian)

instance Arbitrary (DayOfWeek 'Gregorian) where
   arbitrary = toEnum <$> choose (sun, sat)
      where sun = fromEnum (minBound :: DayOfWeek 'Gregorian)
            sat = fromEnum (maxBound :: DayOfWeek 'Gregorian)

instance Arbitrary (UnixDate 'Gregorian) where
   arbitrary = choose (minBound, maxBound)

instance Arbitrary (UnixDateTime 'Gregorian) where
   arbitrary = choose (minBound, maxBound)

instance Arbitrary (UnixDateTimeNanos 'Gregorian) where
   arbitrary = choose (minBound, maxBound)

withArbitraryInput
   :: Arbitrary a
   => (a -> Test)
   -> IO Test
withArbitraryInput f = TestList . map f <$> sample' arbitrary

test_properFracMillis :: Test
test_properFracMillis = TestList
   [
      TestCase . assertEqual "properFracMillis 0.33333333333333" (0, 000000000333) $ properFracMillis 0.33333333333333,
      TestCase . assertEqual "properFracMillis 0.66666666666666" (0, 000000000667) $ properFracMillis 0.66666666666666,
      TestCase . assertEqual "properFracMillis 0.99999999999999" (1, 000000000000) $ properFracMillis 0.99999999999999
   ]

test_properFracMicros :: Test
test_properFracMicros = TestList
   [
      TestCase . assertEqual "properFracMicros 0.33333333333333" (0, 000000333333) $ properFracMicros 0.33333333333333,
      TestCase . assertEqual "properFracMicros 0.66666666666666" (0, 000000666667) $ properFracMicros 0.66666666666666,
      TestCase . assertEqual "properFracMicros 0.99999999999999" (1, 000000000000) $ properFracMicros 0.99999999999999
   ]

test_properFracNanos :: Test
test_properFracNanos = TestList
   [
      TestCase . assertEqual "properFracNanos 0.33333333333333" (0, 000333333333) $ properFracNanos 0.33333333333333,
      TestCase . assertEqual "properFracNanos 0.66666666666666" (0, 000666666667) $ properFracNanos 0.66666666666666,
      TestCase . assertEqual "properFracNanos 0.99999999999999" (1, 000000000000) $ properFracNanos 0.99999999999999
   ]

test_properFracPicos :: Test
test_properFracPicos = TestList
   [
      TestCase . assertEqual "properFracPicos 0.33333333333333" (0, 333333333333) $ properFracPicos 0.33333333333333,
      TestCase . assertEqual "properFracPicos 0.66666666666666" (0, 666666666667) $ properFracPicos 0.66666666666666,
      TestCase . assertEqual "properFracPicos 0.99999999999999" (1, 000000000000) $ properFracPicos 0.99999999999999
   ]

parse
   :: (ParserState 'Gregorian -> Test)
   -> Maybe TZ
   -> Format
   -> String
   -> Test
parse test mtz formet input =
   either error test . runParser defaultTimeLocale mtz state formet $ Text.pack input
   where state = ParserState 1970 January 1 Thursday 0 0 0.0 id id (const (return utc))

deltaZero :: Double -> (Double -> Bool)
deltaZero x = (>) epsilon . abs . (-) x

test_parseYear :: Year -> Test
test_parseYear year =
   parse test Nothing "%Y" input
   where input = printf "%d" year
         err   = printf "test_parseYear: \"%s\"" input
         test  = TestCase . assertEqual err year . _ps_year

test_parseYear' :: Year -> Test
test_parseYear' year =
   parse test Nothing "%y" input
   where year' = mod year 100
         fix x = if x >= 2000 then x - 2000 else x - 1900
         input = printf "%02d" year'
         err   = printf "test_parseYear': \"%s\"" input
         test  = TestCase . assertEqual err year' . fix . _ps_year

test_parseMonth :: Month 'Gregorian -> Test
test_parseMonth month =
   parse test Nothing "%B" input
   where input = show month
         err   = printf "test_parseMonth: \"%s\"" input
         test  = TestCase . assertEqual err month . _ps_mon

test_parseMonth' :: Month 'Gregorian -> Test
test_parseMonth' month =
   parse test Nothing "%b" input
   where input = take 3 $ show month
         err   = printf "test_parseMonth': \"%s\"" input
         test  = TestCase . assertEqual err month . _ps_mon

test_parseMonth'' :: Month 'Gregorian -> Test
test_parseMonth'' month =
   parse test Nothing "%h" input
   where input = take 3 $ show month
         err   = printf "test_parseMonth'': \"%s\"" input
         test  = TestCase . assertEqual err month . _ps_mon

test_parseMonth''' :: Month 'Gregorian -> Test
test_parseMonth''' month =
   parse test Nothing "%m" input
   where input = printf "%02d" $ fromEnum month
         err   = printf "test_parseMonth''': \"%s\"" input
         test  = TestCase . assertEqual err month . _ps_mon

test_parseDay :: Day -> Test
test_parseDay day =
   parse test Nothing "%d" input
   where input = printf "%02d" day
         err   = printf "test_parseDay: \"%s\"" input
         test  = TestCase . assertEqual err day . _ps_mday

test_parseDay' :: Day -> Test
test_parseDay' day =
   parse test Nothing "%e" input
   where input = printf "%2d" day
         err   = printf "test_parseDay': \"%s\"" input
         test  = TestCase . assertEqual err day . _ps_mday

test_parseDayOfWeek :: DayOfWeek 'Gregorian -> Test
test_parseDayOfWeek day =
   parse test Nothing "%A" input
   where input = show day
         err   = printf "test_parseDayOfWeek: \"%s\"" input
         test  = TestCase . assertEqual err day . _ps_wday

test_parseDayOfWeek' :: DayOfWeek 'Gregorian -> Test
test_parseDayOfWeek' day =
   parse test Nothing "%a" input
   where input = take 3 $ show day
         err   = printf "test_parseDayOfWeek': \"%s\"" input
         test  = TestCase . assertEqual err day . _ps_wday

test_parseHour :: Hour -> Test
test_parseHour hour =
   parse test Nothing "%H" input
   where input = printf "%02d" hour
         err   = printf "test_parseHour: \"%s\"" input
         test  = TestCase . assertEqual err hour . _ps_hour

test_parseHour' :: Hour -> Test
test_parseHour' hour =
   parse test Nothing "%I%p" input
   where (,) period hour' = getPeriod hour
         input = printf "%02d%s" hour' period
         err   = printf "test_parseHour': \"%s\"" input
         test  = TestCase . assertEqual err hour . \ ps -> (_ps_ampm ps) (_ps_hour ps)

test_parseHour'' :: Hour -> Test
test_parseHour'' hour =
   parse test Nothing "%l%P" input
   where (,) period hour' = getPeriod hour
         input = printf "%2d%s" hour' (map toLower period)
         err   = printf "test_parseHour'': \"%s\"" input
         test  = TestCase . assertEqual err hour . \ ps -> (_ps_ampm ps) (_ps_hour ps)

test_parseMinute :: Minute -> Test
test_parseMinute minute =
   parse test Nothing "%M" input
   where input = printf "%02d" minute
         err   = printf "test_parseMinute: \"%s\"" input
         test  = TestCase . assertEqual err minute . _ps_min

test_parseSecond :: Second -> Test
test_parseSecond second =
   parse test Nothing "%S" input
   where input = printf "%02d" second
         err   = printf "test_parseSecond: \"%s\"" input
         test  = TestCase . assertEqual err second . truncate . _ps_sec

test_parseMillis :: Millis -> Test
test_parseMillis millis =
   parse test Nothing "%S%f" input
   where frac  = realToFrac millis / 1000 :: Double
         input = printf "%06.3f" frac
         err   = printf "test_parseMillis: \"%s\"" input
         test  = TestCase . assertBool err . deltaZero frac . flip arr 0 . _ps_frac

test_parseMicros :: Micros -> Test
test_parseMicros micros =
   parse test Nothing "%S%f" input
   where frac  = realToFrac micros / 1000000 :: Double
         input = printf "%09.6f" frac
         err   = printf "test_parseMicros: \"%s\"" input
         test  = TestCase . assertBool err . deltaZero frac . flip arr 0 . _ps_frac

test_parseNanos :: Nanos -> Test
test_parseNanos nanos =
   parse test Nothing "%S%f" input
   where frac  = realToFrac nanos / 1000000000 :: Double
         input = printf "%012.9f" frac
         err   = printf "test_parseNanos: \"%s\"" input
         test  = TestCase . assertBool err . deltaZero frac . flip arr 0 . _ps_frac

test_parseZone :: String -> Int64 -> TimeZone -> IO Test
test_parseZone region base tz = do
   tzdata <- Just <$> loadTZFromDB region
   return $! parse test tzdata "%Z" input
   where input = timeZoneName tz
         err   = printf "test_parseZone: \"%s\"" input
         test  = TestCase . assertEqual err (Right tz) . flip arr base . _ps_zone

test_EnumUnixDate :: UnixDate 'Gregorian -> Test
test_EnumUnixDate date =
   TestCase . assertEqual err date . toEnum $ fromEnum date
   where err = "test_EnumUnixDate: " ++ show date

test_EnumUnixDateTime :: UnixDateTime 'Gregorian -> Test
test_EnumUnixDateTime time =
   TestCase . assertEqual err time . toEnum $ fromEnum time
   where err = "test_EnumUnixDateTime: " ++ show time

test_HumanUnixDate :: UnixDate 'Gregorian -> Test
test_HumanUnixDate date =
   TestCase . assertEqual err date . pack $ unpack date
   where err = "test_HumanUnixDate: " ++ show date

test_HumanUnixDateTime :: UnixDateTime 'Gregorian -> Test
test_HumanUnixDateTime time =
   TestCase . assertEqual err time . pack $ unpack time
   where err = "test_HumanUnixDateTime: " ++ show time

test_HumanUnixDateTimeNanos :: UnixDateTimeNanos 'Gregorian -> Test
test_HumanUnixDateTimeNanos time =
   TestCase . assertEqual err time . pack $ unpack time
   where err = "test_HumanUnixDateTimeNanos: " ++ show time

test_HumanCTime :: UnixDateTime 'Gregorian -> Test
test_HumanCTime time =
   TestCase . assertBool err $
      toInteger (c'tm'tm_year + 1900) == toInteger           _dt_year  &&
      toInteger (c'tm'tm_mon  + 0001) == toInteger (fromEnum _dt_mon)  &&
      toInteger  c'tm'tm_mday         == toInteger           _dt_mday  &&
      toInteger (c'tm'tm_wday + 0001) == toInteger (fromEnum _dt_wday) &&
      toInteger  c'tm'tm_hour         == toInteger           _dt_hour  &&
      toInteger  c'tm'tm_min          == toInteger           _dt_min   &&
      toInteger  c'tm'tm_sec          == truncate            _dt_sec
      where ctime :: CTime
            ctime = fromIntegral $ fromEnum time
            C'tm {..} = unpack ctime
            DateTimeStruct {..} = unpack time
            err = "test_HumanCTime: " ++ show time

test_MathUnixDate :: UnixDate 'Gregorian -> Test
test_MathUnixDate date =
   TestCase . assertEqual err minBound $
      date `plus` negate day
      where err = "test_MathUnixDate: " ++ show date
            day = fromIntegral $ fromEnum date :: Day

test_MathUnixDateTime :: UnixDateTime 'Gregorian -> Test
test_MathUnixDateTime time =
   TestCase . assertEqual err minBound $
      time `plus` negate day `plus` negate _dt_hour `plus` negate _dt_min `plus` negate sec
      where DateTimeStruct {..} = unpack time
            date = createUnixDate _dt_year _dt_mon _dt_mday
            err  = "test_MathUnixDateTime: " ++ show time
            day  = fromIntegral $ fromEnum date :: Day
            sec  = round _dt_sec :: Second

test_MathUnixDateTimeNanos :: UnixDateTimeNanos 'Gregorian -> Test
test_MathUnixDateTimeNanos time =
   TestCase . assertEqual err minBound $
      time `plus` negate day `plus` negate _dt_hour `plus` negate _dt_min `plus` negate sec `plus` negate nsec
      where DateTimeStruct {..} = unpack time
            date = createUnixDate _dt_year _dt_mon _dt_mday
            err  = "test_MathUnixDateTime: " ++ show time
            day  = fromIntegral $ fromEnum date :: Day
            (,) sec nsec = properFracNanos _dt_sec

test_ShowUnixDate :: UnixDate 'Gregorian -> Test
test_ShowUnixDate date =
   TestCase . assertEqual err date . either error id .
      parseUnixDate defaultTimeLocale fmt . Text.pack $ show date
      where fmt = "%a %b %d %Y"
            err = "test_ShowUnixDate: " ++ show date

test_ShowUnixDateTime :: UnixDateTime 'Gregorian -> Test
test_ShowUnixDateTime time =
   TestCase . assertEqual err time . either error id .
      parseUnixDateTime defaultTimeLocale fmt . Text.pack $ show time
      where fmt = "%r %a %b %d %Y"
            err = "test_ShowUnixDateTime: " ++ show time

test_ShowUnixDateTimeNanos :: UnixDateTimeNanos 'Gregorian -> Test
test_ShowUnixDateTimeNanos time =
   TestCase . assertEqual err time . either error id .
      parseUnixDateTimeNanos defaultTimeLocale fmt . Text.pack $ show time
      where fmt = "%I:%M:%S%f %p %a %b %d %Y"
            err = "test_ShowUnixDateTimeNanos: " ++ show time

tests :: IO Test
tests = TestList <$> sequence
   [
      return test_properFracMillis,
      return test_properFracMicros,
      return test_properFracNanos,
      return test_properFracPicos,
      withArbitraryInput test_parseYear,
      withArbitraryInput test_parseYear',
      withArbitraryInput test_parseMonth,
      withArbitraryInput test_parseMonth',
      withArbitraryInput test_parseMonth'',
      withArbitraryInput test_parseMonth''',
      withArbitraryInput test_parseDay,
      withArbitraryInput test_parseDay',
      withArbitraryInput test_parseDayOfWeek,
      withArbitraryInput test_parseDayOfWeek',
      withArbitraryInput test_parseHour,
      withArbitraryInput test_parseHour',
      withArbitraryInput test_parseHour'',
      withArbitraryInput test_parseMinute,
      withArbitraryInput test_parseSecond,
      withArbitraryInput test_parseMillis,
      withArbitraryInput test_parseMicros,
      withArbitraryInput test_parseNanos,
      test_parseZone "America/Los_Angeles" 1457834399 (TimeZone (-480) False "PST"),
      test_parseZone "America/Los_Angeles" 1457838000 (TimeZone (-420) True  "PDT"),
      test_parseZone "America/Los_Angeles" 1478397599 (TimeZone (-420) True  "PDT"),
      test_parseZone "America/Los_Angeles" 1478394000 (TimeZone (-480) False "PST"),
      withArbitraryInput test_EnumUnixDate,
      withArbitraryInput test_EnumUnixDateTime,
      withArbitraryInput test_HumanUnixDate,
      withArbitraryInput test_HumanUnixDateTime,
      withArbitraryInput test_HumanUnixDateTimeNanos,
      withArbitraryInput test_HumanCTime,
      withArbitraryInput test_MathUnixDate,
      withArbitraryInput test_MathUnixDateTime,
      withArbitraryInput test_MathUnixDateTimeNanos,
      withArbitraryInput test_ShowUnixDate,
      withArbitraryInput test_ShowUnixDateTime,
      withArbitraryInput test_ShowUnixDateTimeNanos
   ]

main :: IO ()
main = do
   Counts {..} <- tests >>= runTestTT
   exitWith $ case failures + errors of
      0 -> ExitSuccess
      _ -> ExitFailure 1
