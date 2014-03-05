--------------------------------------------------------------------------------
-- Copyright (c) 2014, Enzo Haussecker, Steve Severance. All rights reserved. --
--------------------------------------------------------------------------------

{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE TypeOperators      #-}
{-# OPTIONS -Wall               #-}

-- | Timestamp parsers and related utilities.
module Data.Time.Exts.Parser (

 -- ** Utilities
       FormatText
     , ParseError(..)

 -- ** Parse Unix Timestamps
     , parseUnixDate
     , parseUnixTime
     , parseUnixTimeMillis
     , parseUnixTimeMicros
     , parseUnixTimeNanos
     , parseUnixTimePicos
     , parseUnixDateTime
     , parseUnixDateTimeMillis
     , parseUnixDateTimeMicros
     , parseUnixDateTimeNanos
     , parseUnixDateTimePicos

 -- ** Parse UTC and Local Timestamps
     , parseLocalDate
     , parseLocalDateTime
     , parseLocalDateTimeMillis
     , parseLocalDateTimeMicros
     , parseLocalDateTimeNanos
     , parseLocalDateTimePicos

     ) where

import Control.Applicative              ((<|>), (<$>), (*>))
import Control.Arrow                    ((***))
import Control.Exception                (Exception)
import Control.Monad
import Control.Monad.State.Strict       (execState, State)
import Data.Attoparsec.Text as P hiding (decimal)
import Data.Convertible                 (Convertible(..), prettyConvertError)
import Data.Char                        (isAlpha)
import Data.Label                       ((:->), mkLabels)
import Data.Label.Monadic               (puts, modify)
import Data.List as L                   (foldl', foldl1, map, zip)
import Data.String                      (IsString(..))
import Data.Text as T
import Data.Time.Exts.Base       hiding (TimeZone)
import Data.Time.Exts.Local
import Data.Time.Exts.Unix
import Data.Time.Exts.Zone
import Data.Typeable                    (Typeable)
import System.Locale                    (TimeLocale(..))

-- | The format string is composed of various %-codes, each
--   representing time-related information described below.
--
-- [@%%@] % literal
--
-- [@%A@] day of week, long form, case-insensitive, Sunday - Saturday
--
-- [@%a@] day of week, short form, case-insensitive, Sun - Sat
--
-- [@%B@] month of year, long form, case-insensitive, January - December
--
-- [@%b@] month of year, short form, case-insensitive, Jan - Dec
--
-- [@%D@] same as %m\/%d\/%y
--
-- [@%d@] day of month, 0-padded to two digits, 01 - 31
--
-- [@%e@] day of month, space-padded to two digits, 1 - 31
--
-- [@%F@] same as %Y-%m-%d
--
-- [@%H@] hour of day (24-hour), 0-padded to two digits, 00 - 23
--
-- [@%h@] month of year, short form, case-insensitive, Jan - Dec
--
-- [@%I@] hour of day (12-hour), 0-padded to two digits, 01 - 12
--
-- [@%l@] hour of day (12-hour), pace-padded to two digits, 1 - 12
--
-- [@%M@] minute of hour, 0-padded to two digits, 00 - 59
--
-- [@%m@] month of year, 0-padded to two digits, 01 - 12
--
-- [@%P@] period of day, case-insensitive, am, pm
--
-- [@%p@] period of day, case-insensitive, AM, PM
--
-- [@%Q@] fraction of second, decimal point followed by zero to twelve digits, . - .999999999999
--
-- [@%R@] same as %H:%M
--
-- [@%r@] same as %I:%M:%S %p
--
-- [@%S@] second of minute, 0-padded to two digits, 00 - 60
--
-- [@%T@] same as %H:%M:%S
--
-- [@%X@] same as %H:%M:%S
--
-- [@%x@] same as %m\/%d\/%y
--
-- [@%Y@] year of century, four digits, 1970 - 9999
--
-- [@%y@] year of century, 0-padded to two digits, 00 - 99
--
-- [@%Z@] time zone abbreviation
type FormatText = Text

-- | Error handling type.
newtype ParseError = ParseError String deriving (Show,Typeable)

instance Exception ParseError

instance IsString ParseError where
  fromString = ParseError

mkLabels [''DateTimeZoneStruct]

-- | Parse a Unix date.
--
-- > >>> parseUnixDate "%A, %B %e, %Y" "Tuesday, March 4, 2014"
-- > Right 2014-03-04
--
parseUnixDate :: TimeLocale -> FormatText -> Text -> Either ParseError UnixDate
parseUnixDate locale format text = fun <$> parseDateTimeZoneStruct locale Universal format text
  where fun DateTimeZoneStruct{..} = createUnixDate _dtz_year _dtz_mon _dtz_mday

-- | Parse a Unix time.
--
-- > >>> parseUnixTime "%l:%M %p" "2:28 PM"
-- > Right 14:28:00
--
parseUnixTime :: TimeLocale -> FormatText -> Text -> Either ParseError UnixTime
parseUnixTime locale format text = fun <$> parseDateTimeZoneStruct locale Universal format text
  where fun DateTimeZoneStruct{..} = createUnixTime _dtz_hour _dtz_min $ truncate _dtz_sec

-- | Parse a Unix time with millisecond granularity.
--
-- > >>> parseUnixTimeMillis "%I:%M:%S%Q %p" "09:41:09.313 PM"
-- > Right 21:41:09.313
--
parseUnixTimeMillis :: TimeLocale -> FormatText -> Text -> Either ParseError UnixTimeMillis
parseUnixTimeMillis locale format text = fun <$> parseDateTimeZoneStruct locale Universal format text
  where fun DateTimeZoneStruct{..} =
          let (sec, mil) = properFracMillis _dtz_sec
          in createUnixTimeMillis _dtz_hour _dtz_min sec mil

-- | Parse a Unix time with microsecond granularity.
--
-- > >>> parseUnixTimeMicros "%R:%S%Q" "03:15:50.513439"
-- > Right 03:15:50.513439
--
parseUnixTimeMicros :: TimeLocale -> FormatText -> Text -> Either ParseError UnixTimeMicros
parseUnixTimeMicros locale format text = fun <$> parseDateTimeZoneStruct locale Universal format text
  where fun DateTimeZoneStruct{..} =
          let (sec, mic) = properFracMicros _dtz_sec
          in createUnixTimeMicros _dtz_hour _dtz_min sec mic

-- | Parse a Unix time with nanosecond granularity.
--
-- > >>> parseUnixTimeNanos "%X%Q" "23:34:39.734563023"
-- > Right 23:34:39.734563023
--
parseUnixTimeNanos :: TimeLocale -> FormatText -> Text -> Either ParseError UnixTimeNanos
parseUnixTimeNanos locale format text = fun <$> parseDateTimeZoneStruct locale Universal format text
  where fun DateTimeZoneStruct{..} =
          let (sec, nan) = properFracNanos _dtz_sec
          in createUnixTimeNanos _dtz_hour _dtz_min sec nan

-- | Parse a Unix time with picosecond granularity.
--
-- > >>> parseUnixTimePicos "%T%Q" "02:28:56.621236981055"
-- > Right 02:28:56.621236981055
--
parseUnixTimePicos :: TimeLocale -> FormatText -> Text -> Either ParseError UnixTimePicos
parseUnixTimePicos locale format text = fun <$> parseDateTimeZoneStruct locale Universal format text
  where fun DateTimeZoneStruct{..} =
          let (sec, pic) = properFracPicos _dtz_sec
          in createUnixTimePicos _dtz_hour _dtz_min sec pic

-- | Parse a Unix date and time.
--
-- > >>> parseUnixDateTime "%FT%TZ" "2014-02-27T11:31:20Z"
-- > Right 2014-02-27 11:31:20
--
parseUnixDateTime :: TimeLocale -> FormatText -> Text -> Either ParseError UnixDateTime
parseUnixDateTime locale format text = fun <$> parseDateTimeZoneStruct locale Universal format text
  where fun DateTimeZoneStruct{..} =
          let sec = truncate _dtz_sec
          in createUnixDateTime _dtz_year _dtz_mon _dtz_mday _dtz_hour _dtz_min sec

-- | Parse a Unix date and time with millisecond granularity.
--
-- > >>> parseUnixDateTimeMillis "%a %B %e %T%Q %p %Y" "Wed March 5 06:53:04.475 PM 2014"
-- > Right 2014-03-05 18:53:04.475
--
parseUnixDateTimeMillis :: TimeLocale -> FormatText -> Text -> Either ParseError UnixDateTimeMillis
parseUnixDateTimeMillis locale format text = fun <$> parseDateTimeZoneStruct locale Universal format text
  where fun DateTimeZoneStruct{..} =
          let (sec, mil) = properFracMillis _dtz_sec
          in createUnixDateTimeMillis _dtz_year _dtz_mon _dtz_mday _dtz_hour _dtz_min sec mil

-- | Parse a Unix date and time with microsecond granularity.
--
-- > >>> parseUnixDateTimeMicros "%D %X%Q" "03/06/14 17:26:55.148415"
-- > Right 2014-03-06 17:26:55.148415
--
parseUnixDateTimeMicros :: TimeLocale -> FormatText -> Text -> Either ParseError UnixDateTimeMicros
parseUnixDateTimeMicros locale format text = fun <$> parseDateTimeZoneStruct locale Universal format text
  where fun DateTimeZoneStruct{..} =
          let (sec, mic) = properFracMicros _dtz_sec
          in createUnixDateTimeMicros _dtz_year _dtz_mon _dtz_mday _dtz_hour _dtz_min sec mic

-- | Parse a Unix date and time with nanosecond granularity.
--
-- > >>> parseUnixDateTimeNanos "%d.%m.%Y %T%Q" "18.03.2014 19:06:43.774295132"
-- > Right 2014-03-18 19:06:43.774295132
--
parseUnixDateTimeNanos :: TimeLocale -> FormatText -> Text -> Either ParseError UnixDateTimeNanos
parseUnixDateTimeNanos locale format text = fun <$> parseDateTimeZoneStruct locale Universal format text
  where fun DateTimeZoneStruct{..} =
          let (sec, nan) = properFracNanos _dtz_sec
          in createUnixDateTimeNanos _dtz_year _dtz_mon _dtz_mday _dtz_hour _dtz_min sec nan

-- | Parse a Unix date and time with picosecond granularity.
--
-- > >>> parseUnixDateTimePicos "%FT%T%QZ" "2014-03-03T17:58:15.916795765305Z"
-- > Right 2014-03-03 17:58:15.916795765305
--
parseUnixDateTimePicos :: TimeLocale -> FormatText -> Text -> Either ParseError UnixDateTimePicos
parseUnixDateTimePicos locale format text = fun <$> parseDateTimeZoneStruct locale Universal format text
  where fun DateTimeZoneStruct{..} =
          let (sec, pic) = properFracPicos _dtz_sec
          in createUnixDateTimePicos _dtz_year _dtz_mon _dtz_mday _dtz_hour _dtz_min sec pic

-- | Parse a local date.
--
-- > >>> parseLocalDate Los_Angeles "%A, %B %e, %Y (%Z)" "Monday, March 17, 2014 (PST)"
-- > Right 2014-03-17 PST
--
parseLocalDate :: TimeLocale -> City -> FormatText -> Text -> Either ParseError LocalDate
parseLocalDate locale city format text = fun <$> parseDateTimeZoneStruct locale city format text
  where fun DateTimeZoneStruct{..} = createLocalDate _dtz_year _dtz_mon _dtz_mday _dtz_zone

-- | Parse a local date and time.
--
-- > >>> parseLocalDateTime New_York "%a %b %e %H:%M:%S %Z %Y" "Fri Mar 14 09:29:53 EST 2014"
-- > Right 2014-03-14 09:29:53 EST
--
parseLocalDateTime :: TimeLocale -> City -> FormatText -> Text -> Either ParseError LocalDateTime
parseLocalDateTime locale city format text = fromDateTimeZoneStruct <$> parseDateTimeZoneStruct locale city format text

-- | Parse a local date and time with millisecond granularity.
--
-- > >>> parseLocalDateTimeMillis Tel_Aviv "%B %e %Y %I:%M:%S%Q %p %Z" "July 1 2012 01:59:60.215 AM IST"
-- > Right 2012-07-01 01:59:60.215 IST
--
--   Note that the timestamp in the example above corresponds to a leap second.
parseLocalDateTimeMillis :: TimeLocale -> City -> FormatText -> Text -> Either ParseError LocalDateTimeMillis
parseLocalDateTimeMillis locale city format text = fromDateTimeZoneStruct <$> parseDateTimeZoneStruct locale city format text

-- | Parse a local date and time with microsecond granularity.
--
-- > >>> parseLocalDateTimeMicros Hong_Kong "%F %T%Q (%Z)" "2014-03-04 02:45:42.827495 (HKT)"
-- > Right 2014-03-04 02:45:42.827495 HKT
--
parseLocalDateTimeMicros :: TimeLocale -> City -> FormatText -> Text -> Either ParseError LocalDateTimeMicros
parseLocalDateTimeMicros locale city format text = fromDateTimeZoneStruct <$> parseDateTimeZoneStruct locale city format text

-- | Parse a local date and time with nanosecond granularity.
--
-- > >>> parseLocalDateTimeNanos Universal "%b. %d, %T%Q %Z %Y" "Mar. 09, 18:53:55.856423459 UTC 2014"
-- > Right 2014-03-09 18:53:55.856423459 UTC
--
parseLocalDateTimeNanos :: TimeLocale -> City -> FormatText -> Text -> Either ParseError LocalDateTimeNanos
parseLocalDateTimeNanos locale city format text = fromDateTimeZoneStruct <$> parseDateTimeZoneStruct locale city format text

-- | Parse a local date and time with picosecond granularity.
--
-- > >>> parseLocalDateTimePicos Singapore "%d.%m.%Y %T%Q %Z" "09.04.2014 05:22:56.587234905781 SGT"
-- > Right 2014-04-09 05:22:56.587234905781 SGT
--
parseLocalDateTimePicos :: TimeLocale -> City -> FormatText -> Text -> Either ParseError LocalDateTimePicos
parseLocalDateTimePicos locale city format text = fromDateTimeZoneStruct <$> parseDateTimeZoneStruct locale city format text

-- | Initialize date, time, and time zone components.
initStruct :: DateTimeZoneStruct
initStruct =  DateTimeZoneStruct 1970 January 1 Thursday 0 0 0.0 utc

-- | Parse date, time, and time zone components.
parseDateTimeZoneStruct
  :: TimeLocale
  -> City
  -> FormatText
  -> Text
  -> Either ParseError DateTimeZoneStruct
parseDateTimeZoneStruct locale city format text =
  either left Right $ do
    parser <- parseFormat locale city format
    parseOnly parser text
    where left = Left . ParseError

-- | Parse a format string.
parseFormat
  :: TimeLocale
  -> City
  -> FormatText
  -> Either String (Parser DateTimeZoneStruct)
parseFormat locale city =
  fmap exec . parseOnly parser
  where parser = many' $ createParser locale city
        exec x = flip execState initStruct <$> sequence <$> sequence x

-- | Create a format string parser.
createParser
  :: TimeLocale
  -> City
  -> Parser (Parser (State DateTimeZoneStruct ()))
createParser locale city =
      matchLit "%%"
  <|> matchSet "%A" dtz_wday (weekLong locale)
  <|> matchSet "%a" dtz_wday (weekShort locale)
  <|> matchSet "%B" dtz_mon (monthLong locale)
  <|> matchSet "%b" dtz_mon (monthShort locale)
  <|> matchMDY "%D" dtz_year dtz_mon dtz_mday
  <|> matchSet "%d" dtz_mday (fixInt 2)
  <|> matchSet "%e" dtz_mday padIntTwo
  <|> matchYMD "%F" dtz_year dtz_mon dtz_mday
  <|> matchSet "%H" dtz_hour (fixInt 2)
  <|> matchSet "%h" dtz_mon (monthShort locale)
  <|> matchSet "%I" dtz_hour (fixInt 2)
  <|> matchSet "%l" dtz_hour padIntTwo
  <|> matchSet "%M" dtz_min (fixInt 2)
  <|> matchSet "%m" dtz_mon monthInt
  <|> matchMod "%P" dtz_hour (period locale toLower)
  <|> matchMod "%p" dtz_hour (period locale toUpper)
  <|> matchMod "%Q" dtz_sec decimal
  <|> matchHM  "%R" dtz_hour dtz_min
  <|> matchT12 "%r" dtz_hour dtz_min dtz_sec locale
  <|> matchSet "%S" dtz_sec second
  <|> matchHMS "%T" dtz_hour dtz_min dtz_sec
  <|> matchHMS "%X" dtz_hour dtz_min dtz_sec
  <|> matchMDY "%x" dtz_year dtz_mon dtz_mday
  <|> matchSet "%Y" dtz_year (fixInt 4)
  <|> matchSet "%y" dtz_year yearTwo
  <|> matchSet "%Z" dtz_zone (timezone city)
  <|> matchTxt

-- | Match a percent literal.
matchLit
  :: Text
  -> Parser (Parser (State DateTimeZoneStruct ()))
matchLit code =
  string code *>
  return (char '%' *> return (return ()))

-- | Match a percent code and update the field
--   with the value returned by the parser.
matchSet 
  :: Text
  -> (DateTimeZoneStruct :-> a)
  -> Parser a
  -> Parser (Parser (State DateTimeZoneStruct ()))
matchSet code field parser =
  string code *> return (puts field <$> parser)

-- | Match a year-month-day percent code and update
--   the fields with the values returned by the parser.
matchYMD
  :: Text
  -> (DateTimeZoneStruct :-> Year )
  -> (DateTimeZoneStruct :-> Month)
  -> (DateTimeZoneStruct :-> Day  )
  -> Parser (Parser (State DateTimeZoneStruct ()))
matchYMD code _year _mon _day =
  string code *> return parser where
  parser = do
    y <- fixInt 4; _ <- char '-'
    m <- monthInt; _ <- char '-'
    d <- fixInt 2
    return $!
      puts _year y *>
      puts _mon  m *>
      puts _day  d

-- | Match a month-day-year percent code and update
--   the fields with the values returned by the parser.
matchMDY
  :: Text
  -> (DateTimeZoneStruct :-> Year )
  -> (DateTimeZoneStruct :-> Month)
  -> (DateTimeZoneStruct :-> Day  )
  -> Parser (Parser (State DateTimeZoneStruct ()))
matchMDY code _year _mon _day =
  string code *> return parser where
  parser = do
    m <- monthInt; _ <- char '/'
    d <- fixInt 2; _ <- char '/'
    y <- yearTwo
    return $!
      puts _year y *>
      puts _mon  m *>
      puts _day  d

-- | Match a hour-minute percent code and update the
--   fields with the values returned by the parser.
matchHM
  :: Text
  -> (DateTimeZoneStruct :-> Hour  )
  -> (DateTimeZoneStruct :-> Minute)
  -> Parser (Parser (State DateTimeZoneStruct ()))
matchHM  code _hour _min =
  string code *> return parser where
  parser = do
    h <- fixInt 2; _ <- char ':'
    m <- fixInt 2
    return $!
      puts _hour h *>
      puts _min  m

-- | Match a hour-minute-second percent code and update
--   the fields with the values returned by the parser.
matchHMS
  :: Text
  -> (DateTimeZoneStruct :-> Hour  )
  -> (DateTimeZoneStruct :-> Minute)
  -> (DateTimeZoneStruct :-> Double)
  -> Parser (Parser (State DateTimeZoneStruct ()))
matchHMS code _hour _min _sec =
  string code *> return parser where
  parser = do
    h <- fixInt 2; _ <- char ':'
    m <- fixInt 2; _ <- char ':'
    s <- second
    return $!
      puts _hour h *>
      puts _min  m *>
      puts _sec  s

-- | Match a hour-minute-second-period percent code and
--   update the fields with the values returned by the parser.
matchT12
  :: Text
  -> (DateTimeZoneStruct :-> Hour  )
  -> (DateTimeZoneStruct :-> Minute)
  -> (DateTimeZoneStruct :-> Double)
  -> TimeLocale
  -> Parser (Parser (State DateTimeZoneStruct ()))
matchT12 code _hour _min _sec locale =
  string code *> return parser where
  parser = do
    h <- fixInt 2; _ <- char ':'
    m <- fixInt 2; _ <- char ':'
    s <- second  ; _ <- char ' '
    f <- period locale toUpper
    return $!
      puts   _hour h *>
      puts   _min  m *>
      puts   _sec  s *>
      modify _hour f

-- | Match a percent code and modify the field
--   with the function returned by the parser.
matchMod
  :: Text
  -> (DateTimeZoneStruct :-> a)
  -> Parser (a -> a)
  -> Parser (Parser (State DateTimeZoneStruct ()))
matchMod code field parser =
  string code *> return (modify field <$> parser)

-- | Match any other character sequence.
matchTxt :: Parser (Parser (State DateTimeZoneStruct ()))
matchTxt = takeWhile1 (/='%') >>= return . \ src -> do
  trg <- P.take $ T.length src
  if src == trg then return (return ())
  else fail "matchTxt: mismatch"

-- | Parse an integral type of exactly @n@ digits.
fixInt :: Integral a => Int -> Parser a
fixInt n = do
  s <- replicateM n digit
  return $! fromIntegral $ L.foldl' step 0 s
  where step a c = a * 10 + fromEnum c - 48

-- | Parse an integral type of two digits
--   or one digit preceded by a space.
padIntTwo :: Integral a => Parser a
padIntTwo = do
  let f a b = a * 10 + b
  liftM2  f getDigit getDigit
  <|> do char ' ' >> getDigit
  where getDigit = do
          d <- digit
          return $! fromIntegral $ fromEnum d - 48

-- | Parse a year in two digit format.
yearTwo :: Parser Year
yearTwo = f <$> fixInt 2
  where f y = if y <= 69 then 2000 + y else 1900 + y

-- | Parse a month in two digit format.
monthInt :: Parser Month
monthInt = do
  m <- fixInt 2
  if 1 <= m && m <= 12
  then return $! toEnum (m-1)
  else fail $ "monthInt: out of bounds"

-- | Parse a month in short text format.
monthShort :: TimeLocale -> Parser Month
monthShort = fromList . flip L.zip monthList . L.map (pack . snd) . months

-- | Parse a month in long text format.
monthLong :: TimeLocale -> Parser Month
monthLong = fromList . flip L.zip monthList . L.map (pack . fst) . months

-- | Parse a day of week in short text format.
weekShort :: TimeLocale -> Parser DayOfWeek
weekShort = fromList . flip L.zip weekList . L.map (pack . snd) . wDays

-- | Parse a day of week in long text format. 
weekLong :: TimeLocale -> Parser DayOfWeek
weekLong = fromList . flip L.zip weekList . L.map (pack . fst) . wDays

-- | Parse a second in two digit format.
second :: Parser Double
second = (realToFrac :: Int -> Double) <$> fixInt 2

-- | Parse a decimal in zero to twelve digit format.
decimal :: Parser (Double -> Double)
decimal = do
  _       <- char '.'
  (,) n l <- foldM step (0,0) [1..12]
  return $! (+ realToFrac n * 10 ** (- realToFrac l))
  where step :: (Int, Int) -> Int -> Parser (Int, Int)
        step acc@(n,_) l = option acc . try $ do
          c <- digit
          let n' = n * 10 + fromEnum c - 48
          return $! (n', l)

-- | Parse period symbols.
period :: TimeLocale -> (Text -> Text) -> Parser (Hour -> Hour)
period TimeLocale{amPm = (am, pm)} casify = fromList
  [(toText am, \ case 12 -> 00; x -> x     )
  ,(toText pm, \ case 12 -> 12; x -> x + 12)]
  where toText = casify . pack

-- | Parse a time zone.
timezone :: City -> Parser TimeZone
timezone city = do
  t <- takeWhile1 isAlpha
  case safeConvert . TimeZoneAbbr city $ unpack t of
    Left  err  -> fail $ prettyConvertError err
    Right zone -> return $! zone

-- | Create a parser from a list of key-value pairs.
fromList :: [(Text, a)] -> Parser a
fromList = L.foldl1 (<|>) . L.map (uncurry (*>) . (string *** return))

-- | List of days of the week.
weekList :: [DayOfWeek]
weekList =  [Sunday, Monday, Tuesday, Wednesday, Thursday, Friday, Saturday]

-- | List of months of the year.
monthList :: [Month]
monthList =  [January, February, March, April, May, June, July, August, September, October, November, December]
