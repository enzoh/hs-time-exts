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

 -- ** Parse Unix Timestamps
       parseUnixDate
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

 -- ** Utilities
     , FormatText
     , ParseError(..)

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
import Data.List as L                   (foldl', foldl1, map)
import Data.String                      (IsString(..))
import Data.Text as T
import Data.Time.Exts.Base       hiding (TimeZone)
import Data.Time.Exts.Local
import Data.Time.Exts.Unix
import Data.Time.Exts.Zone
import Data.Typeable                    (Typeable)

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
-- [@%d@] day of month, 0-padded to two chars, 01 - 31
--
-- [@%e@] day of month, one or two chars, 1 - 31
--
-- [@%F@] same as %Y-%m-%d
--
-- [@%H@] hour of day (24-hour), 0-padded to two chars, 00 - 23
--
-- [@%h@] month of year, short form, case-insensitive, Jan - Dec
--
-- [@%I@] hour of day (12-hour), 0-padded to two chars, 01 - 12
--
-- [@%l@] hour of day (12-hour), one or two chars, 1 - 12
--
-- [@%M@] minute of hour, 0-padded to two chars, 00 - 59
--
-- [@%m@] month of year, 0-padded to two chars, 01 - 12
--
-- [@%P@] period of day, case-insensitive, am, pm
--
-- [@%p@] period of day, case-insensitive, AM, PM
--
-- [@%Q@] fraction of second, decimal point followed by zero to twelve chars, . - .999999999999
--
-- [@%R@] same as %H:%M
--
-- [@%r@] same as %I:%M:%S %p
--
-- [@%S@] second of minute, 0-padded to two chars, 00 - 60
--
-- [@%T@] same as %H:%M:%S
--
-- [@%X@] same as %H:%M:%S
--
-- [@%x@] same as %m\/%d\/%y
--
-- [@%Y@] year of century, four chars, 1970 - 9999
--
-- [@%y@] year of century, 0-padded to two chars, 00 - 99
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
parseUnixDate :: FormatText -> Text -> Either ParseError UnixDate
parseUnixDate format text = fun <$> parseDateTimeZoneStruct Universal format text
  where fun DateTimeZoneStruct{..} = createUnixDate _dtz_year _dtz_mon _dtz_mday

-- | Parse a Unix time.
--
-- > >>> parseUnixTime "%l:%M %p" "2:28 PM"
-- > Right 14:28:00
--
parseUnixTime :: FormatText -> Text -> Either ParseError UnixTime
parseUnixTime format text = fun <$> parseDateTimeZoneStruct Universal format text
  where fun DateTimeZoneStruct{..} = createUnixTime _dtz_hour _dtz_min $ truncate _dtz_sec

-- | Parse a Unix time with millisecond granularity.
--
-- > >>> parseUnixTimeMillis "%I:%M:%S%Q %p" "09:41:09.313 PM"
-- > Right 21:41:09.313
--
parseUnixTimeMillis :: FormatText -> Text -> Either ParseError UnixTimeMillis
parseUnixTimeMillis format text = fun <$> parseDateTimeZoneStruct Universal format text
  where fun DateTimeZoneStruct{..} =
          let (sec, mil) = properFracMillis _dtz_sec
          in createUnixTimeMillis _dtz_hour _dtz_min sec mil

-- | Parse a Unix time with microsecond granularity.
--
-- > >>> parseUnixTimeMicros "%R:%S%Q" "03:15:50.513439"
-- > Right 03:15:50.513439
--
parseUnixTimeMicros :: FormatText -> Text -> Either ParseError UnixTimeMicros
parseUnixTimeMicros format text = fun <$> parseDateTimeZoneStruct Universal format text
  where fun DateTimeZoneStruct{..} =
          let (sec, mic) = properFracMicros _dtz_sec
          in createUnixTimeMicros _dtz_hour _dtz_min sec mic

-- | Parse a Unix time with nanosecond granularity.
--
-- > >>> parseUnixTimeNanos "%X%Q" "23:34:39.734563023"
-- > Right 23:34:39.734563023
--
parseUnixTimeNanos :: FormatText -> Text -> Either ParseError UnixTimeNanos
parseUnixTimeNanos format text = fun <$> parseDateTimeZoneStruct Universal format text
  where fun DateTimeZoneStruct{..} =
          let (sec, nan) = properFracNanos _dtz_sec
          in createUnixTimeNanos _dtz_hour _dtz_min sec nan

-- | Parse a Unix time with picosecond granularity.
--
-- > >>> parseUnixTimePicos "%T%Q" "02:28:56.621236981055"
-- > Right 02:28:56.621236981055
--
parseUnixTimePicos :: FormatText -> Text -> Either ParseError UnixTimePicos
parseUnixTimePicos format text = fun <$> parseDateTimeZoneStruct Universal format text
  where fun DateTimeZoneStruct{..} =
          let (sec, pic) = properFracPicos _dtz_sec
          in createUnixTimePicos _dtz_hour _dtz_min sec pic

-- | Parse a Unix date and time.
--
-- > >>> parseUnixDateTime "%FT%TZ" "2014-02-27T11:31:20Z"
-- > Right 2014-02-27 11:31:20
--
parseUnixDateTime :: FormatText -> Text -> Either ParseError UnixDateTime
parseUnixDateTime format text = fun <$> parseDateTimeZoneStruct Universal format text
  where fun DateTimeZoneStruct{..} =
          let sec = truncate _dtz_sec
          in createUnixDateTime _dtz_year _dtz_mon _dtz_mday _dtz_hour _dtz_min sec

-- | Parse a Unix date and time with millisecond granularity.
--
-- > >>> parseUnixDateTimeMillis "%a %B %e %T%Q %p %Y" "Wed March 5 06:53:04.475 PM 2014"
-- > Right 2014-03-05 18:53:04.475
--
parseUnixDateTimeMillis :: FormatText -> Text -> Either ParseError UnixDateTimeMillis
parseUnixDateTimeMillis format text = fun <$> parseDateTimeZoneStruct Universal format text
  where fun DateTimeZoneStruct{..} =
          let (sec, mil) = properFracMillis _dtz_sec
          in createUnixDateTimeMillis _dtz_year _dtz_mon _dtz_mday _dtz_hour _dtz_min sec mil

-- | Parse a Unix date and time with microsecond granularity.
--
-- > >>> parseUnixDateTimeMicros "%D %X%Q" "03/06/14 17:26:55.148415"
-- > Right 2014-03-06 17:26:55.148415
--
parseUnixDateTimeMicros :: FormatText -> Text -> Either ParseError UnixDateTimeMicros
parseUnixDateTimeMicros format text = fun <$> parseDateTimeZoneStruct Universal format text
  where fun DateTimeZoneStruct{..} =
          let (sec, mic) = properFracMicros _dtz_sec
          in createUnixDateTimeMicros _dtz_year _dtz_mon _dtz_mday _dtz_hour _dtz_min sec mic

-- | Parse a Unix date and time with nanosecond granularity.
--
-- > >>> parseUnixDateTimeNanos "%d.%m.%Y %T%Q" "18.03.2014 19:06:43.774295132"
-- > Right 2014-03-18 19:06:43.774295132
--
parseUnixDateTimeNanos :: FormatText -> Text -> Either ParseError UnixDateTimeNanos
parseUnixDateTimeNanos format text = fun <$> parseDateTimeZoneStruct Universal format text
  where fun DateTimeZoneStruct{..} =
          let (sec, nan) = properFracNanos _dtz_sec
          in createUnixDateTimeNanos _dtz_year _dtz_mon _dtz_mday _dtz_hour _dtz_min sec nan

-- | Parse a Unix date and time with picosecond granularity.
--
-- > >>> parseUnixDateTimePicos "%FT%T%QZ" "2014-03-03T17:58:15.916795765305Z"
-- > Right 2014-03-03 17:58:15.916795765305
--
parseUnixDateTimePicos :: FormatText -> Text -> Either ParseError UnixDateTimePicos
parseUnixDateTimePicos format text = fun <$> parseDateTimeZoneStruct Universal format text
  where fun DateTimeZoneStruct{..} =
          let (sec, pic) = properFracPicos _dtz_sec
          in createUnixDateTimePicos _dtz_year _dtz_mon _dtz_mday _dtz_hour _dtz_min sec pic

-- | Parse a local date.
--
-- > >>> parseLocalDate Los_Angeles "%A, %B %e, %Y (%Z)" "Monday, March 17, 2014 (PST)"
-- > Right 2014-03-17 PST
--
parseLocalDate :: City -> FormatText -> Text -> Either ParseError LocalDate
parseLocalDate city format text = fun <$> parseDateTimeZoneStruct city format text
  where fun DateTimeZoneStruct{..} = createLocalDate _dtz_year _dtz_mon _dtz_mday _dtz_zone

-- | Parse a local date and time.
--
-- > >>> parseLocalDateTime New_York "%a %b %e %H:%M:%S %Z %Y" "Fri Mar 14 09:29:53 EST 2014"
-- > Right 2014-03-14 09:29:53 EST
--
parseLocalDateTime :: City -> FormatText -> Text -> Either ParseError LocalDateTime
parseLocalDateTime city format text = fromDateTimeZoneStruct <$> parseDateTimeZoneStruct city format text

-- | Parse a local date and time with millisecond granularity.
--
-- > >>> parseLocalDateTimeMillis Tel_Aviv "%B %e %Y %I:%M:%S%Q %p %Z" "July 1 2012 01:59:60.215 AM IST"
-- > Right 2012-07-01 01:59:60.215 IST
--
--   Note that the timestamp in the example above corresponds to a leap second.
parseLocalDateTimeMillis :: City -> FormatText -> Text -> Either ParseError LocalDateTimeMillis
parseLocalDateTimeMillis city format text = fromDateTimeZoneStruct <$> parseDateTimeZoneStruct city format text

-- | Parse a local date and time with microsecond granularity.
--
-- > >>> parseLocalDateTimeMicros Hong_Kong "%F %T%Q (%Z)" "2014-03-04 02:45:42.827495 (HKT)"
-- > Right 2014-03-04 02:45:42.827495 HKT
--
parseLocalDateTimeMicros :: City -> FormatText -> Text -> Either ParseError LocalDateTimeMicros
parseLocalDateTimeMicros city format text = fromDateTimeZoneStruct <$> parseDateTimeZoneStruct city format text

-- | Parse a local date and time with nanosecond granularity.
--
-- > >>> parseLocalDateTimeNanos Universal "%b. %d, %T%Q %Z %Y" "Mar. 09, 18:53:55.856423459 UTC 2014"
-- > Right 2014-03-09 18:53:55.856423459 UTC
--
parseLocalDateTimeNanos :: City -> FormatText -> Text -> Either ParseError LocalDateTimeNanos
parseLocalDateTimeNanos city format text = fromDateTimeZoneStruct <$> parseDateTimeZoneStruct city format text

-- | Parse a local date and time with picosecond granularity.
--
-- > >>> parseLocalDateTimePicos Singapore "%d.%m.%Y %T%Q %Z" "09.04.2014 05:22:56.587234905781 SGT"
-- > Right 2014-04-09 05:22:56.587234905781 SGT
--
parseLocalDateTimePicos :: City -> FormatText -> Text -> Either ParseError LocalDateTimePicos
parseLocalDateTimePicos city format text = fromDateTimeZoneStruct <$> parseDateTimeZoneStruct city format text

-- | Initialize date, time, and time zone components.
initStruct :: DateTimeZoneStruct
initStruct =  DateTimeZoneStruct 1970 January 1 Thursday 0 0 0.0 utc

-- | Parse date, time, and time zone components.
parseDateTimeZoneStruct
  :: City
  -> FormatText
  -> Text
  -> Either ParseError DateTimeZoneStruct
parseDateTimeZoneStruct city format text =
  either left Right $ do
    parser <- parseFormat city format
    parseOnly parser text
    where left = Left . ParseError

-- | Parse a format string.
parseFormat
  :: City
  -> FormatText
  -> Either String (Parser DateTimeZoneStruct)
parseFormat city = fmap exec . parseOnly parser
    where parser = many' $ createParser city
          exec x = flip execState initStruct <$> sequence <$> sequence x

-- | Create a format string parser.
createParser :: City -> Parser (Parser (State DateTimeZoneStruct ()))
createParser x = 
      matchLit "%%"
  <|> matchSet "%A" dtz_wday weekLong
  <|> matchSet "%a" dtz_wday weekShort
  <|> matchSet "%B" dtz_mon monthLong
  <|> matchSet "%b" dtz_mon monthShort
  <|> matchMDY "%D" dtz_year dtz_mon dtz_mday
  <|> matchSet "%d" dtz_mday (fixInt 2)
  <|> matchSet "%e" dtz_mday varInt
  <|> matchYMD "%F" dtz_year dtz_mon dtz_mday
  <|> matchSet "%H" dtz_hour (fixInt 2)
  <|> matchSet "%h" dtz_mon monthShort
  <|> matchSet "%I" dtz_hour (fixInt 2)
  <|> matchSet "%l" dtz_hour varInt
  <|> matchSet "%M" dtz_min (fixInt 2)
  <|> matchSet "%m" dtz_mon month
  <|> matchMod "%P" dtz_hour period
  <|> matchMod "%p" dtz_hour period
  <|> matchMod "%Q" dtz_sec decimal
  <|> matchHM  "%R" dtz_hour dtz_min
  <|> matchT12 "%r" dtz_hour dtz_min dtz_sec
  <|> matchSet "%S" dtz_sec second
  <|> matchHMS "%T" dtz_hour dtz_min dtz_sec
  <|> matchHMS "%X" dtz_hour dtz_min dtz_sec
  <|> matchMDY "%x" dtz_year dtz_mon dtz_mday
  <|> matchSet "%Y" dtz_year (fixInt 4)
  <|> matchSet "%y" dtz_year year
  <|> matchSet "%Z" dtz_zone (timezone x)
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
    m <- month   ; _ <- char '-'
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
    m <- month   ; _ <- char '/'
    d <- fixInt 2; _ <- char '/'
    y <- year
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
  -> Parser (Parser (State DateTimeZoneStruct ()))
matchT12 code _hour _min _sec =
  string code *> return parser where
  parser = do
    h <- fixInt 2; _ <- char ':'
    m <- fixInt 2; _ <- char ':'
    s <- second  ; _ <- char ' '
    f <- period
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

-- | Parse an integral type of 1 or 2 digits.
varInt :: Integral a => Parser a
varInt = do
  n0 <- getDigit
  option n0 . try $ do
    n1 <- getDigit
    return $! n0 * 10 + n1
    where getDigit = do
            d <- digit
            return $! fromIntegral $ fromEnum d - 48

-- | Parse a year in two digit format (00-99).
year :: Parser Year
year =  f <$> fixInt 2
  where f y = if y <= 69 then 2000 + y else 1900 + y

-- | Parse a month in two digit format (01-12).
month :: Parser Month
month = do
  m <- fixInt 2
  if 1 <= m && m <= 12
  then return $! toEnum (m-1)
  else fail $ "month: out of bounds"

-- | Parse a month in short text format.
monthShort :: Parser Month
monthShort = fromList
  [("Jan", January),("Feb", February),("Mar", March    )
  ,("Apr", April  ),("May", May     ),("Jun", June     )
  ,("Jul", July   ),("Aug", August  ),("Sep", September)
  ,("Oct", October),("Nov", November),("Dec", December )]

-- | Parse a month in long text format.
monthLong :: Parser Month
monthLong = fromList
  [("January", January),("February", February),("March"    , March    )
  ,("April"  , April  ),("May"     , May     ),("June"     , June     )
  ,("July"   , July   ),("August"  , August  ),("September", September)
  ,("October", October),("November", November),("December" , December )]

-- | Parse a day of week in short text form.
weekShort :: Parser DayOfWeek
weekShort = fromList
  [("Sun", Sunday   ),("Mon", Monday  ),("Tue", Tuesday)
  ,("Wed", Wednesday),("Thu", Thursday),("Fri", Friday )
  ,("Sat", Saturday )]

-- | Parse a day of week in long text form. 
weekLong :: Parser DayOfWeek
weekLong = fromList
  [("Sunday"   , Sunday   ),("Monday"  , Monday  ),("Tuesday", Tuesday)
  ,("Wednesday", Wednesday),("Thursday", Thursday),("Friday" , Friday )
  ,("Saturday" , Saturday )]

-- | Parse a second in two digit format (00-60).
second :: Parser Double
second = (realToFrac :: Int -> Double) <$> fixInt 2

-- | Parse a decimal in zero to twelve digit format (.0-.999999999999).
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

-- | Parse AM\/PM symbols.
period :: Parser (Hour -> Hour)
period = fromList
  [("AM",\ case 12 -> 00; x -> x     )
  ,("PM",\ case 12 -> 12; x -> x + 12)]

-- | Parse a time zone.
timezone :: City -> Parser TimeZone
timezone city = do
  t <- takeWhile1 isAlpha
  case safeConvert . TimeZoneAbbr city $ unpack t of
    Left  err  -> fail $ prettyConvertError err
    Right zone -> return $! zone

-- | Create a parser from a list of key-value pairs.
fromList :: [(Text, a)] -> Parser a
fromList = L.foldl1 (<|>) . L.map (uncurry (*>) . (asciiCI *** return))
