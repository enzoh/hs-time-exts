--------------------------------------------------------------------------------
-- Copyright (c) 2014, Enzo Haussecker, Steve Severance. All rights reserved. --
--------------------------------------------------------------------------------

{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE TypeOperators      #-}
{-# LANGUAGE UnicodeSyntax      #-}
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
{-
     , parseUnixDateTimeNanos
     , parseUnixDateTimePicos
-}
 -- ** Parse Unix Timestamps With Locale
     , parseUnixDate'
     , parseUnixTime'
     , parseUnixTimeMillis'
     , parseUnixTimeMicros'
     , parseUnixTimeNanos'
     , parseUnixTimePicos'
     , parseUnixDateTime'
     , parseUnixDateTimeMillis'
     , parseUnixDateTimeMicros'
{-
     , parseUnixDateTimeNanos'
     , parseUnixDateTimePicos'

 -- ** Parse UTC and Local Timestamps
     , parseLocalDate
     , parseLocalDateTime
     , parseLocalDateTimeMillis
     , parseLocalDateTimeMicros
     , parseLocalDateTimeNanos
     , parseLocalDateTimePicos

 -- ** Parse UTC and Local Timestamps With Locale
     , parseLocalDate'
     , parseLocalDateTime'
     , parseLocalDateTimeMillis'
     , parseLocalDateTimeMicros'
     , parseLocalDateTimeNanos'
     , parseLocalDateTimePicos'
-}
     ) where

import Control.Applicative              ((<|>), (<$>), (*>))
import Control.Arrow                    ((***))
import Control.Exception                (Exception)
import Control.Monad
import Control.Monad.State.Strict       (execState, State)
import Data.Attoparsec.Text as P hiding (decimal)
import Data.Convertible                 (Convertible(..), prettyConvertError)
import Data.Char                        (isAlpha)
import Data.Default                     (def)
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
-- [@%%@] A literal '%' character.
--
-- [@%A@] The full weekday name according to the current locale.
--
-- [@%a@] The abbreviated weekday name according to the current locale.
--
-- [@%B@] The full month name according to the current locale.
--
-- [@%b@] The abbreviated month name according to the current locale.
--
-- [@%D@] Equivalent to %m\/%d\/%y.
--
-- [@%d@] The day of the month as a decimal number (range 01 to 31).
--
-- [@%e@] Like %d, the day of the month as a decimal number, but a leading zero is replaced by a space.
--
-- [@%F@] Equivalent to %Y-%m-%d (the ISO 8601 date format).
--
-- [@%H@] The hour as a decimal number using a 24-hour clock (range 00 to 23).
--
-- [@%h@] Equivalent to %b.
--
-- [@%I@] The hour as a decimal number using a 12-hour clock (range 01 to 12).
--
-- [@%l@] Like %I, the hour as a decimal number using a 12-hour clock, but a leading zero is replaced by a space.
--
-- [@%M@] The minute as a decimal number (range 00 to 59).
--
-- [@%m@] The month as a decimal number (range 01 to 12).
--
-- [@%P@] Like %p, the period of the day according to the current locale, but lowercase.
--
-- [@%p@] The period of the day according to the current locale.
--
-- [@%Q@] The fraction of the second as a decimal number (range 0 to 999999999999).
--
-- [@%R@] Equivalent to %H:%M.
--
-- [@%r@] Equivalent to %I:%M:%S %p.
--
-- [@%S@] The second as a decimal number (range 00 to 60).
--
-- [@%T@] Equivalent to %H:%M:%S.
--
-- [@%Y@] The year as a decimal number (range 1970 to 9999).
--
-- [@%y@] The year as a decimal number without a century (range 00 to 99). 
--
-- [@%Z@] The timezone abbreviation. 
type FormatText = Text

-- | Error handling type.
newtype ParseError = ParseError String deriving (Show,Typeable)

instance Exception ParseError

instance IsString ParseError where
  fromString = ParseError

-- | A struct with date, time, and time zone
--   components, plus component modifiers.
data TZ = TZ {
    _set_year :: Year
  , _set_mon  :: Month
  , _set_mday :: Day
  , _set_wday :: DayOfWeek
  , _set_hour :: Hour
  , _set_min  :: Minute
  , _set_sec  :: Double
  , _set_frac :: Double -> Double
  , _set_ampm :: Hour   -> Hour
  , _set_zone :: TimeZone
  }

mkLabels [''TZ]

-- | Parse a Unix date.
--
-- > >>> parseUnixDate "%A, %B %e, %Y" "Tuesday, March  4, 2014"
-- > Right 2014-03-04
--
parseUnixDate :: FormatText -> Text -> Either ParseError UnixDate
parseUnixDate = parseUnixDate' def

-- | Same as @parseUnixDate@, except takes 'TimeLocale' as an additional parameter.
--
-- > >>> let german = defaultTimeLocale { wDays = [("Sonntag","So"),("Montag","Mo")...
-- > >>> parseUnixDate' german "%A, %B %e, %Y" "Dienstag, März  4, 2014"
-- > Right 2014-03-04
--
parseUnixDate' :: TimeLocale -> FormatText -> Text -> Either ParseError UnixDate
parseUnixDate' locale format text = fun <$> parseTZ locale Universal format text
  where fun TZ{..} = createUnixDate _set_year _set_mon _set_mday

-- | Parse a Unix time.
--
-- > >>> parseUnixTime "%T" "15:32:19"
-- > Right 15:32:19
--
parseUnixTime :: FormatText -> Text -> Either ParseError UnixTime
parseUnixTime = parseUnixTime' def

-- | Same as @parseUnixTime@, except takes 'TimeLocale' as an additional parameter. 
--
-- > >>> let albanian = defaultTimeLocale { wDays = [("e diel","Die"),("e hënë ","Hën")..
-- > >>> parseUnixTime' albanian "%l:%M:%S %p" "12:28:47 PD"
-- > Right 00:28:47
--
parseUnixTime' :: TimeLocale -> FormatText -> Text -> Either ParseError UnixTime
parseUnixTime' locale format text = fun <$> parseTZ locale Universal format text
  where fun TZ{..} = createUnixTime hour _set_min sec
          where hour = _set_ampm _set_hour
                sec  = truncate _set_sec

-- | Parse a Unix time with millisecond granularity.
--
-- > >>> parseUnixTimeMillis "%I:%M:%S.%Q %p" "09:41:09.313 PM"
-- > Right 21:41:09.313
--
parseUnixTimeMillis :: FormatText -> Text -> Either ParseError UnixTimeMillis
parseUnixTimeMillis = parseUnixTimeMillis' def

-- | Same as @parseUnixTimeMillis@, except takes 'TimeLocale' as an additional parameter.
--
-- > >>> let urdu = defaultTimeLocale { wDays = [("پير","پير"),("اتوار","اتوار")...
-- > >>> parseUnixTimeMillis' urdu "%l:%M:%S.%Q %p" " 3:12:47.624 ش"
-- > Right 15:12:47.624
--
parseUnixTimeMillis' :: TimeLocale -> FormatText -> Text -> Either ParseError UnixTimeMillis
parseUnixTimeMillis' locale format text = fun <$> parseTZ locale Universal format text
  where fun TZ{..} = createUnixTimeMillis hour _set_min sec mil
          where hour = _set_ampm _set_hour
                (,) sec mil = properFracMillis $ _set_frac _set_sec

-- | Parse a Unix time with microsecond granularity.
--
-- > >>> parseUnixTimeMicros "%R:%S.%Q" "03:15:50.513439"
-- > Right 03:15:50.513439
--
parseUnixTimeMicros :: FormatText -> Text -> Either ParseError UnixTimeMicros
parseUnixTimeMicros = parseUnixTimeMicros' def

-- | Same as @parseUnixTimeMicros@, except takes 'TimeLocale' as an additional parameter.
--
-- > >>> let chinese = defaultTimeLocale { wDays = [("星期日","日"),("星期一","一")...
-- > >>> parseUnixTimeMicros' chinese "%p%I:%M:%S.%Q" "下午11:46:18.130561"
-- > Right 23:46:18.130561
--
parseUnixTimeMicros' :: TimeLocale -> FormatText -> Text -> Either ParseError UnixTimeMicros
parseUnixTimeMicros' locale format text = fun <$> parseTZ locale Universal format text
  where fun TZ{..} = createUnixTimeMicros hour _set_min sec mic
          where hour = _set_ampm _set_hour
                (,) sec mic = properFracMicros $ _set_frac _set_sec

-- | Parse a Unix time with nanosecond granularity.
--
-- > >>> parseUnixTimeNanos "%l:%M:%S.%Q %P" " 1:27:44.001256754 pm"
-- > Right 13:27:44.001256754
--
parseUnixTimeNanos :: FormatText -> Text -> Either ParseError UnixTimeNanos
parseUnixTimeNanos = parseUnixTimeNanos' def

-- | Same as @parseUnixTimeNanos@, except takes 'TimeLocale' as an additional parameter.
--
-- > >>> let swahili = defaultTimeLocale { wDays = [("Jumapili","J2"),("Jumatatu","J3")...
-- > >>> parseUnixTimeNanos' swahili "%H:%M:%S.%Q %p" "12:05:50.547621324 asubuhi"
-- > Right 00:05:50.547621324
--
parseUnixTimeNanos' :: TimeLocale -> FormatText -> Text -> Either ParseError UnixTimeNanos
parseUnixTimeNanos' locale format text = fun <$> parseTZ locale Universal format text
  where fun TZ{..} = createUnixTimeNanos hour _set_min sec nan
          where hour = _set_ampm _set_hour
                (,) sec nan = properFracNanos $ _set_frac _set_sec

-- | Parse a Unix time with picosecond granularity.
--
-- > >>> parseUnixTimePicos "%T.%QZ" "13:09:23.247795919586Z"
-- > Right 13:09:23.247795919586
--
parseUnixTimePicos :: FormatText -> Text -> Either ParseError UnixTimePicos
parseUnixTimePicos = parseUnixTimePicos' def

-- | Same as @parseUnixTimePicos@, except takes 'TimeLocale' as an additional parameter.
--
-- > >>> let japanese = defaultTimeLocale { wDays = [("日曜日","日"),("月曜日","月")...
-- > >>> parseUnixTimePicos' japanese "%I:%M:%S.%Q %p" "04:20:15.340563315063 午前"
-- > Right 04:20:15.340563315063
--
parseUnixTimePicos' :: TimeLocale -> FormatText -> Text -> Either ParseError UnixTimePicos
parseUnixTimePicos' locale format text = fun <$> parseTZ locale Universal format text
  where fun TZ{..} = createUnixTimePicos hour _set_min sec pic
          where hour = _set_ampm _set_hour
                (,) sec pic = properFracPicos $ _set_frac _set_sec

-- | Parse a Unix date and time.
--
-- > >>> parseUnixDateTime "%FT%TZ" "2014-02-27T11:31:20Z"
-- > Right 2014-02-27 11:31:20
--
parseUnixDateTime :: FormatText -> Text -> Either ParseError UnixDateTime
parseUnixDateTime = parseUnixDateTime' def

-- | Same as @parseUnixDateTime@, except takes 'TimeLocale' as an additional parameter.
--
-- > >>> let somali = defaultTimeLocale { wDays = [("Axad","Axa"),("Isniin","Isn")...
-- > >>> parseUnixDateTime' somali "%A, %B %e, %r %Y" "Salaaso, Bisha Saddexaad 11, 03:41:33 galabnimo 2014"
-- > Right 2014-03-11 15:41:33
--
parseUnixDateTime' :: TimeLocale -> FormatText -> Text -> Either ParseError UnixDateTime
parseUnixDateTime' locale format text = fun <$> parseTZ locale Universal format text
  where fun TZ{..} = createUnixDateTime _set_year _set_mon _set_mday hour _set_min sec
          where hour = _set_ampm _set_hour
                sec  = truncate _set_sec

-- | Parse a Unix date and time with millisecond granularity.
--
-- > >>> parseUnixDateTimeMillis "%a %B %e %I:%M:%S.%Q %p %Y" "Wed March  5 06:53:04.475 PM 2014"
-- > Right 2014-03-05 18:53:04.475
--
parseUnixDateTimeMillis :: FormatText -> Text -> Either ParseError UnixDateTimeMillis
parseUnixDateTimeMillis = parseUnixDateTimeMillis' def

-- | Same as @parseUnixDateTimeMillis@, except takes 'TimeLocale' as an additional parameter.
--
-- > >>> let turkish = defaultTimeLocale { wDays = [("Pazar","Paz"),("Pazartesi","Pzt")...
-- > >>> parseUnixDateTimeMillis' turkish "%a %B %e %I:%M:%S.%Q %p %Y" "Prş Mart 13 07:22:54.324 ÖS 2014"
-- > Right 2014-03-13 19:22:54.324
--
parseUnixDateTimeMillis' :: TimeLocale -> FormatText -> Text -> Either ParseError UnixDateTimeMillis
parseUnixDateTimeMillis' locale format text = fun <$> parseTZ locale Universal format text
  where fun TZ{..} = createUnixDateTimeMillis _set_year _set_mon _set_mday hour _set_min sec mil
          where hour = _set_ampm _set_hour
                (,) sec mil = properFracMillis $ _set_frac _set_sec

-- | Parse a Unix date and time with microsecond granularity.
--
-- > >>> parseUnixDateTimeMicros "%D %T.%Q" "03/06/14 17:26:55.148415"
-- > Right 2014-03-06 17:26:55.148415
--
parseUnixDateTimeMicros :: FormatText -> Text -> Either ParseError UnixDateTimeMicros
parseUnixDateTimeMicros = parseUnixDateTimeMicros' def

-- | Same as @parseUnixDateTimeMicros@, except takes 'TimeLocale' as an additional parameter.
--
-- > >>> 
-- > >>> 
-- > 
--
parseUnixDateTimeMicros' :: TimeLocale -> FormatText -> Text -> Either ParseError UnixDateTimeMicros
parseUnixDateTimeMicros' locale format text = fun <$> parseTZ locale Universal format text
  where fun TZ{..} = createUnixDateTimeMicros _set_year _set_mon _set_mday hour _set_min sec mic
          where hour = _set_ampm _set_hour
                (,) sec mic = properFracMicros $ _set_frac _set_sec

{-

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

-}

-- | Initialize timestamp components.
initTZ :: TZ
initTZ =  TZ 1970 January 1 Thursday 0 0 0.0 id id utc

-- | Parse timestamp components.
parseTZ
  :: TimeLocale
  -> City
  -> FormatText
  -> Text
  -> Either ParseError TZ
parseTZ locale city format text =
  either left Right $ do
    parser <- parseFormat locale city format
    parseOnly parser text
    where left = Left . ParseError

-- | Parse format text.
parseFormat
  :: TimeLocale
  -> City
  -> FormatText
  -> Either String (Parser TZ)
parseFormat locale city =
  fmap exec . parseOnly parser
  where parser = many' $ createParser locale city
        exec x = flip execState initTZ <$> sequence <$> sequence x

-- | Create a format text parser.
createParser
  :: TimeLocale
  -> City
  -> Parser (Parser (State TZ ()))
createParser locale city =
      matchLit "%%"
  <|> matchSet "%A" set_wday (weekLong   locale)
  <|> matchSet "%a" set_wday (weekShort  locale)
  <|> matchSet "%B" set_mon  (monthLong  locale)
  <|> matchSet "%b" set_mon  (monthShort locale)
  <|> matchMDY "%D" set_year  set_mon set_mday
  <|> matchSet "%d" set_mday (fixInt 2)
  <|> matchSet "%e" set_mday  padIntTwo
  <|> matchYMD "%F" set_year  set_mon set_mday
  <|> matchSet "%H" set_hour (fixInt 2)
  <|> matchSet "%h" set_mon  (monthShort locale)
  <|> matchSet "%I" set_hour (fixInt 2)
  <|> matchSet "%l" set_hour  padIntTwo
  <|> matchSet "%M" set_min  (fixInt 2)
  <|> matchSet "%m" set_mon   monthInt
  <|> matchSet "%P" set_ampm (period locale toLower)
  <|> matchSet "%p" set_ampm (period locale id)
  <|> matchSet "%Q" set_frac  fraction
  <|> matchHM  "%R" set_hour  set_min
  <|> matchT12 "%r" set_hour  set_min set_sec locale
  <|> matchSet "%S" set_sec   second
  <|> matchHMS "%T" set_hour  set_min set_sec
  <|> matchSet "%Y" set_year (fixInt 4)
  <|> matchSet "%y" set_year  yearTwo
  <|> matchSet "%Z" set_zone (timezone city)
  <|> matchTxt

-- | Match a percent literal.
matchLit
  :: Text
  -> Parser (Parser (State TZ ()))
matchLit code =
  string code *>
  return (char '%' *> return (return ()))

-- | Match a percent code and update the field
--   with the value returned by the parser.
matchSet 
  :: Text
  -> (TZ :-> a)
  -> Parser a
  -> Parser (Parser (State TZ ()))
matchSet code field parser =
  string code *> return (puts field <$> parser)

-- | Match a year-month-day percent code and update
--   the fields with the values returned by the parser.
matchYMD
  :: Text
  -> (TZ :-> Year )
  -> (TZ :-> Month)
  -> (TZ :-> Day  )
  -> Parser (Parser (State TZ ()))
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
  -> (TZ :-> Year )
  -> (TZ :-> Month)
  -> (TZ :-> Day  )
  -> Parser (Parser (State TZ ()))
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
  -> (TZ :-> Hour  )
  -> (TZ :-> Minute)
  -> Parser (Parser (State TZ ()))
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
  -> (TZ :-> Hour  )
  -> (TZ :-> Minute)
  -> (TZ :-> Double)
  -> Parser (Parser (State TZ ()))
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
  -> (TZ :-> Hour  )
  -> (TZ :-> Minute)
  -> (TZ :-> Double)
  -> TimeLocale
  -> Parser (Parser (State TZ ()))
matchT12 code _hour _min _sec locale =
  string code *> return parser where
  parser = do
    h <- fixInt 2; _ <- char ':'
    m <- fixInt 2; _ <- char ':'
    s <- second  ; _ <- char ' '
    f <- period locale id
    return $!
      puts   _hour h *>
      puts   _min  m *>
      puts   _sec  s *>
      modify _hour f

-- | Match any other character sequence.
matchTxt :: Parser (Parser (State TZ ()))
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
fraction :: Parser (Double -> Double)
fraction = do
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
