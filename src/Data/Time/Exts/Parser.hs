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

import Control.Applicative        ((<|>), (<$>), (*>))
import Control.Exception          (Exception)
import Control.Monad              (liftM, liftM3)
import Control.Monad.State.Strict (execState, State)
import Data.Attoparsec.Text as P
import Data.Convertible           (Convertible(..), prettyConvertError)
import Data.Char                  (isAlpha, isDigit)
import Data.Label                 ((:->), mkLabels)
import Data.Label.Monadic         (puts, modify)
import Data.List as L             (foldl1, map)
import Data.String                (IsString(..))
import Data.Text as T
import Data.Time.Exts.Base hiding (TimeZone)
import Data.Time.Exts.Local
import Data.Time.Exts.Unix
import Data.Time.Exts.Zone
import Data.Typeable              (Typeable)

type FormatText = Text

newtype ParseError = ParseError String deriving (Show,Typeable)

instance Exception ParseError

instance IsString ParseError where
  fromString = ParseError

mkLabels [''DateTimeZoneStruct]

-- | Parse a Unix date.
--
-- > >>> parseUnixDate "%F" "2014-03-02"
-- > Right 2014-03-02
--
parseUnixDate :: FormatText -> Text -> Either ParseError UnixDate
parseUnixDate format text = fun <$> parseStruct Universal format text
  where fun DateTimeZoneStruct{..} = createUnixDate _dtz_year _dtz_mon _dtz_mday

-- | Parse a Unix time.
--
-- > >>> parseUnixTime "%T" "02:28:32"
-- > Right 02:28:32
--
parseUnixTime :: FormatText -> Text -> Either ParseError UnixTime
parseUnixTime format text = fun <$> parseStruct Universal format text
  where fun DateTimeZoneStruct{..} = createUnixTime _dtz_hour _dtz_min $ truncate _dtz_sec

-- | Parse a Unix time with millisecond granularity.
parseUnixTimeMillis :: FormatText -> Text -> Either ParseError UnixTimeMillis
parseUnixTimeMillis format text = fun <$> parseStruct Universal format text
  where fun DateTimeZoneStruct{..} =
          let (sec, mil) = properFracMillis _dtz_sec
          in createUnixTimeMillis _dtz_hour _dtz_min sec mil

-- | Parse a Unix time with microsecond granularity.
parseUnixTimeMicros :: FormatText -> Text -> Either ParseError UnixTimeMicros
parseUnixTimeMicros format text = fun <$> parseStruct Universal format text
  where fun DateTimeZoneStruct{..} =
          let (sec, mic) = properFracMicros _dtz_sec
          in createUnixTimeMicros _dtz_hour _dtz_min sec mic

-- | Parse a Unix time with nanosecond granularity.
parseUnixTimeNanos :: FormatText -> Text -> Either ParseError UnixTimeNanos
parseUnixTimeNanos format text = fun <$> parseStruct Universal format text
  where fun DateTimeZoneStruct{..} =
          let (sec, nan) = properFracNanos _dtz_sec
          in createUnixTimeNanos _dtz_hour _dtz_min sec nan

-- | Parse a Unix time with picosecond granularity.
parseUnixTimePicos :: FormatText -> Text -> Either ParseError UnixTimePicos
parseUnixTimePicos format text = fun <$> parseStruct Universal format text
  where fun DateTimeZoneStruct{..} =
          let (sec, pic) = properFracPicos _dtz_sec
          in createUnixTimePicos _dtz_hour _dtz_min sec pic

-- | Parse a Unix date and time.
--
-- > >>> parseUnixDateTime "%FT%TZ" "2014-02-27T11:31:20Z"
-- > Right 2014-02-27 11:31:20
--
parseUnixDateTime :: FormatText -> Text -> Either ParseError UnixDateTime
parseUnixDateTime format text = fun <$> parseStruct Universal format text
  where fun DateTimeZoneStruct{..} =
          let sec = truncate _dtz_sec
          in createUnixDateTime _dtz_year _dtz_mon _dtz_mday _dtz_hour _dtz_min sec

-- | Parse a Unix date and time with millisecond granularity.
parseUnixDateTimeMillis :: FormatText -> Text -> Either ParseError UnixDateTimeMillis
parseUnixDateTimeMillis format text = fun <$> parseStruct Universal format text
  where fun DateTimeZoneStruct{..} =
          let (sec, mil) = properFracMillis _dtz_sec
          in createUnixDateTimeMillis _dtz_year _dtz_mon _dtz_mday _dtz_hour _dtz_min sec mil

-- | Parse a Unix date and time with microsecond granularity.
parseUnixDateTimeMicros :: FormatText -> Text -> Either ParseError UnixDateTimeMicros
parseUnixDateTimeMicros format text = fun <$> parseStruct Universal format text
  where fun DateTimeZoneStruct{..} =
          let (sec, mic) = properFracMicros _dtz_sec
          in createUnixDateTimeMicros _dtz_year _dtz_mon _dtz_mday _dtz_hour _dtz_min sec mic

-- | Parse a Unix date and time with nanosecond granularity.
parseUnixDateTimeNanos :: FormatText -> Text -> Either ParseError UnixDateTimeNanos
parseUnixDateTimeNanos format text = fun <$> parseStruct Universal format text
  where fun DateTimeZoneStruct{..} =
          let (sec, nan) = properFracNanos _dtz_sec
          in createUnixDateTimeNanos _dtz_year _dtz_mon _dtz_mday _dtz_hour _dtz_min sec nan

-- | Parse a Unix date and time with picosecond granularity.
parseUnixDateTimePicos :: FormatText -> Text -> Either ParseError UnixDateTimePicos
parseUnixDateTimePicos format text = fun <$> parseStruct Universal format text
  where fun DateTimeZoneStruct{..} =
          let (sec, pic) = properFracPicos _dtz_sec
          in createUnixDateTimePicos _dtz_year _dtz_mon _dtz_mday _dtz_hour _dtz_min sec pic

-- | Parse a local date.
parseLocalDate :: City -> FormatText -> Text -> Either ParseError LocalDate
parseLocalDate city format text = convert <$> parseStruct city format text
  where convert DateTimeZoneStruct{..} = createLocalDate _dtz_year _dtz_mon _dtz_mday _dtz_zone

-- | Parse a local date and time.
parseLocalDateTime :: City -> FormatText -> Text -> Either ParseError LocalDateTime
parseLocalDateTime city format text = fromDateTimeZoneStruct <$> parseStruct city format text

-- | Parse a local date and time with millisecond granularity.
parseLocalDateTimeMillis :: City -> FormatText -> Text -> Either ParseError LocalDateTimeMillis
parseLocalDateTimeMillis city format text = fromDateTimeZoneStruct <$> parseStruct city format text

-- | Parse a local date and time with microsecond granularity.
parseLocalDateTimeMicros :: City -> FormatText -> Text -> Either ParseError LocalDateTimeMicros
parseLocalDateTimeMicros city format text = fromDateTimeZoneStruct <$> parseStruct city format text

-- | Parse a local date and time with nanosecond granularity.
parseLocalDateTimeNanos :: City -> FormatText -> Text -> Either ParseError LocalDateTimeNanos
parseLocalDateTimeNanos city format text = fromDateTimeZoneStruct <$> parseStruct city format text

-- | Parse a local date and time with picosecond granularity.
parseLocalDateTimePicos :: City -> FormatText -> Text -> Either ParseError LocalDateTimePicos
parseLocalDateTimePicos city format text = fromDateTimeZoneStruct <$> parseStruct city format text

-- | Initialize date, time, and time zone components.
initStruct :: DateTimeZoneStruct
initStruct =  DateTimeZoneStruct 1970 January 1 Thursday 0 0 0.0 utc

-- | Parse date, time, and time zone components.
parseStruct :: City -> FormatText -> Text -> Either ParseError DateTimeZoneStruct
parseStruct city format text =
  case parseFormat city format of
       Left  err    -> Left err
       Right parser ->
        case parse parser text of
             Done _   val -> val
             Fail _ _ err -> Left $ ParseError err
             Partial  fun ->
                 case fun "" of
                      Done _   val -> val
                      Fail _ _ err -> Left $ ParseError err
                      Partial    _ -> Left $ ParseError "parseStruct: failed to complete"

-- | Parse a format string.
parseFormat :: City -> FormatText -> Either ParseError (Parser (Either ParseError DateTimeZoneStruct))
parseFormat city format =
  case parse parser format of
       Done _   val -> Right $ eval val
       Fail _ _ err -> Left  $ ParseError err
       Partial  fun ->
           case fun "" of
                Done _   val -> Right $ eval val
                Fail _ _ err -> Left  $ ParseError err
                Partial    _ -> Left  $ ParseError "parseFormat: failed to complete"
  where
     parser = many1 $ createParser city
     eval x = fmap (flip execState initStruct . sequence) . sequence <$> sequence x

-- | Create a format string parser.
createParser :: City -> Parser (Parser (Either ParseError (State DateTimeZoneStruct ())))
createParser x = 
      matchLit
  <|> matchSet "%a" dtz_wday  parseWeekShort
  <|> matchSet "%b" dtz_mon   parseMonthShort
  <|> matchSet "%d" dtz_mday (parseFixedInt 2)
  <|> matchSet "%h" dtz_mon   parseMonthShort
  <|> matchSet "%m" dtz_mon   parseMonthInt
  <|> matchMod "%p" dtz_hour  parsePeriodLow
  <|> matchMDY "%x" dtz_year  dtz_mon dtz_mday
  <|> matchSet "%y" dtz_year  parseYearShort
  <|> matchSet "%A" dtz_wday  parseWeekLong
  <|> matchSet "%B" dtz_mon   parseMonthLong
  <|> matchMDY "%D" dtz_year  dtz_mon dtz_mday
  <|> matchYMD "%F" dtz_year  dtz_mon dtz_mday
  <|> matchSet "%H" dtz_hour (parseFixedInt 2)
  <|> matchSet "%M" dtz_min  (parseFixedInt 2)
  <|> matchMod "%P" dtz_hour  parsePeriodHigh
  <|> matchSet "%S" dtz_sec   parseSecond
  <|> matchHMS "%T" dtz_hour  dtz_min dtz_sec
  <|> matchSet "%Y" dtz_year (parseFixedInt 4)
  <|> matchSet "%Z" dtz_zone (parseTimeZone x)
  <|> matchAny

-- | Match a percent literal.
matchLit :: Parser (Parser (Either ParseError (State DateTimeZoneStruct ())))
matchLit = string "%%" *> return (char '%' *> return (return (return ())))

-- | Match a percent code and update the field
--   with the value returned by the parser.
matchSet 
  :: Text
  -> (DateTimeZoneStruct :-> a)
  -> Parser (Either ParseError a)
  -> Parser (Parser (Either ParseError (State DateTimeZoneStruct ())))
matchSet code field parser =
  string code *> return (puts field <$$> parser)

-- | Match a year-month-day percent code and update
--   the fields with the values returned by the parser.
matchYMD
  :: Text
  -> (DateTimeZoneStruct :-> Year )
  -> (DateTimeZoneStruct :-> Month)
  -> (DateTimeZoneStruct :-> Day  )
  -> Parser (Parser (Either ParseError (State DateTimeZoneStruct ())))
matchYMD code year mon day = string code *> return (fields <$$> parser)
  where fields (y,m,d) = puts year y *> puts mon m *> puts day d
        parser = do
          y <- parseFixedInt 4
          _ <- char '-'
          m <- parseMonthInt
          _ <- char '-'
          d <- parseFixedInt 2
          return $! liftM3 (,,) y m d

-- | Match a month-day-year percent code and update
--   the fields with the values returned by the parser.
matchMDY
  :: Text
  -> (DateTimeZoneStruct :-> Year )
  -> (DateTimeZoneStruct :-> Month)
  -> (DateTimeZoneStruct :-> Day  )
  -> Parser (Parser (Either ParseError (State DateTimeZoneStruct ())))
matchMDY code year mon day = string code *> return (fields <$$> parser)
  where fields (m,d,y) = puts mon m *> puts day d *> puts year y
        parser = do
          m <- parseMonthInt
          _ <- char '/'
          d <- parseFixedInt 2
          _ <- char '/'
          y <- parseYearShort
          return $! liftM3 (,,) m d y

-- | Match a hour-minute-second percent code and update
--   the fields with the values returned by the parser.
matchHMS
  :: Text
  -> (DateTimeZoneStruct :-> Hour  )
  -> (DateTimeZoneStruct :-> Minute)
  -> (DateTimeZoneStruct :-> Double)
  -> Parser (Parser (Either ParseError (State DateTimeZoneStruct ())))
matchHMS code hour mn sec = string code *> return (fields <$$> parser)
  where fields (h,m,s) = puts hour h *> puts mn m *> puts sec s
        parser = do
          h <- parseFixedInt 2
          _ <- char ':'
          m <- parseFixedInt 2
          _ <- char ':'
          s <- parseSecond
          return $! liftM3 (,,) h m s

-- | Match a percent code and modify the field
--   with the function returned by the parser.
matchMod
  :: Text
  -> (DateTimeZoneStruct :-> a)
  -> Parser (Either ParseError (a -> a))
  -> Parser (Parser (Either ParseError (State DateTimeZoneStruct ())))
matchMod code field parser =
  string code *> return (modify field <$$> parser)

-- | Match any other character sequence.
matchAny :: Parser (Parser (Either ParseError (State DateTimeZoneStruct ())))
matchAny  = takeWhile1 (/='%') >>= return . \ text -> const (return ()) <$$> do
  let src = toLower text
  trg <- liftM toLower . P.take $ T.length src
  return $! if src == trg
    then Right src
    else let msg = "matchAny: " ++ show src ++ " not equal to " ++ show trg
         in Left $ ParseError msg

-- | Parse an integer with fixed length.
parseFixedInt :: Integral a => Int -> Parser (Either ParseError a)
parseFixedInt n = do
  text <- P.take n
  return $! if not $ T.all isDigit text
    then Left  . ParseError   $ "parseFixedInt: " ++ unpack text
    else Right . fromIntegral $ T.foldl s2n 0 text
    where s2n a b = a * 10 + fromEnum b - 48

-- | Parse a year in two digit format.
parseYearShort :: Parser (Either ParseError Year)
parseYearShort = fun <$$> parseFixedInt 2
  where fun yr = if yr <= 69 then 2000 + yr else 1900 + yr

-- | Parse a month in integer format with fixed length.
parseMonthInt :: Parser (Either ParseError Month)
parseMonthInt = match <$> parseFixedInt 2 where
  match = \ case
    Right n | 1 <= n && n <= 12 -> Right $ toEnum (n-1)
    Right n -> Left . ParseError $ "parseMonthInt: " ++ show n
    Left  e -> Left e

-- | Parse a month in short text format.
parseMonthShort :: Parser (Either ParseError Month)
parseMonthShort = parseText text <|> left where
  left = return (Left $ ParseError "parseMonthShort: not enough input")
  text = [("Jan", January),("Feb", February),("Mar", March    )
         ,("Apr", April  ),("May", May     ),("Jun", June     )
         ,("Jul", July   ),("Aug", August  ),("Sep", September)
         ,("Oct", October),("Nov", November),("Dec", December )]

-- | Parse a month in long text format.
parseMonthLong :: Parser (Either ParseError Month)
parseMonthLong = parseText text <|> left where
  left = return (Left $ ParseError "parseMonthLong: not enough input")
  text = [("January", January),("February", February),("March"    , March    )
         ,("April"  , April  ),("May"     , May     ),("June"     , June     )
         ,("July"   , July   ),("August"  , August  ),("September", September)
         ,("October", October),("November", November),("December" , December )]

-- | Parse a day of week in short text form. 
parseWeekShort :: Parser (Either ParseError DayOfWeek)
parseWeekShort = parseText text <|> left where
  left = return (Left $ ParseError "parseWeekShort: not enough input")
  text = [("Sun", Sunday   ),("Mon", Monday  ),("Tue", Tuesday)
         ,("Wed", Wednesday),("Thu", Thursday),("Fri", Friday )
         ,("Sat", Saturday )]

-- | Parse a day of week in long text form. 
parseWeekLong :: Parser (Either ParseError DayOfWeek)
parseWeekLong = parseText text <|> left where
  left = return (Left $ ParseError "parseWeekShort: not enough input")
  text = [("Sunday"   , Sunday   ),("Monday"  , Monday  ),("Tuesday", Tuesday)
         ,("Wednesday", Wednesday),("Thursday", Thursday),("Friday" , Friday )
         ,("Saturday" , Saturday )]

-- | Parse a second.
parseSecond :: Parser (Either ParseError Double)
parseSecond = fun <$$> parseFixedInt 2
  where fun = realToFrac :: Int -> Double

-- | Parse lowercase am//pm symbols.
parsePeriodLow :: Parser (Either ParseError (Hour -> Hour))
parsePeriodLow = match <$> P.take 2 where
  match = \ case
    "am" -> Right $ \ case 12 -> 00; x -> x
    "pm" -> Right $ \ case 12 -> 12; x -> x + 12
    text -> Left  . ParseError $ "parsePeriodLow: " ++ unpack text

-- | Parse uppercase AM//PM symbols.
parsePeriodHigh :: Parser (Either ParseError (Hour -> Hour))
parsePeriodHigh = match <$> P.take 2 where
  match = \ case
    "AM" -> Right $ \ case 12 -> 00; x -> x
    "PM" -> Right $ \ case 12 -> 12; x -> x + 12
    text -> Left  . ParseError $ "parsePeriodHigh: " ++ unpack text

-- | Parse a time zone.
parseTimeZone :: City -> Parser (Either ParseError TimeZone)
parseTimeZone city = do
  text <- takeWhile1 isAlpha
  let zone = safeConvert . TimeZoneAbbr city $ unpack text
  return $! either err Right zone
  where err = Left . ParseError . prettyConvertError

-- | Parse text and match with value.
parseText :: [(Text, a)] -> Parser (Either ParseError a)
parseText = L.foldl1 (<|>) . L.map match where
  match (name, val) = asciiCI name *> return (Right val)

-- | Twice promote a function to a monad.
(<$$>) :: (Functor f1, Functor f2) => (a -> b) -> f1 (f2 a) -> f1 (f2 b)
(<$$>) f = fmap $ fmap f
