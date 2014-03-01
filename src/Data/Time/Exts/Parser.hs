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

-- | Timestamp parsers.
module Data.Time.Exts.Parser (

 -- ** Parse UTC and Local Timestamps
       parseLocalDate
     , parseLocalDateTime
     , parseLocalDateTimeMillis
     , parseLocalDateTimeMicros
     , parseLocalDateTimeNanos
     , parseLocalDateTimePicos

     ) where

import Control.Applicative        ((<|>), (<$>), (*>))
import Control.Exception          (Exception)
import Control.Monad              (liftM)
import Control.Monad.State.Strict (execState, State)
import Data.Attoparsec.Text as P
import Data.Char                  (isAlpha, isDigit)
import Data.Label                 ((:->), mkLabels)
import Data.Label.Monadic         (puts, modify)
import Data.String                (IsString(..))
import Data.Text as T
import Data.Time.Exts.Base hiding (TimeZone)
import Data.Time.Exts.Local
import Data.Time.Exts.Zone
import Data.Typeable              (Typeable)

type FormatText = Text

newtype ParseError = ParseError String deriving (Show,Typeable)

instance Exception ParseError

instance IsString ParseError where
  fromString = ParseError

mkLabels [''DateTimeZoneStruct]

-- | Parse a local date.
--
-- > >>> parseLocalDate Seoul "%Y년 %m월 %d일 (%Z)" "2009년 03월 10일 (KST)"
-- > Right 2009-03-10 KST
--
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
createParser city = 
      matchLit
  <|> matchSet "%Y" dtz_year (parseFixedInt 4)
  <|> matchSet "%m" dtz_mon  (parseMonth)
  <|> matchSet "%d" dtz_mday (parseFixedInt 2)
  <|> matchSet "%H" dtz_hour (parseFixedInt 2)
  <|> matchSet "%M" dtz_min  (parseFixedInt 2)
  <|> matchSet "%S" dtz_sec  (parseSecond)
  <|> matchSet "%Z" dtz_zone (parseTimeZone city)
  <|> matchMod "%p" dtz_hour (parsePeriodLow)
  <|> matchMod "%P" dtz_hour (parsePeriodHigh)
  <|> matchAny

-- | Match a percent literal.
matchLit :: Parser (Parser (Either ParseError (State DateTimeZoneStruct ())))
matchLit = string "%%" *> return (char '%' *> return (return (return ())))

-- | Match a percent code and set the state
--   using the value returned by the parser.
matchSet 
  :: Text
  -> (DateTimeZoneStruct :-> a)
  -> Parser (Either ParseError a)
  -> Parser (Parser (Either ParseError (State DateTimeZoneStruct ())))
matchSet code field parser =
  string code *> return (puts field <$$> parser)

-- | Match a percent code and modify the state
--   using the function returned by the parser.
matchMod 
  :: Text
  -> (DateTimeZoneStruct :-> a)
  -> Parser (Either ParseError (a -> a))
  -> Parser (Parser (Either ParseError (State DateTimeZoneStruct ())))
matchMod code field parser = 
  string code *> return (modify field <$$> parser)

-- | Match any character sequence.
matchAny :: Parser (Parser (Either ParseError (State DateTimeZoneStruct ())))
matchAny = takeWhile1 (/='%') >>= return . \ text -> const (return ()) <$$> do
  str <- liftM toLower . P.take $ T.length text
  return $! if str == toLower text then Right str else Left "matchAny: mismatch"

-- | Parse an integral type with fixed length.
parseFixedInt :: Integral a => Int -> Parser (Either ParseError a)
parseFixedInt n = either (Left . ParseError) Right . parseOnly decimal <$> P.take n

-- | Parse a month.
parseMonth :: Parser (Either ParseError Month)
parseMonth  = fun <$> parseFixedInt 2 where
  fun = \ case
    Right n | 1 <= n && n <= 12 -> Right $ toEnum (n-1)
    Right n -> Left . ParseError $ "parseMonth: " ++ show n
    Left  e -> Left e

-- | Parse a second.
parseSecond :: Parser (Either ParseError Double)
parseSecond = do
  text1 <- P.take 2
  let x = realToFrac $ T.foldl s2n 0 text1
  if not $ T.all isDigit text1
  then return $! Left . ParseError $ "parseFixedDouble: " ++ unpack text1
  else option (Right x) . try $ do
         _     <- char '.'
         text2 <- takeWhile1 isDigit
         let y = realToFrac $ T.foldl s2n 0 text2
             z = realToFrac $ T.length text2
         return $! Right $ x + y * 10**(-z)
         where s2n a b = a * 10 + fromIntegral (fromEnum b) - 48 :: Int

-- | Parse a time zone.
parseTimeZone :: City -> Parser (Either ParseError TimeZone)
parseTimeZone city = do
  text <- takeWhile1 isAlpha
  let abbr = TimeZoneAbbr city $ unpack text
  return $! Right $ unabbreviate abbr -- FIXME: unabbreviate could throw an error...

-- | Parse lowercase am//pm symbols.
parsePeriodLow :: Parser (Either ParseError (Hour -> Hour))
parsePeriodLow = period <$> P.take 2
  where period = \ case
           "am" -> Right $ \ case 12 -> 00; x -> x
           "pm" -> Right $ \ case 12 -> 12; x -> x + 12
           text -> Left  . ParseError $ "parsePeriodLow: " ++ unpack text

-- | Parse uppercase AM//PM symbols.
parsePeriodHigh :: Parser (Either ParseError (Hour -> Hour))
parsePeriodHigh = period <$> P.take 2
   where period = \ case
            "AM" -> Right $ \ case 12 -> 00; x -> x
            "PM" -> Right $ \ case 12 -> 12; x -> x + 12
            text -> Left  . ParseError $ "parsePeriodHigh: " ++ unpack text

-- | Twice promote a function to a monad.
(<$$>) :: (Functor f1, Functor f2) => (a -> b) -> f1 (f2 a) -> f1 (f2 b)
(<$$>) f = fmap $ fmap f
