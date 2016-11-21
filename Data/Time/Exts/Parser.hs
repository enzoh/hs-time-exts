-- |
-- Module     : Data.Time.Exts.Parser
-- Copyright  : 2013-2016 Enzo Haussecker
-- License    : BSD3
-- Maintainer : Enzo Haussecker <enzo@sovereign.io>
-- Stability  : Stable

{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE Rank2Types          #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TupleSections       #-}

{-# OPTIONS -fno-warn-missing-signatures #-}

module Data.Time.Exts.Parser (

  -- * Parser
       runParser

  -- * State
     , ParserState(..)

  -- * Lenses
     , ps_year
     , ps_mon
     , ps_mday
     , ps_wday
     , ps_hour
     , ps_min
     , ps_sec
     , ps_frac
     , ps_ampm
     , ps_zone

     ) where

import Control.Applicative        ((<|>))
import Control.Arrow              ((***))
import Control.Monad              (foldM, join, replicateM)
import Control.Monad.State.Strict (State, execState)
import Data.Attoparsec.Text       (Parser, char, digit, many', option, parseOnly, string, take, takeWhile1, try)
import Data.Char                  (isAlpha)
import Data.Foldable              (asum)
import Data.Int                   (Int64)
import Data.Text                  (Text, length, pack, toLower, unpack)
import Data.Time                  (TimeZone(..))
import Data.Time.Exts.Base        (Calendar, Day, DayOfWeek, Hour, Minute, Month, Year)
import Data.Time.Exts.Format      (Format)
import Data.Time.Zones            (LocalToUTCResult(..), TZ, localTimeToUTCFull)
import Data.Time.Zones.Internal   (int64PairToLocalTime)
import Lens.Simple                (Setter', (.=), makeLenses)
import Prelude hiding             (length, take)
import System.Locale              (TimeLocale(..))

-- |
-- Parser state.
data ParserState (cal :: Calendar) =
     ParserState
     { _ps_year :: Year
     , _ps_mon  :: Month cal
     , _ps_mday :: Day
     , _ps_wday :: DayOfWeek cal
     , _ps_hour :: Hour
     , _ps_min  :: Minute
     , _ps_sec  :: Double
     , _ps_frac :: Double -> Double
     , _ps_ampm :: Hour   -> Hour
     , _ps_zone :: Int64  -> Either String TimeZone
     }

makeLenses ''ParserState

-- |
-- Create a timestamp parser from the given format text and apply it to the given input text. Return the final parser state or an error message if the parser failed.
runParser
   :: Bounded (Month cal)
   => Enum (DayOfWeek cal)
   => Enum (Month cal)
   => TimeLocale
   -> Maybe TZ
   -> ParserState cal
   -> Format
   -> Text
   -> Either String (ParserState cal)
runParser locale tzdata state format input = join (fmap (flip parseOnly input . fmap (flip execState state . sequence) . sequence) (parseOnly (many' (create locale tzdata)) format))

-- |
-- Create a timestamp parser.
create
   :: Bounded (Month cal)
   => Enum (DayOfWeek cal)
   => Enum (Month cal)
   => TimeLocale
   -> Maybe TZ
   -> Parser (Parser (State (ParserState cal) ()))
create locale tzdata =

   --- Literals
       percent

   --- Components
   <|> match "%A" ps_wday (dayLong locale)
   <|> match "%B" ps_mon  (monthLong locale)
   <|> match "%H" ps_hour (int 2)
   <|> match "%I" ps_hour (int 2)
   <|> match "%M" ps_min  (int 2)
   <|> match "%P" ps_ampm (period locale toLower)
   <|> match "%S" ps_sec  (second)
   <|> match "%Y" ps_year (int 4)
   <|> match "%Z" ps_zone (zone tzdata)
   <|> match "%a" ps_wday (dayShort locale)
   <|> match "%b" ps_mon  (monthShort locale)
   <|> match "%d" ps_mday (int 2)
   <|> match "%e" ps_mday (int'2)
   <|> match "%f" ps_frac (decimal)
   <|> match "%h" ps_mon  (monthShort locale)
   <|> match "%l" ps_hour (int'2)
   <|> match "%m" ps_mon  (month)
   <|> match "%p" ps_ampm (period locale id)
   <|> match "%y" ps_year (year)
   <|> match "%z" ps_zone (zone tzdata)

   --- Combinators
   <|> america
   <|> iso8601
   <|> clock12 locale
   <|> clock24
   <|> clock24Short

   --- Other
   <|> text

-- |
-- Match a percent literal.
percent :: Parser (Parser (State (ParserState cal) ()))
percent = do
   _ <- string "%%"
   return $ do
      _ <- char '%'
      return $ return ()

-- |
-- Match a percent code.
match
   :: Enum (DayOfWeek cal)
   => Enum (Month cal)
   => Text
   -> Setter' (ParserState cal) a
   -> Parser a
   -> Parser (Parser (State (ParserState cal) ()))
match code field parser = do
   _ <- string code
   return $ do
      p <- parser
      return $ field .= p

-- |
-- Match a date in American format.
america :: Bounded (Month cal) => Enum (Month cal) => Parser (Parser (State (ParserState cal) ()))
america = do
   _ <- string "%D"
   return $ do
      m <- month
      _ <- char '/'
      d <- int 2
      _ <- char '/'
      y <- year
      return $ do
         ps_year .= y
         ps_mon  .= m
         ps_mday .= d

-- |
-- Match a date in ISO 8601 format.
iso8601 :: Bounded (Month cal) => Enum (Month cal) => Parser (Parser (State (ParserState cal) ()))
iso8601 = do
   _ <- string "%F"
   return $ do
      y <- int 4
      _ <- char '-'
      m <- month
      _ <- char '-'
      d <- int 2
      return $ do
         ps_year .= y
         ps_mon  .= m
         ps_mday .= d

-- |
-- Match a time in 12-hour clock format.
clock12 :: TimeLocale -> Parser (Parser (State (ParserState cal) ()))
clock12 locale = do
   _ <- string "%r"
   return $ do
      h <- int 2
      _ <- char ':'
      m <- int 2
      _ <- char ':'
      s <- second
      _ <- char ' '
      p <- period locale id
      return $ do
         ps_hour .= h
         ps_min  .= m
         ps_sec  .= s
         ps_ampm .= p

-- |
-- Match a time in 24-hour clock format.
clock24 :: Parser (Parser (State (ParserState cal) ()))
clock24 = do
   _ <- string "%T"
   return $ do
      h <- int 2
      _ <- char ':'
      m <- int 2
      _ <- char ':'
      s <- second
      return $ do
         ps_hour .= h
         ps_min  .= m
         ps_sec  .= s

-- |
-- Same as 'clock24', but with the seconds omitted.
clock24Short :: Parser (Parser (State (ParserState cal) ()))
clock24Short = do
   _ <- string "%R"
   return $ do
      h <- int 2
      _ <- char ':'
      m <- int 2
      return $ do
         ps_hour .= h
         ps_min  .= m

-- |
-- Match any other character sequence.
text :: Parser (Parser (State (ParserState cal) ()))
text = do
   src <- takeWhile1 (/='%')
   return $ do
      tgt <- take $ length src
      if src == tgt
      then return $ return ()
      else fail "text: mismatch"

-- |
-- Create a parser from key-value pairs.
fromList :: [(Text, a)] -> Parser a
fromList = asum . fmap (uncurry (*>) . (string *** return))

-- |
-- Parse an integer having the given number of digits.
int :: Integral a => Read a => Int -> Parser a
int = fmap (fromInteger . read) . flip replicateM digit

-- |
-- Parse an integer having two digits or one digit preceded by a space.
int'2 :: Integral a => Read a => Parser a
int'2 = int 2 <|> (char ' ' *> int 1)

-- |
-- Parse a year in two-digit format.
year :: Parser Year
year = fix <$> int 2 where fix n = n + if n < 70 then 2000 else 1900

-- |
-- Parse a month in two-digit format.
month :: forall cal . Bounded (Month cal) => Enum (Month cal) => Parser (Month cal)
month = do
   n <- int 2
   if fromEnum (minBound :: Month cal) <= n &&
      fromEnum (maxBound :: Month cal) >= n
   then return $! toEnum n
   else fail "month: out of bounds"

-- |
-- Parse a month in short text format.
monthShort :: Enum (Month cal) => TimeLocale -> Parser (Month cal)
monthShort = fromList . zipWith (\n (_, t) -> (pack t, toEnum n)) [1..] . months

-- |
-- Parse a month in long text format.
monthLong :: Enum (Month cal) => TimeLocale -> Parser (Month cal)
monthLong = fromList . zipWith (\n (t, _) -> (pack t, toEnum n)) [1..] . months

-- |
-- Parse a day of week in short text format.
dayShort :: Enum (DayOfWeek cal) => TimeLocale -> Parser (DayOfWeek cal)
dayShort = fromList . zipWith (\n (_, t) -> (pack t, toEnum n)) [1..] . wDays

-- |
-- Parse a day of week in long text format.
dayLong :: Enum (DayOfWeek cal) => TimeLocale -> Parser (DayOfWeek cal)
dayLong = fromList . zipWith (\n (t, _) -> (pack t, toEnum n)) [1..] . wDays

-- |
-- Parse a second in two-digit format.
second :: Parser Double
second = toEnum <$> int 2

-- |
-- Parse a decimal having up to nine digits.
decimal :: Parser (Double -> Double)
decimal = char '.' *> do
   (n, m) <- foldM step (0, 0) [1.. 9]
   return $ (+) (toEnum n * 10 ** (- toEnum m))
   where step :: (Int, Int) -> Int -> Parser (Int, Int)
         step acc@(!n, _) m = option acc (try (fmap ((, m) . (+) (10 * n) . subtract 48 . fromEnum) digit))

-- |
-- Parse a period.
period :: TimeLocale -> (Text -> Text) -> Parser (Hour -> Hour)
period TimeLocale { amPm = (am, pm) } format = fromList
   [
      (format (pack am), \ case 12 -> 00; x -> x),
      (format (pack pm), \ case 12 -> 12; x -> x + 12)
   ]

-- |
-- Parse a time zone.
zone :: Maybe TZ -> Parser (Int64 -> Either String TimeZone)
zone = \ case
   Nothing -> fail "zone: no time zone data"
   Just tzdata -> do
      name <- unpack <$> takeWhile1 isAlpha
      return $ \ base -> do
         let time = int64PairToLocalTime base 0
         case localTimeToUTCFull tzdata time of
            LTUUnique    {..} | timeZoneName _ltuZone       == name -> Right _ltuZone
            LTUAmbiguous {..} | timeZoneName _ltuFirstZone  == name -> Right _ltuFirstZone
            LTUAmbiguous {..} | timeZoneName _ltuSecondZone == name -> Right _ltuSecondZone
            _                                                       -> Left $ "unmatched acronym: " ++ name
