-- |
-- Module     : Data.Time.Exts.Format
-- Copyright  : 2013-2016 Enzo Haussecker
-- License    : BSD3
-- Maintainer : Enzo Haussecker <enzo@sovereign.io>
-- Stability  : Stable
--
-- A list of time format directives.

module Data.Time.Exts.Format (

  -- * Format Text
       Format

     ) where

import Data.Text (Text)

-- |
-- Format text is composed of time format directives, each matching data described below.
--
-- [@%%@] % literal.
-- [@%A@] Full weekday name according to the current locale.
-- [@%B@] Full month name according to the current locale.
-- [@%D@] Equivalent to %m\/%d\/%y.
-- [@%F@] Equivalent to %Y-%m-%d.
-- [@%H@] Hour of the day using the 24-hour clock (00..23).
-- [@%I@] Hour of the day using the 12-hour clock (01..12).
-- [@%M@] Minute of the hour (00..59).
-- [@%P@] Like %p, the period of the day according to the current locale, but lowercase.
-- [@%R@] Equivalent to %H:%M.
-- [@%S@] Second of the minute (00..60).
-- [@%T@] Equivalent to %H:%M:%S.
-- [@%Y@] Year of the era (1970..9999).
-- [@%Z@] Alphabetic time zone abbreviation.
-- [@%a@] Abbreviated weekday name according to the current locale.
-- [@%b@] Abbreviated month name according to the current locale.
-- [@%d@] Day of the month (01..31).
-- [@%e@] Like %d, the day of the month, but a leading zero is replaced with a space.
-- [@%f@] Fraction of the second prefixed by a period (.0..999999999).
-- [@%h@] Equivalent to %b.
-- [@%l@] Like %I, the hour of the day using the 12-hour clock, but a leading zero is replaced with a space.
-- [@%m@] Month of the year (01..12).
-- [@%p@] Period of the day according to the current locale.
-- [@%r@] Equivalent to %I:%M:%S %p.
-- [@%y@] Year of the era without the century (00..99).
-- [@%z@] Numeric time zone.
type Format = Text
