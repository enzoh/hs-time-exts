-- |
-- Module     : Data.Time.Exts.Util
-- Copyright  : 2013-2016 Enzo Haussecker
-- License    : BSD3
-- Maintainer : Enzo Haussecker <enzo@sovereign.io>
-- Stability  : Stable

module Data.Time.Exts.Util where

import Data.Int
import Data.Time.Exts.Base

-- |
-- Convert Unix seconds into a UTC seconds.
baseUnixToUTC :: Int64 -> Int64
baseUnixToUTC base
   | base >= 1341100800 = base + 25
   | base >= 1230768000 = base + 24
   | base >= 1136073600 = base + 23
   | base >= 0915148800 = base + 22
   | base >= 0867715200 = base + 21
   | base >= 0820454400 = base + 20
   | base >= 0773020800 = base + 19
   | base >= 0741484800 = base + 18
   | base >= 0709948800 = base + 17
   | base >= 0662688000 = base + 16
   | base >= 0631152000 = base + 15
   | base >= 0567993600 = base + 14
   | base >= 0489024000 = base + 13
   | base >= 0425865600 = base + 12
   | base >= 0394329600 = base + 11
   | base >= 0362793600 = base + 10
   | base >= 0315532800 = base + 09
   | base >= 0283996800 = base + 08
   | base >= 0252460800 = base + 07
   | base >= 0220924800 = base + 06
   | base >= 0189302400 = base + 05
   | base >= 0157766400 = base + 04
   | base >= 0126230400 = base + 03
   | base >= 0094694400 = base + 02
   | base >= 0078796800 = base + 01
   | otherwise          = base + 00

-- |
-- Convert UTC seconds into Unix and leap seconds.
baseUTCToUnix :: Int64 -> (Int64, Double)
baseUTCToUnix base
   | base >= 1341100825 = (base - 0025, 0)
   | base == 1341100824 = (01341100799, 1)
   | base >= 1230768024 = (base - 0024, 0)
   | base == 1230768023 = (01230767999, 1)
   | base >= 1136073623 = (base - 0023, 0)
   | base == 1136073622 = (01136073599, 1)
   | base >= 0915148822 = (base - 0022, 0)
   | base == 0915148821 = (00915148799, 1)
   | base >= 0867715221 = (base - 0021, 0)
   | base == 0867715220 = (00867715199, 1)
   | base >= 0820454420 = (base - 0020, 0)
   | base == 0820454419 = (00820454399, 1)
   | base >= 0773020819 = (base - 0019, 0)
   | base == 0773020818 = (00773020799, 1)
   | base >= 0741484818 = (base - 0018, 0)
   | base == 0741484817 = (00741484799, 1)
   | base >= 0709948817 = (base - 0017, 0)
   | base == 0709948816 = (00709948799, 1)
   | base >= 0662688016 = (base - 0016, 0)
   | base == 0662688015 = (00662687999, 1)
   | base >= 0631152015 = (base - 0015, 0)
   | base == 0631152014 = (00631151999, 1)
   | base >= 0567993614 = (base - 0014, 0)
   | base == 0567993613 = (00567993599, 1)
   | base >= 0489024013 = (base - 0013, 0)
   | base == 0489024012 = (00489023999, 1)
   | base >= 0425865612 = (base - 0012, 0)
   | base == 0425865611 = (00425865599, 1)
   | base >= 0394329611 = (base - 0011, 0)
   | base == 0394329610 = (00394329599, 1)
   | base >= 0362793610 = (base - 0010, 0)
   | base == 0362793609 = (00362793599, 1)
   | base >= 0315532809 = (base - 0009, 0)
   | base == 0315532808 = (00315532799, 1)
   | base >= 0283996808 = (base - 0008, 0)
   | base == 0283996807 = (00283996799, 1)
   | base >= 0252460807 = (base - 0007, 0)
   | base == 0252460806 = (00252460799, 1)
   | base >= 0220924806 = (base - 0006, 0)
   | base == 0220924805 = (00220924799, 1)
   | base >= 0189302405 = (base - 0005, 0)
   | base == 0189302404 = (00189302399, 1)
   | base >= 0157766404 = (base - 0004, 0)
   | base == 0157766403 = (00157766399, 1)
   | base >= 0126230403 = (base - 0003, 0)
   | base == 0126230402 = (00126230399, 1)
   | base >= 0094694402 = (base - 0002, 0)
   | base == 0094694401 = (00094694399, 1)
   | base >= 0078796801 = (base - 0001, 0)
   | base == 0078796800 = (00078796799, 1)
   | otherwise          = (base - 0000, 0)

-- |
-- Show the 12-hour pariod (ante or post meridiem) of the given 24-hour hour without performing any bounds check.
getPeriod :: Hour -> (String, Hour)
getPeriod hour
   | hour == 00 = ("AM", 12)
   | hour <= 11 = ("AM", hour)
   | hour == 12 = ("PM", hour)
   | otherwise  = ("PM", hour - 12)
