-- |
-- Module     : Data.Time.Exts.Util
-- Copyright  : 2013-2017 Enzo Haussecker
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
   | base >= 1483228800 = base + 28
   | base >= 1435708800 = base + 27
   | base >= 1341100800 = base + 26
   | base >= 1230768000 = base + 25
   | base >= 1136073600 = base + 24
   | base >= 0915148800 = base + 23
   | base >= 0867715200 = base + 22
   | base >= 0820454400 = base + 21
   | base >= 0773020800 = base + 20
   | base >= 0741484800 = base + 19
   | base >= 0709948800 = base + 18
   | base >= 0662688000 = base + 17
   | base >= 0631152000 = base + 16
   | base >= 0567993600 = base + 15
   | base >= 0489024000 = base + 14
   | base >= 0425865600 = base + 13
   | base >= 0394329600 = base + 12
   | base >= 0362793600 = base + 11
   | base >= 0315532800 = base + 10
   | base >= 0283996800 = base + 09
   | base >= 0252460800 = base + 08
   | base >= 0220924800 = base + 07
   | base >= 0189302400 = base + 06
   | base >= 0157766400 = base + 05
   | base >= 0126230400 = base + 04
   | base >= 0094694400 = base + 03
   | base >= 0078796800 = base + 02
   | base >= 0063072000 = base + 01
   | otherwise          = base + 00

-- |
-- Convert UTC seconds into Unix and leap seconds.
baseUTCToUnix :: Int64 -> (Int64, Double)
baseUTCToUnix base
   | base >= 1483228828 = (base - 028, 0)
   | base == 1483228827 = (1483228799, 1)
   | base >= 1435708827 = (base - 027, 0)
   | base == 1435708826 = (1435708799, 1)
   | base >= 1341100826 = (base - 026, 0)
   | base == 1341100825 = (1341100799, 1)
   | base >= 1230768025 = (base - 025, 0)
   | base == 1230768024 = (1230767999, 1)
   | base >= 1136073624 = (base - 024, 0)
   | base == 1136073623 = (1136073599, 1)
   | base >= 0915148823 = (base - 023, 0)
   | base == 0915148822 = (0915148799, 1)
   | base >= 0867715222 = (base - 022, 0)
   | base == 0867715221 = (0867715199, 1)
   | base >= 0820454421 = (base - 021, 0)
   | base == 0820454420 = (0820454399, 1)
   | base >= 0773020820 = (base - 020, 0)
   | base == 0773020819 = (0773020799, 1)
   | base >= 0741484819 = (base - 019, 0)
   | base == 0741484818 = (0741484799, 1)
   | base >= 0709948818 = (base - 018, 0)
   | base == 0709948817 = (0709948799, 1)
   | base >= 0662688017 = (base - 017, 0)
   | base == 0662688016 = (0662687999, 1)
   | base >= 0631152016 = (base - 016, 0)
   | base == 0631152015 = (0631151999, 1)
   | base >= 0567993615 = (base - 015, 0)
   | base == 0567993614 = (0567993599, 1)
   | base >= 0489024014 = (base - 014, 0)
   | base == 0489024013 = (0489023999, 1)
   | base >= 0425865613 = (base - 013, 0)
   | base == 0425865612 = (0425865599, 1)
   | base >= 0394329612 = (base - 012, 0)
   | base == 0394329611 = (0394329599, 1)
   | base >= 0362793611 = (base - 011, 0)
   | base == 0362793610 = (0362793599, 1)
   | base >= 0315532810 = (base - 010, 0)
   | base == 0315532809 = (0315532799, 1)
   | base >= 0283996809 = (base - 009, 0)
   | base == 0283996808 = (0283996799, 1)
   | base >= 0252460808 = (base - 008, 0)
   | base == 0252460807 = (0252460799, 1)
   | base >= 0220924807 = (base - 007, 0)
   | base == 0220924806 = (0220924799, 1)
   | base >= 0189302406 = (base - 006, 0)
   | base == 0189302405 = (0189302399, 1)
   | base >= 0157766405 = (base - 005, 0)
   | base == 0157766404 = (0157766399, 1)
   | base >= 0126230404 = (base - 004, 0)
   | base == 0126230403 = (0126230399, 1)
   | base >= 0094694403 = (base - 003, 0)
   | base == 0094694402 = (0094694399, 1)
   | base >= 0078796802 = (base - 002, 0)
   | base == 0078796801 = (0078796799, 1)
   | base >= 0063072001 = (base - 001, 0)
   | base == 0063072000 = (0063071999, 1)
   | otherwise          = (base - 000, 0)

-- |
-- Show the 12-hour pariod (ante or post meridiem) of the given 24-hour hour without performing any bounds check.
getPeriod :: Hour -> (String, Hour)
getPeriod hour
   | hour == 00 = ("AM", 12)
   | hour <= 11 = ("AM", hour)
   | hour == 12 = ("PM", hour)
   | otherwise  = ("PM", hour - 12)
