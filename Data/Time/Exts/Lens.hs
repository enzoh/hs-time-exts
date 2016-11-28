-- |
-- Module     : Data.Time.Exts.Lens
-- Copyright  : 2013-2016 Enzo Haussecker
-- License    : BSD3
-- Maintainer : Enzo Haussecker <enzo@sovereign.io>
-- Stability  : Stable
--
-- A lens-based interface to date and time data structures.

{-# LANGUAGE TemplateHaskell #-}

{-# OPTIONS -fno-warn-missing-signatures #-}

module Data.Time.Exts.Lens where

import Data.Time.Exts.Base
import Lens.Simple

makeLenses ''DateStruct
makeLenses ''TimeStruct
makeLenses ''DateTimeStruct
makeLenses ''LocalDateStruct
makeLenses ''LocalTimeStruct
makeLenses ''LocalDateTimeStruct
