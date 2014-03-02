---------------------------------------------------------------
-- Copyright (c) 2014, Enzo Haussecker. All rights reserved. --
---------------------------------------------------------------

{-# LANGUAGE DeriveDataTypeable     #-}
{-# LANGUAGE DeriveGeneric          #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE NamedFieldPuns         #-}
{-# LANGUAGE RecordWildCards        #-}
{-# OPTIONS -Wall                   #-}
{-# OPTIONS -fno-warn-type-defaults #-}

-- | Location and time zone constructors and functions.
module Data.Time.Exts.Zone (

 -- ** Locations
       City(..)
     , cities

 -- ** Olson Database
     , getOlsonFile
     , olsonFiles

 -- ** Time Zones
     , TimeZone(..)
     , utc
     , getUTCOffset
     , utcOffsets

 -- ** Abbreviations
     , TimeZoneAbbr(..)
     , abbreviate
     , unabbreviate
     , abbreviations

     ) where

import Control.Arrow    (first)
import Data.Aeson       (FromJSON, ToJSON)
import Data.Convertible (convError, convert, Convertible(..))
import Data.Map.Strict  (Map, (!), fromDistinctAscList)
import Data.Typeable    (Typeable)
import GHC.Generics     (Generic)
import System.Random    (Random(..))

-- | Cities from around the world.
data City =
     Aden         -- ^ Yemeni Republic
   | Amman        -- ^ Hashemite Kingdom of Jordan
   | Anchorage    -- ^ United States of America
   | Auckland     -- ^ New Zealand
   | Baghdad      -- ^ Republic of Iraq
   | Berlin       -- ^ Federal Republic of Germany
   | Brussels     -- ^ Kingdom of Belgium
   | Bujumbura    -- ^ Republic of Burundi
   | Cairo        -- ^ Arab Republic of Egypt
   | Chicago      -- ^ United States of America
   | Damascus     -- ^ Syrian Arab Republic
   | Denver       -- ^ United States of America
   | Doha         -- ^ State of Qatar
   | Gaborone     -- ^ Republic of Botswana
   | Hong_Kong    -- ^ People's Republic of China
   | Honolulu     -- ^ United States of America
   | Johannesburg -- ^ Republic of South Africa
   | Kabul        -- ^ Islamic Republic of Afghanistan
   | Karachi      -- ^ Islamic Republic of Pakistan
   | Kinshasa     -- ^ Democratic Republic of the Congo
   | Kolkata      -- ^ Republic of India
   | Kuwait_City  -- ^ State of Kuwait
   | London       -- ^ United Kingdom of Great Britain and Northern Ireland
   | Los_Angeles  -- ^ United States of America
   | Luanda       -- ^ Republic of Angola
   | Manama       -- ^ Kingdom of Bahrain
   | Minsk        -- ^ Republic of Belarus
   | Mogadishu    -- ^ Federal Republic of Somalia
   | Moscow       -- ^ Russian Federation
   | New_York     -- ^ United States of America
   | Oslo         -- ^ Kingdom of Norway
   | Ouagadougou  -- ^ Burkina Faso
   | Paris        -- ^ French Republic
   | Pyongyang    -- ^ Democratic People's Republic of Korea
   | Riyadh       -- ^ Kingdom of Saudi Arabia
   | Sao_Paulo    -- ^ Federative Republic of Brazil
   | Sarajevo     -- ^ Bosnia and Herzegovina
   | Seoul        -- ^ Republic of Korea
   | Shanghai     -- ^ People's Republic of China
   | Singapore    -- ^ Republic of Singapore
   | Sofia        -- ^ Republic of Bulgaria
   | Stockholm    -- ^ Kingdom of Sweden
   | Tehran       -- ^ Islamic Republic of Iran
   | Tel_Aviv     -- ^ State of Israel
   | Tirana       -- ^ Republic of Albania
   | Tokyo        -- ^ Japan
   | Toronto      -- ^ Canada
   | Universal    -- ^ International Territory
   | Vienna       -- ^ Republic of Austria
   | Zurich       -- ^ Swiss Confederation
   deriving (Eq,Enum,Generic,Ord,Read,Show,Typeable)

instance Bounded City where
   minBound = Aden
   maxBound = Zurich

instance FromJSON City

instance Random City where
   random        = first toEnum . randomR (fromEnum (minBound::City), fromEnum (maxBound::City))
   randomR (a,b) = first toEnum . randomR (fromEnum a, fromEnum b)

instance ToJSON City

-- | A list of cities in alphabetical order.
cities :: [City]
cities =
   [ Aden
   , Amman
   , Anchorage
   , Auckland
   , Baghdad
   , Berlin
   , Brussels
   , Bujumbura
   , Cairo
   , Chicago
   , Damascus
   , Denver
   , Doha
   , Gaborone
   , Hong_Kong
   , Honolulu
   , Johannesburg
   , Kabul
   , Karachi
   , Kinshasa
   , Kolkata
   , Kuwait_City
   , London
   , Los_Angeles
   , Luanda
   , Manama
   , Minsk
   , Mogadishu
   , Moscow
   , New_York
   , Oslo
   , Ouagadougou
   , Paris
   , Pyongyang
   , Riyadh
   , Sao_Paulo
   , Sarajevo
   , Seoul
   , Shanghai
   , Singapore
   , Sofia
   , Stockholm
   , Tehran
   , Tel_Aviv
   , Tirana
   , Tokyo
   , Toronto
   , Universal
   , Vienna
   , Zurich
   ]

-- | Get the Olson file associated with the given city.
getOlsonFile :: City -> FilePath
getOlsonFile = (!) olsonFiles

-- | A map from cities to Olson file paths.
olsonFiles :: Map City FilePath
olsonFiles = fromDistinctAscList
   [ (Aden,"/usr/share/zoneinfo/Asia/Aden")
   , (Amman,"/usr/share/zoneinfo/Asia/Amman")
   , (Anchorage,"/usr/share/zoneinfo/America/Anchorage")
   , (Auckland,"/usr/share/zoneinfo/Pacific/Auckland")
   , (Baghdad,"/usr/share/zoneinfo/Asia/Baghdad")
   , (Berlin,"/usr/share/zoneinfo/Europe/Berlin")
   , (Brussels,"/usr/share/zoneinfo/Europe/Brussels")
   , (Bujumbura,"/usr/share/zoneinfo/Africa/Bujumbura")
   , (Cairo,"/usr/share/zoneinfo/Africa/Cairo")
   , (Chicago,"/usr/share/zoneinfo/America/Chicago")
   , (Damascus,"/usr/share/zoneinfo/Asia/Damascus")
   , (Denver,"/usr/share/zoneinfo/America/Denver")
   , (Doha,"/usr/share/zoneinfo/Asia/Qatar")
   , (Gaborone,"/usr/share/zoneinfo/Africa/Gaborone")
   , (Hong_Kong,"/usr/share/zoneinfo/Asia/Hong_Kong")
   , (Honolulu,"/usr/share/zoneinfo/Pacific/Honolulu")
   , (Johannesburg,"/usr/share/zoneinfo/Africa/Johannesburg")
   , (Kabul,"/usr/share/zoneinfo/Asia/Kabul")
   , (Karachi,"/usr/share/zoneinfo/Asia/Karachi")
   , (Kinshasa,"/usr/share/zoneinfo/Africa/Kinshasa")
   , (Kolkata,"/usr/share/zoneinfo/Asia/Kolkata")
   , (Kuwait_City,"/usr/share/zoneinfo/Asia/Kuwait")
   , (London,"/usr/share/zoneinfo/Europe/London")
   , (Los_Angeles,"/usr/share/zoneinfo/America/Los_Angeles")
   , (Luanda,"/usr/share/zoneinfo/Africa/Luanda")
   , (Manama,"/usr/share/zoneinfo/Asia/Bahrain")
   , (Minsk,"/usr/share/zoneinfo/Europe/Minsk")
   , (Mogadishu,"/usr/share/zoneinfo/Africa/Mogadishu")
   , (Moscow,"/usr/share/zoneinfo/Europe/Moscow")
   , (New_York,"/usr/share/zoneinfo/America/New_York")
   , (Oslo,"/usr/share/zoneinfo/Europe/Oslo")
   , (Ouagadougou,"/usr/share/zoneinfo/Africa/Ouagadougou")
   , (Paris,"/usr/share/zoneinfo/Europe/Paris")
   , (Pyongyang,"/usr/share/zoneinfo/Asia/Pyongyang")
   , (Riyadh,"/usr/share/zoneinfo/Asia/Riyadh")
   , (Sao_Paulo,"/usr/share/zoneinfo/America/Sao_Paulo")
   , (Sarajevo,"/usr/share/zoneinfo/Europe/Sarajevo")
   , (Seoul,"/usr/share/zoneinfo/Asia/Seoul")
   , (Shanghai,"/usr/share/zoneinfo/Asia/Shanghai")
   , (Singapore,"/usr/share/zoneinfo/Asia/Singapore")
   , (Sofia,"/usr/share/zoneinfo/Europe/Sofia")
   , (Stockholm,"/usr/share/zoneinfo/Europe/Stockholm")
   , (Tehran,"/usr/share/zoneinfo/Asia/Tehran")
   , (Tel_Aviv,"/usr/share/zoneinfo/Asia/Tel_Aviv")
   , (Tirana,"/usr/share/zoneinfo/Europe/Tirane")
   , (Tokyo,"/usr/share/zoneinfo/Asia/Tokyo")
   , (Toronto,"/usr/share/zoneinfo/America/Toronto")
   , (Universal,"/usr/share/zoneinfo/Universal")
   , (Vienna,"/usr/share/zoneinfo/Europe/Vienna")
   , (Zurich,"/usr/share/zoneinfo/Europe/Zurich")
   ]

-- | Time zones from around the world.
data TimeZone =
     Afghanistan_Time
   | Alaska_Daylight_Time
   | Alaska_Hawaii_Daylight_Time
   | Alaska_Hawaii_Standard_Time
   | Alaska_Standard_Time
   | Arabia_Daylight_Time
   | Arabia_Standard_Time
   | Brasilia_Summer_Time
   | Brasilia_Time
   | British_Summer_Time
   | Central_Africa_Time
   | Central_Daylight_Time
   | Central_European_Summer_Time
   | Central_European_Time
   | Central_Standard_Time
   | China_Daylight_Time
   | China_Standard_Time
   | Coordinated_Universal_Time
   | East_Africa_Time
   | Eastern_Daylight_Time
   | Eastern_European_Summer_Time
   | Eastern_European_Time
   | Eastern_Standard_Time
   | Further_Eastern_European_Time
   | Greenwich_Mean_Time
   | Gulf_Standard_Time
   | Hawaii_Aleutian_Standard_Time
   | Hong_Kong_Summer_Time
   | Hong_Kong_Time
   | India_Standard_Time
   | Iran_Daylight_Time
   | Iran_Standard_Time
   | Israel_Daylight_Time
   | Israel_Standard_Time
   | Japan_Standard_Time
   | Karachi_Time
   | Korea_Daylight_Time
   | Korea_Standard_Time
   | Moscow_Daylight_Time
   | Moscow_Standard_Time
   | Mountain_Daylight_Time
   | Mountain_Standard_Time
   | New_Zealand_Daylight_Time
   | New_Zealand_Standard_Time
   | Pacific_Daylight_Time
   | Pacific_Standard_Time
   | Pakistan_Standard_Time
   | Pakistan_Summer_Time
   | Singapore_Time
   | South_Africa_Standard_Time
   | West_Africa_Time
   | Yukon_Standard_Time
   deriving (Eq,Enum,Generic,Ord,Read,Show,Typeable)

instance Bounded TimeZone where
   minBound = Afghanistan_Time
   maxBound = Yukon_Standard_Time

instance FromJSON TimeZone

instance Random TimeZone where
   random        = first toEnum . randomR (fromEnum (minBound::TimeZone), fromEnum (maxBound::TimeZone))
   randomR (a,b) = first toEnum . randomR (fromEnum a, fromEnum b)

instance ToJSON TimeZone

-- | The UTC time zone.
utc :: TimeZone 
utc = Coordinated_Universal_Time

-- | Get the UTC offset (in minutes) for the given time zone.
getUTCOffset :: Num a => TimeZone -> a
getUTCOffset = (!) utcOffsets

-- | A map from time zones to UTC offsets (in minutes).
utcOffsets :: Num a => Map TimeZone a
utcOffsets = fromDistinctAscList
   [ (Afghanistan_Time,270)
   , (Alaska_Daylight_Time,-480)
   , (Alaska_Hawaii_Daylight_Time,-540)
   , (Alaska_Hawaii_Standard_Time,-600)
   , (Alaska_Standard_Time,-540)
   , (Arabia_Daylight_Time,240)
   , (Arabia_Standard_Time,180)
   , (Brasilia_Summer_Time,-120)
   , (Brasilia_Time,-180)
   , (British_Summer_Time,60)
   , (Central_Africa_Time,120)
   , (Central_Daylight_Time,-300)
   , (Central_European_Summer_Time,120)
   , (Central_European_Time,60)
   , (Central_Standard_Time,-360)
   , (China_Daylight_Time,540)
   , (China_Standard_Time,480)
   , (Coordinated_Universal_Time,0)
   , (East_Africa_Time,180)
   , (Eastern_Daylight_Time,-240)
   , (Eastern_European_Summer_Time,180)
   , (Eastern_European_Time,120)
   , (Eastern_Standard_Time,-300)
   , (Further_Eastern_European_Time,180)
   , (Greenwich_Mean_Time,0)
   , (Gulf_Standard_Time,240)
   , (Hawaii_Aleutian_Standard_Time,-600)
   , (Hong_Kong_Summer_Time,540)
   , (Hong_Kong_Time,480)
   , (India_Standard_Time,330)
   , (Iran_Daylight_Time,270)
   , (Iran_Standard_Time,210)
   , (Israel_Daylight_Time,180)
   , (Israel_Standard_Time,120)
   , (Japan_Standard_Time,540)
   , (Karachi_Time,300)
   , (Korea_Daylight_Time,600)
   , (Korea_Standard_Time,540)
   , (Moscow_Daylight_Time,240)
   , (Moscow_Standard_Time,240)
   , (Mountain_Daylight_Time,-360)
   , (Mountain_Standard_Time,-420)
   , (New_Zealand_Daylight_Time,780)
   , (New_Zealand_Standard_Time,720)
   , (Pacific_Daylight_Time,-420)
   , (Pacific_Standard_Time,-480)
   , (Pakistan_Standard_Time,300)
   , (Pakistan_Summer_Time,360)
   , (Singapore_Time,480)
   , (South_Africa_Standard_Time,120)
   , (West_Africa_Time,60)
   , (Yukon_Standard_Time,-540)
   ]

-- | A time zone abbreviation.
data TimeZoneAbbr = TimeZoneAbbr {
     abbr_city :: City   -- ^ reference location
   , abbr_str  :: String -- ^ time zone abbreviation string
   } deriving (Eq,Generic,Typeable)

instance Convertible TimeZoneAbbr TimeZone where
   safeConvert abbr@TimeZoneAbbr{..} = 
     case abbr_str of
       "AFT"  -> Right Afghanistan_Time
       "AHDT" -> Right Alaska_Hawaii_Daylight_Time
       "AHST" -> Right Alaska_Hawaii_Standard_Time
       "AKDT" -> Right Alaska_Daylight_Time
       "AKST" -> Right Alaska_Standard_Time
       "ADT"  -> Right Arabia_Daylight_Time
       "AST"  -> Right Arabia_Standard_Time
       "BRST" -> Right Brasilia_Summer_Time
       "BRT"  -> Right Brasilia_Time
       "BST"  -> Right British_Summer_Time
       "CAT"  -> Right Central_Africa_Time
       "CDT"  -> case abbr_city of
                   Chicago   -> Right Central_Daylight_Time
                   Shanghai  -> Right China_Daylight_Time
                   _         -> collision
       "CEST" -> Right Central_European_Summer_Time
       "CET"  -> Right Central_European_Time
       "CST"  -> case abbr_city of
                   Chicago   -> Right Central_Standard_Time
                   Shanghai  -> Right China_Standard_Time
                   _         -> collision
       "EAT"  -> Right East_Africa_Time
       "EDT"  -> Right Eastern_Daylight_Time
       "EEST" -> Right Eastern_European_Summer_Time
       "EET"  -> Right Eastern_European_Time
       "EST"  -> Right Eastern_Standard_Time
       "FET"  -> Right Further_Eastern_European_Time
       "GMT"  -> Right Greenwich_Mean_Time
       "GST"  -> Right Gulf_Standard_Time
       "HST"  -> Right Hawaii_Aleutian_Standard_Time
       "HKST" -> Right Hong_Kong_Summer_Time
       "HKT"  -> Right Hong_Kong_Time
       "IDT"  -> Right Israel_Daylight_Time
       "IRDT" -> Right Iran_Daylight_Time
       "IRST" -> Right Iran_Standard_Time
       "IST"  -> case abbr_city of
                   Kolkata   -> Right India_Standard_Time
                   Tel_Aviv  -> Right Israel_Standard_Time
                   _         -> collision
       "JST"  -> Right Japan_Standard_Time
       "KART" -> Right Karachi_Time
       "KDT"  -> Right Korea_Daylight_Time
       "KST"  -> Right Korea_Standard_Time
       "MDT"  -> Right Mountain_Daylight_Time
       "MSD"  -> Right Moscow_Daylight_Time
       "MSK"  -> Right Moscow_Standard_Time
       "MST"  -> Right Mountain_Standard_Time
       "NZDT" -> Right New_Zealand_Daylight_Time
       "NZST" -> Right New_Zealand_Standard_Time
       "PDT"  -> Right Pacific_Daylight_Time
       "PKST" -> Right Pakistan_Summer_Time
       "PKT"  -> Right Pakistan_Standard_Time
       "PST"  -> Right Pacific_Standard_Time
       "SAST" -> Right South_Africa_Standard_Time
       "SGT"  -> Right Singapore_Time
       "UTC"  -> Right Coordinated_Universal_Time
       "WAT"  -> Right West_Africa_Time
       "YST"  -> Right Yukon_Standard_Time
       _      ->         convError "undefined time zone abbreviation string" abbr
       where collision = convError "reference location collision"            abbr

instance Convertible TimeZone TimeZoneAbbr where
   safeConvert = Right . (!) abbreviations

instance FromJSON TimeZoneAbbr

instance Show TimeZoneAbbr where
   show TimeZoneAbbr{abbr_str} = abbr_str

instance ToJSON TimeZoneAbbr

-- | Abbreviate a time zone.
abbreviate :: TimeZone -> TimeZoneAbbr
abbreviate = convert

-- | Unabbreviate a time zone.
unabbreviate :: TimeZoneAbbr -> TimeZone
unabbreviate = convert

-- | A map from time zones to time zone abbreviations.
abbreviations :: Map TimeZone TimeZoneAbbr
abbreviations = fromDistinctAscList
   [ (Afghanistan_Time,TimeZoneAbbr Kabul "AFT")
   , (Alaska_Daylight_Time,TimeZoneAbbr Anchorage "AKDT")
   , (Alaska_Hawaii_Daylight_Time,TimeZoneAbbr Anchorage "AHDT")
   , (Alaska_Hawaii_Standard_Time,TimeZoneAbbr Anchorage "AHST")
   , (Alaska_Standard_Time,TimeZoneAbbr Anchorage "AKST")
   , (Arabia_Daylight_Time,TimeZoneAbbr Baghdad "ADT")
   , (Arabia_Standard_Time,TimeZoneAbbr Riyadh "AST")
   , (Brasilia_Summer_Time,TimeZoneAbbr Sao_Paulo "BRST")
   , (Brasilia_Time,TimeZoneAbbr Sao_Paulo "BRT")
   , (British_Summer_Time,TimeZoneAbbr London "BST")
   , (Central_Africa_Time,TimeZoneAbbr Gaborone "CAT")
   , (Central_Daylight_Time,TimeZoneAbbr Chicago "CDT")
   , (Central_European_Summer_Time,TimeZoneAbbr Paris "CEST")
   , (Central_European_Time,TimeZoneAbbr Paris "CET")
   , (Central_Standard_Time,TimeZoneAbbr Chicago "CST")
   , (China_Daylight_Time,TimeZoneAbbr Shanghai "CDT")
   , (China_Standard_Time,TimeZoneAbbr Shanghai "CST")
   , (Coordinated_Universal_Time,TimeZoneAbbr Universal "UTC")
   , (East_Africa_Time,TimeZoneAbbr Mogadishu "EAT")
   , (Eastern_Daylight_Time,TimeZoneAbbr New_York "EDT")
   , (Eastern_European_Summer_Time,TimeZoneAbbr Sofia "EEST")
   , (Eastern_European_Time,TimeZoneAbbr Sofia "EET")
   , (Eastern_Standard_Time,TimeZoneAbbr New_York "EST")
   , (Further_Eastern_European_Time,TimeZoneAbbr Minsk "FET")
   , (Greenwich_Mean_Time,TimeZoneAbbr London "GMT")
   , (Gulf_Standard_Time,TimeZoneAbbr Manama "GST")
   , (Hawaii_Aleutian_Standard_Time,TimeZoneAbbr Honolulu "HST")
   , (Hong_Kong_Summer_Time,TimeZoneAbbr Hong_Kong "HKST")
   , (Hong_Kong_Time,TimeZoneAbbr Hong_Kong "HKT")
   , (India_Standard_Time,TimeZoneAbbr Kolkata "IST")
   , (Iran_Daylight_Time,TimeZoneAbbr Tehran "IRDT")
   , (Iran_Standard_Time,TimeZoneAbbr Tehran "IRST")
   , (Israel_Daylight_Time,TimeZoneAbbr Tel_Aviv "IDT")
   , (Israel_Standard_Time,TimeZoneAbbr Tel_Aviv "IST")
   , (Japan_Standard_Time,TimeZoneAbbr Tokyo "JST")
   , (Karachi_Time,TimeZoneAbbr Karachi "KART")
   , (Korea_Daylight_Time,TimeZoneAbbr Seoul "KDT")
   , (Korea_Standard_Time,TimeZoneAbbr Seoul "KST")
   , (Moscow_Daylight_Time,TimeZoneAbbr Moscow "MSD")
   , (Moscow_Standard_Time,TimeZoneAbbr Moscow "MSK")
   , (Mountain_Daylight_Time,TimeZoneAbbr Denver "MDT")
   , (Mountain_Standard_Time,TimeZoneAbbr Denver "MST")
   , (New_Zealand_Daylight_Time,TimeZoneAbbr Auckland "NZDT")
   , (New_Zealand_Standard_Time,TimeZoneAbbr Auckland "NZST")
   , (Pacific_Daylight_Time,TimeZoneAbbr Los_Angeles "PDT")
   , (Pacific_Standard_Time,TimeZoneAbbr Los_Angeles "PST")
   , (Pakistan_Standard_Time,TimeZoneAbbr Karachi "PKT")
   , (Pakistan_Summer_Time,TimeZoneAbbr Karachi "PKST")
   , (Singapore_Time,TimeZoneAbbr Singapore "SGT")
   , (South_Africa_Standard_Time,TimeZoneAbbr Johannesburg "SAST")
   , (West_Africa_Time,TimeZoneAbbr Luanda "WAT")
   , (Yukon_Standard_Time,TimeZoneAbbr Anchorage "YST")
   ]
