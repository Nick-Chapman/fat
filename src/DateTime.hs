
module DateTime
  ( Date, Time
  , wellKnownDate, wellKnownTime
  , dateWord, timeWord
  ) where

import Types (W16)
import Data.Bits ((.|.),shiftL)

wellKnownDate :: Date
wellKnownDate =  Date { year = 1997, month = 12, day = 29 }

wellKnownTime :: Time
wellKnownTime = Time { hour = 12, minute = 25, second = 02 }

data Date = Date
  { year  :: Int -- (7 bits) 0..119 (1980..2099)
  , month :: Int -- (4 bits) 1..12
  , day   :: Int -- (5 bits) 1..31
  }
  deriving Show

data Time = Time
  { hour   :: Int -- (5 bits) 0..23
  , minute :: Int -- (6 bits) 0..59
  , second :: Int -- (5 bits) 0..29 (two seconds)
  }
  deriving Show

dateWord :: Date -> W16
dateWord Date{year,month,day} =
  (((fromIntegral year - 1980) `mod` 128) `shiftL` 9) .|.
  ((fromIntegral month `mod` 16) `shiftL` 5) .|.
  (fromIntegral day `mod` 32)

timeWord :: Time -> W16
timeWord Time{hour,minute,second} =
  ((fromIntegral hour `mod` 32) `shiftL` 11) .|.
  ((fromIntegral minute `mod` 64) `shiftL` 5) .|.
  (fromIntegral (second `div` 2) `mod` 32)
