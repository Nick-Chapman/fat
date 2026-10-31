
module DateTime
  ( Date, Time
  , parseDate, parseTime
  , wellKnownDate, wellKnownTime
  , dateWord, timeWord, fineTimeByte
  ) where

import Types (W8,W16)
import Data.Bits ((.|.),shiftL)
import Data.List.Split (splitOn)

parseDate :: String -> Date
parseDate str = do
  case splitOn "-" str of
    [y,m,d] ->  do
      let year = read y
      let month = read m
      let day = read d
      Date {year, month, day}
    _ -> do
      error $ "parseDate: '" ++ str ++ "'"

parseTime :: String -> Time
parseTime str = do
  case splitOn ":" str of
    [h,m,s_h] ->  do
      let hour = read h
      let minute = read m
      case splitOn "." s_h of
        [s,c] -> do
          let second = read s
          let centiseconds = read c
          Time {hour, minute, second, centiseconds}
        _ -> err
    _ -> err
  where err = error $ "parseTime: '" ++ str ++ "'"

wellKnownDate :: Date
wellKnownDate =  Date { year = 1997, month = 12, day = 29 }

wellKnownTime :: Time
wellKnownTime = Time { hour = 12, minute = 25, second = 02, centiseconds = 0 }

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
  , centiseconds :: Int -- 0..99
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

fineTimeByte :: Time -> W8
fineTimeByte Time{second,centiseconds} = do
  fromIntegral (centiseconds + if second `mod` 2 == 0 then 0 else 100)
