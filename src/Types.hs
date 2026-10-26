
module Types
  ( W8, W16, W32, mk16, mk32, toChar, split16, split32
  , HL(..)
  , SectorIndex
  , Sector, sectorSize, mkSector, deSector
  , SectorOffset, readInSector, writeInSector
  ) where

import Data.Bits (Bits)
import Data.ByteString qualified as B
import Data.ByteString.Internal (w2c)
import Data.Word (Word8,Word16,Word32)
import Prelude hiding (interact)
import Text.Printf (printf)

fi :: (Integral a, Num b) => a -> b
fi x = fromIntegral x

newtype W8 = W8 Word8
  deriving (Num,Integral,Real,Enum,Ord,Eq,Bits)
instance Show W8 where show (W8 w) = printf "$%02x" w

newtype W16 = W16 Word16
  deriving (Num,Integral,Real,Enum,Ord,Eq,Bits)
instance Show W16 where show (W16 w) = printf "$%04x" w

newtype W32 = W32 Word32 deriving (Num,Integral,Real,Enum,Ord,Eq)
instance Show W32 where show (W32 w) = printf "$%08x" w

data HL a = HL { hi :: a, lo :: a }

mk32 :: HL W16 -> W32
mk32 HL{hi,lo} = W32 (fi hi * (256*256) + fi lo)

mk16 :: HL W8 -> W16
mk16 HL{hi,lo} = W16 (fi hi * 256 + fi lo)

split16 :: W16 -> HL W8
split16 w = HL { lo = fi (w `mod` 256), hi = fi (w `div` 256) }

split32 :: W32 -> HL W16
split32 w = HL { lo = fi (w `mod` (256*256)), hi = fi (w `div` (256*256)) }

toChar :: W8 -> Char
toChar (W8 w) = w2c w

newtype SectorIndex = SectorIndex W16
  deriving (Num,Integral,Real,Enum,Ord,Eq,Bits,Show)

newtype SectorOffset = SectorOffset W16 -- TODO: actually only need 9 bits
  deriving (Num,Integral,Real,Enum,Ord,Eq,Bits,Show)

sectorSize :: Int
sectorSize = 512

newtype Sector = Sector [W8] -- TODO: Use mutable array representation

mkSector :: B.ByteString -> Sector
mkSector bs = Sector (map fromIntegral (B.unpack bs))

deSector :: Sector -> B.ByteString
deSector (Sector bytes) = B.pack (map fromIntegral bytes)

readInSector :: Sector -> SectorOffset -> W8
readInSector (Sector bs) (SectorOffset n) = bs !! fi n

writeInSector :: Sector -> SectorOffset -> W8 -> Sector
writeInSector (Sector bs) (SectorOffset n) b = Sector bs'
  where
    bs' = take (fi n) bs ++ [b] ++ drop (fi n + 1) bs -- terrible algorihmics!
