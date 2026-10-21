
module Types
  ( W8, W16, W32, mk16, mk32, toChar
  , HL(..)
  , SectorIndex
  , Sector, mkSector, sectorSize
  , SectorOffset, readInSector
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

toChar :: W8 -> Char
toChar (W8 w) = w2c w

newtype SectorIndex = SectorIndex W16
  deriving (Num,Integral,Real,Enum,Ord,Eq,Bits,Show)

newtype SectorOffset = SectorOffset W16 -- TODO: actually only need 9 bits
  deriving (Num,Integral,Real,Enum,Ord,Eq,Bits,Show)

newtype Sector = Sector B.ByteString

sectorSize :: Int
sectorSize = 512

mkSector :: B.ByteString -> SectorIndex -> Sector
mkSector bs (SectorIndex n) = Sector (B.drop (sectorSize * fi n) bs)

readInSector :: Sector -> SectorOffset -> W8
readInSector (Sector bs) (SectorOffset n) = W8 $ B.head (B.drop (fi n) bs)
