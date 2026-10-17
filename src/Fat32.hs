module Fat32 (decode) where

import Data.Word (Word8,Word16,Word32)
import Control.Monad (ap,liftM)
import Text.Printf (printf)
import Data.ByteString qualified as B
import Data.ByteString.Internal (w2c)
import Data.Bits (Bits,(.&.))
import FileTree (Tree(..),Entry(..))

decode :: FilePath -> IO Tree
decode path = exec (decode1 0) path

decode1 :: W32 -> M Tree
decode1 off = do
  vol <- getVolumeInfo off
  --Print vol
  getTree vol

getTree :: VolumeInfo -> M Tree
getTree vol@VolumeInfo{rootDirFirstCluster=cn0} = deepTree cn0
  where
    deepTree :: W32 -> M Tree
    deepTree cn = do
      rs <- getDirInfo vol cn
      binds <- sequence [ do entry <- deepEntry r; pure (shortName,entry)
                        | r@DirRecord{shortName} <- rs
                        , shortName /= ".          "
                        , shortName /= "..         "
                        ]
      pure (Tree binds)

    deepEntry :: DirRecord -> M Entry
    deepEntry r@DirRecord{isDir,firstCluster=cn} = if
      | isDir -> do
          tree <- deepTree cn
          pure (ETree tree)
      | otherwise -> do
          contents <- getFileContents vol r
          pure (EFile contents)

data VolumeInfo = VolumeInfo
  { bytesPerSector :: W16
  , sectorsPerCluster :: W8
  , numberReservedSectors :: W16
  , numberOfFats :: W8
  , sectorsPerFat :: W32
  , rootDirFirstCluster :: W32 -- TODO: newtype for ClusterNumber?
  , sig :: W16
  , fatBegin :: W32
  , clusterBegin :: W32
  } deriving Show

getVolumeInfo :: W32 -> M VolumeInfo
getVolumeInfo off = do
  bytesPerSector <- get16 (off+0xB)
  sectorsPerCluster <- get8 (off+0xD)
  numberReservedSectors <- get16 (off+0xE)
  numberOfFats <- get8 (off+0x10)
  sectorsPerFat <- get32 (off+0x24)
  rootDirFirstCluster <- get32 (off+0x2C)
  sig <- get16 (off+0x1FE)
  let fatBegin = off + fromIntegral numberReservedSectors
  let clusterBegin = off + fromIntegral numberReservedSectors + (fromIntegral numberOfFats * sectorsPerFat)
  -- TODO: assert some expected values
  pure VolumeInfo { bytesPerSector
                  , sectorsPerCluster
                  , numberReservedSectors
                  , numberOfFats
                  , sectorsPerFat
                  , rootDirFirstCluster
                  , sig
                  , fatBegin
                  , clusterBegin
                  }

getDirInfo :: VolumeInfo -> W32 -> M [DirRecord]
getDirInfo vol cn = loop cn 0
  where
    loop :: W32 -> Int -> M [DirRecord]
    loop cn i = if
      | i > 15 -> do
          cn' <- stepFatChain vol cn
          loop cn' 0
      | otherwise -> do
          let off = offsetFromClusterNumber vol cn
          dr@DirRecord{kind} <- getDirRecord (off + 32 * fromIntegral i)
          case kind of
            DRK_EndOfDir -> pure []
            DRK_Unused -> loop cn (i+1)
            DRK_Long -> loop cn (i+1)
            DRK_Normal -> do
              drs <- loop cn (i+1)
              pure (dr:drs)

getDirRecord :: W32 -> M DirRecord
getDirRecord o = do
  firstByte <- get8 o
  attr <- get8 (o+0xB)
  let
    kind = if
      | firstByte == 0x00 -> DRK_EndOfDir
      | firstByte == 0xE5 -> DRK_Unused
      | (attr .&. 0xF) == 0xF -> DRK_Long
      | otherwise -> DRK_Normal
  firstClusterHi <- get16 (o+0x14)
  firstClusterLo <- get16 (o+0x1A)
  let firstCluster = mk32 HL {hi = firstClusterHi, lo = firstClusterLo}
  fileSize <- get32 (o+0x1C)
  shortName <- map toChar <$> getBytes o 11
  let isDir = (attr .&. 0x10) /= 0
  pure DirRecord { kind,attr,firstCluster,fileSize,shortName,isDir }

getFileContents :: VolumeInfo -> DirRecord -> M String
getFileContents vol dr = do
  let VolumeInfo{bytesPerSector} = vol
  let DirRecord{fileSize,firstCluster} = dr
  let bps = fromIntegral bytesPerSector
  let
    loop :: W32 -> Int -> M String
    loop cn size = if
      | size <= bps -> do
          let off = offsetFromClusterNumber vol cn
          map toChar <$> getBytes off (fromIntegral size)
      | otherwise -> do
          let off = offsetFromClusterNumber vol cn
          start <- map toChar <$> getBytes off bps
          cn' <- stepFatChain vol cn
          rest <- loop cn' (size - bps)
          pure (start ++ rest)
  loop firstCluster (fromIntegral fileSize)

stepFatChain ::VolumeInfo -> W32 -> M W32
stepFatChain vol cn = do
  let VolumeInfo{fatBegin,bytesPerSector} = vol
  let off = fatBegin * fromIntegral bytesPerSector + 4 * cn
  cn' <- get32 off
  --Print ("stepFatChain",cn,"-->",cn')
  pure cn'

offsetFromClusterNumber :: VolumeInfo -> W32 -> W32
offsetFromClusterNumber vol n = do
  let VolumeInfo{clusterBegin,bytesPerSector,sectorsPerCluster} = vol
  let lba_addr = clusterBegin + (n - 2) * fromIntegral sectorsPerCluster
  fromIntegral bytesPerSector * fromIntegral sectorsPerCluster * fromIntegral lba_addr

toChar :: W8 -> Char
toChar (W8 w) = w2c w

data DirRecord = DirRecord
  { kind :: DirRecordKind
  , attr :: W8
  , firstCluster :: W32
  , fileSize :: W32
  , shortName :: String
  , isDir :: Bool
  } deriving Show

data DirRecordKind = DRK_Normal | DRK_Long | DRK_Unused | DRK_EndOfDir
  deriving (Show,Eq)

getBytes :: W32 -> Int -> M [W8]
getBytes off n = sequence [ GetW8 (off + fromIntegral i) | i <- [0.. n-1] ]

get32 :: W32 -> M W32
get32 o = do
  lo <- get16 o
  hi <- get16 (o+2)
  pure (mk32 HL {hi,lo})

get16 :: W32 -> M W16
get16 o = do
  lo <- get8 o
  hi <- get8 (o+1)
  pure (mk16 HL{hi,lo})

get8 :: W32 -> M W8
get8 = GetW8

instance Functor M where fmap = liftM
instance Applicative M where pure = Pure; (<*>) = ap
instance Monad M where (>>=) = Bind

data M a where
  Pure :: a -> M a
  Bind :: M a -> (a -> M b) -> M b
  GetW8 :: W32 -> M W8 -- TODO: reading a whole sector should be the prim-op
  Print :: Show a => a -> M ()

exec :: M a -> FilePath -> IO a
exec m0 path = do
  the_bs <- B.readFile path
  let
    lookup :: W32 -> W8
    lookup off = W8 (B.head (B.drop (fromIntegral off) the_bs))
    go :: M a -> IO a
    go = \case
      Pure x -> pure x
      Bind m f -> do a <- go m; go (f a)
      GetW8 off -> pure (lookup off)
      Print x ->  print x
  go m0

newtype W8 = W8 Word8 deriving (Num,Integral,Real,Enum,Ord,Eq,Bits)
instance Show W8 where show (W8 w) = printf "$%02x" w

newtype W16 = W16 Word16 deriving (Num,Integral,Real,Enum,Ord,Eq)
instance Show W16 where show (W16 w) = printf "$%04x" w

newtype W32 = W32 Word32 deriving (Num,Integral,Real,Enum,Ord,Eq)
instance Show W32 where show (W32 w) = printf "$%08x" w

data HL a = HL { hi :: a, lo :: a }

mk32 :: HL W16 -> W32
mk32 HL{hi,lo} = W32 (fromIntegral hi * (256*256) + fromIntegral lo)

mk16 :: HL W8 -> W16
mk16 HL{hi,lo} = W16 (fromIntegral hi * 256 + fromIntegral lo)
