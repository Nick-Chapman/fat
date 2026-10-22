
module Fat16
  ( VolumeInfo
  , DirRecord(..) -- TODO: dont expose this
  , CN
  , getVolumeInfo
  , getRootDirInfo
  , getDirInfo
  , getFileContents

  , devMutM
  ) where

import TheDisk (M (ReadSector,WriteSector))
import Control.Exception (assert)
import Control.Monad.IO.Class (liftIO)
import Data.Bits ((.&.))
import Prelude hiding (interact)
import Types

----------------------------------------------------------------------
-- DEV: echo xxx > yyy (at root) -- match the bytes changed
-- start by trying to change any bytes!
devMutM :: M ()
devMutM = do
  --let si :: SectorIndex = fi (0xc94a00 `div` 512 :: Int)
  let si = 0x64a5
  liftIO $ print si

  getBytes si 0 5 >>= liftIO.print
  set8 si 1 0x79
  getBytes si 0 5 >>= liftIO.print

  pure ()


----------------------------------------------------------------------

fi :: (Integral a, Num b) => a -> b
fi x = fromIntegral x

bytesPerDirEntry :: Int
bytesPerDirEntry = 32

data VolumeInfo = VolumeInfo
  { sectorsPerCluster :: W8
--  , numberReservedSectors :: W16
--  , numberOfFats :: W8
--  , numberOfRootEntries :: W16
--  , sectorsPerFat :: W16
--  , sig :: W16
--  , sectorsForRootDir :: W16
  , fatBegin :: SectorIndex
  , rootDirBegin :: SectorIndex
  , clusterBegin :: SectorIndex
  } deriving Show


getVolumeInfo :: SectorIndex -> M VolumeInfo
getVolumeInfo si = do
  bytesPerSector <- get16 si 0xB
  sectorsPerCluster <- get8 si 0xD
  numberReservedSectors <- get16 si 0xE
  numberOfFats <- get8 si 0x10
  numberOfRootEntries <- get16 si 0x11
  sectorsPerFat <- get16 si 0x16
  sig <- get16 si 0x1FE

  assert (fi bytesPerSector == sectorSize) $ do
  --assert (sectorsPerCluster == 1) $ do
  assert (numberOfFats == 2) $ do
  --assert (numberReservedSectors == 1) $ do
  assert (sig == 0xAA55) $ do

  let numberOfDirEntriesPerSector = fi (sectorSize `div` bytesPerDirEntry)
  assert (numberOfDirEntriesPerSector == 16) $ do
  assert (numberOfRootEntries `mod` numberOfDirEntriesPerSector == 0) $ do
  let sectorsForRootDir = numberOfRootEntries `div` numberOfDirEntriesPerSector

  let fatBegin = si + fi numberReservedSectors
  let rootDirBegin = fatBegin + fi sectorsPerFat * fi numberOfFats
  let clusterBegin = rootDirBegin + fi sectorsForRootDir

  pure VolumeInfo { sectorsPerCluster
                  --, numberReservedSectors
                  --, numberOfFats
                  --, numberOfRootEntries
                  --, sectorsPerFat
                  --, sig
                  --, sectorsForRootDir
                  , fatBegin
                  , rootDirBegin
                  , clusterBegin
                  }

getRootDirInfo :: VolumeInfo -> M [DirRecord]
getRootDirInfo VolumeInfo{rootDirBegin} = loop "" rootDirBegin 0
  where
    loop :: String -> SectorIndex -> Int -> M [DirRecord]
    loop acc si i = if
      | i >= 16 -> do
          loop acc (si+1) 0
      | otherwise -> do
          let so = fi bytesPerDirEntry * fi i
          dr@DirRecord{kind} <- getDirRecord acc si so
          case kind of
            DRK_EndOfDir -> pure []
            DRK_Unused -> loop acc si (i+1)
            DRK_Long -> do
              part <- getLongNamePart si so
              loop (part++acc) si (i+1)
            DRK_Normal -> do
              drs <- loop "" si (i+1)
              pure (dr:drs)

getDirInfo :: VolumeInfo -> CN -> M [DirRecord]
getDirInfo vol cn = loop "" cn 0
  where
    VolumeInfo{sectorsPerCluster} = vol
    recordsPerCluster = 16 * fromIntegral sectorsPerCluster
    loop :: String -> CN -> Int -> M [DirRecord]
    loop acc cn i = if
      | i >= recordsPerCluster -> do
          cn' <- stepFatChain vol cn
          loop acc cn' 0
      | otherwise -> do
          let si = siFromClusterNumber vol cn
          let so = fi (bytesPerDirEntry * i)
          dr@DirRecord{kind} <- getDirRecord acc si so
          case kind of
            DRK_EndOfDir -> pure []
            DRK_Unused -> loop acc cn (i+1)
            DRK_Long -> do
              part <- getLongNamePart si so
              loop (part++acc) cn (i+1)
            DRK_Normal -> do
              drs <- loop "" cn (i+1)
              pure (dr:drs)

getLongNamePart :: SectorIndex -> SectorOffset -> M String
getLongNamePart si so = do
  bs <- sequence [ get8 si (so+i) | i <- [1,3,5,7,9, 14,16,18,20,22,24, 28,30] ]
  pure [ toChar b | b <- takeWhile (/=0) bs ]

getDirRecord :: String -> SectorIndex -> SectorOffset -> M DirRecord
getDirRecord acc si so = do
  firstByte <- get8 si so
  attr <- get8 si (so+0xB)
  let
    kind = if
      | firstByte == 0x00 -> DRK_EndOfDir
      | firstByte == 0xE5 -> DRK_Unused
      | (attr .&. 0xF) == 0xF -> DRK_Long
      | otherwise -> DRK_Normal
  firstCluster <- CN <$> get16 si (so + 0x1A)
  fileSize <- get32 si (so + 0x1C)
  shortName <- map toChar <$> getBytes si so 11
  let longName = if
        | shortName == ".          " -> "."
        | shortName == "..         " -> ".."
        | otherwise -> acc
  let isDir = (attr .&. 0x10) /= 0
  pure DirRecord { kind,attr,firstCluster,fileSize,shortName,longName,isDir }

getFileContents :: VolumeInfo -> DirRecord -> M String
getFileContents vol dr = do
  let VolumeInfo{sectorsPerCluster} = vol
  let DirRecord{fileSize,firstCluster} = dr
  let clusterSize = sectorSize * fi sectorsPerCluster
  let
    loop :: CN -> Int -> M String
    loop cn size = if
      | size <= clusterSize -> do
          let si = siFromClusterNumber vol cn
          map toChar <$> getBytes si 0 (fi size)
      | otherwise -> do
          let si = siFromClusterNumber vol cn
          start <- map toChar <$> getBytes si 0 clusterSize
          cn' <- stepFatChain vol cn
          rest <- loop cn' (size - clusterSize)
          pure (start ++ rest)
  loop firstCluster (fi fileSize)

stepFatChain ::VolumeInfo -> CN -> M CN
stepFatChain vol cn = do
  let VolumeInfo{fatBegin} = vol
  let bytePerChainWord :: Int = 2
  let n = bytePerChainWord * fi cn
  let si = fi (n `div` sectorSize) + fatBegin
  let so = fi (n `mod` sectorSize)
  cn' <- CN <$> get16 si so
  --Print ("stepFatChain",cn,"-->",cn')
  pure cn'

siFromClusterNumber :: VolumeInfo -> CN -> SectorIndex
siFromClusterNumber vol cn = do
  let VolumeInfo{sectorsPerCluster,clusterBegin} = vol
  clusterBegin + (fi cn - 2) * fi sectorsPerCluster

data DirRecord = DirRecord
  { kind :: DirRecordKind
  , attr :: W8
  , firstCluster :: CN
  , fileSize :: W32
  , shortName :: String
  , longName :: String
  , isDir :: Bool
  } deriving Show

-- Cluster Number
newtype CN = CN W16 deriving (Num,Integral,Real,Enum,Ord,Eq,Show)

data DirRecordKind = DRK_Normal | DRK_Long | DRK_Unused | DRK_EndOfDir
  deriving (Show,Eq)

getBytes :: SectorIndex -> SectorOffset -> Int -> M [W8]
getBytes si so n = sequence [ get8 si (so + fi i) | i <- [0.. n-1] ]

get32 :: SectorIndex -> SectorOffset -> M W32
get32 si so = do
  lo <- get16 si so
  hi <- get16 si (so+2)
  pure (mk32 HL {hi,lo})

get16 :: SectorIndex -> SectorOffset -> M W16
get16 si so = do
  lo <- get8 si so
  hi <- get8 si (so+1)
  pure (mk16 HL{hi,lo})

get8 :: SectorIndex -> SectorOffset -> M W8
get8 si so = do
  sec <- ReadSector si
  let byte = readInSector sec so
  pure byte

set8 :: SectorIndex -> SectorOffset -> W8 -> M ()
set8 si so byte = do
  sec <- ReadSector si
  let sec' = writeInSector sec so byte
  WriteSector si sec'
