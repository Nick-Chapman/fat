
module Volume
  ( bytesPerDirEntry
  , Vol(..)
  , getVolumeInfo
  , get8
  , get16
  )
where

import TheDisk (M, readSector)
import Control.Exception (assert)
import Prelude hiding (interact)
import Types (HL(..),SectorIndex,SectorOffset,W8,W16,mk16,sectorSize,readInSector)

bytesPerDirEntry :: Int
bytesPerDirEntry = 32

data Vol = VolumeInfo
  { sectorsPerCluster :: W8
  , sectorsPerFat :: W16
  , fatBegin :: SectorIndex
  , rootDirBegin :: SectorIndex
  , clusterBegin :: SectorIndex
  } deriving Show

getVolumeInfo :: SectorIndex -> M Vol
getVolumeInfo si = do
  bytesPerSector <- get16 si 0xB
  sectorsPerCluster <- get8 si 0xD
  numberReservedSectors <- get16 si 0xE
  numberOfFats <- get8 si 0x10
  numberOfRootEntries <- get16 si 0x11
  sectorsPerFat <- get16 si 0x16
  sig <- get16 si 0x1FE

  assert (fromIntegral bytesPerSector == sectorSize) $ do
  assert (sectorsPerCluster == 1) $ do
  assert (numberOfFats == 2) $ do
  assert (numberReservedSectors == 1) $ do
  assert (sig == 0xAA55) $ do

  let numberOfDirEntriesPerSector = fromIntegral (sectorSize `div` bytesPerDirEntry)
  assert (numberOfDirEntriesPerSector == 16) $ do
  assert (numberOfRootEntries `mod` numberOfDirEntriesPerSector == 0) $ do
  let sectorsForRootDir = numberOfRootEntries `div` numberOfDirEntriesPerSector

  let fatBegin = si + fromIntegral numberReservedSectors
  let rootDirBegin = fatBegin + fromIntegral sectorsPerFat * fromIntegral numberOfFats
  let clusterBegin = rootDirBegin + fromIntegral sectorsForRootDir

  pure VolumeInfo { sectorsPerCluster
                  , sectorsPerFat
                  , fatBegin
                  , rootDirBegin
                  , clusterBegin
                  }

get16 :: SectorIndex -> SectorOffset -> M W16
get16 si so = do
  lo <- get8 si so
  hi <- get8 si (so+1)
  pure (mk16 HL{hi,lo})

get8 :: SectorIndex -> SectorOffset -> M W8
get8 si so = do
  sec <- readSector si
  let byte = readInSector sec so
  pure byte
