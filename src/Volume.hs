
module Volume
  ( bytesPerDirEntry
  , Vol(..)
  , getVolumeInfo
  )
where

import Control.Exception (assert)
import GetSet (get8,get16)
import TheDisk (M)
import Types (SectorIndex,W8,W16,sectorSize)

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
