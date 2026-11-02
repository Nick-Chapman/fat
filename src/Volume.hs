
module Volume
  ( bytesPerDirEntry
  , Vol(..)
  , getVolumeInfo
  , CN(..),
  )
where

import Control.Exception (assert)
import Control.Monad.IO.Class (liftIO)
import Data.IORef (newIORef)
import Disk (M,sectorSize,newSector,loadIntoSector,readInSector)
import Types (SectorIndex,CN(..),Vol(..),W8,W16,SectorOffset,HL(..),mk16)

bytesPerDirEntry :: Int
bytesPerDirEntry = 32

getVolumeInfo :: SectorIndex -> M Vol
getVolumeInfo si = do
  sec <- newSector
  loadIntoSector si sec
  let
    get8 :: SectorOffset -> M W8
    get8 so = do
      readInSector sec so

    get16 :: SectorOffset -> M W16
    get16 so = do
      lo <- get8 so
      hi <- get8 (so+1)
      pure (mk16 HL{hi,lo})

  bytesPerSector <- get16 0xB
  sectorsPerCluster <- get8 0xD
  numberReservedSectors <- get16 0xE
  numberOfFats <- get8 0x10
  numberOfRootEntries <- get16 0x11
  sectorsPerFat <- get16 0x16
  sig <- get16 0x1FE

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

  lastAllocatedCN <- liftIO $ newIORef (CN 2)

  pure VolumeInfo { sectorsPerCluster
                  , sectorsPerFat
                  , fatBegin
                  , rootDirBegin
                  , clusterBegin
                  , lastAllocatedCN
                  }
