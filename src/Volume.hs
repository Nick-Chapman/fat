
module Volume
  ( bytesPerDirEntry
  , Vol(..)
  , getVolumeInfo
  , CN(..), getNextCandidateCN, setLastAllocatedCN
  )
where

import Control.Exception (assert)
import Control.Monad.IO.Class (liftIO)
import Data.IORef (IORef,newIORef,readIORef,writeIORef)
import GetSet (get8,get16)
import TheDisk (M,sectorSize)
import Types (SectorIndex,W8,W16)

-- Cluster Number
newtype CN = CN W16 deriving (Num,Integral,Real,Enum,Ord,Eq,Show)

getNextCandidateCN :: Vol -> M CN
getNextCandidateCN vol = do
  let VolumeInfo{lastAllocatedCN} = vol
  cn <- liftIO $ readIORef lastAllocatedCN
  pure (cn + 1)

setLastAllocatedCN :: Vol -> CN -> M ()
setLastAllocatedCN vol cn = do
  let VolumeInfo{lastAllocatedCN} = vol
  liftIO $ writeIORef lastAllocatedCN cn

bytesPerDirEntry :: Int
bytesPerDirEntry = 32

data Vol = VolumeInfo
  { sectorsPerCluster :: W8
  , sectorsPerFat :: W16
  , fatBegin :: SectorIndex
  , rootDirBegin :: SectorIndex
  , clusterBegin :: SectorIndex
  , lastAllocatedCN :: IORef CN
  } --deriving Show

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

  lastAllocatedCN <- liftIO $ newIORef (CN 2)

  pure VolumeInfo { sectorsPerCluster
                  , sectorsPerFat
                  , fatBegin
                  , rootDirBegin
                  , clusterBegin
                  , lastAllocatedCN
                  }
