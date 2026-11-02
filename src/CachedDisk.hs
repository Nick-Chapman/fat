
module CachedDisk
  ( M, exec
  , Sector, sectorSize
  , loadSector, saveSector
  , readInSector, writeInSector
  , volume
  , getLastAllocatedCN
  ) where

import Control.Monad (ap,liftM)
import Control.Monad.IO.Class (MonadIO,liftIO)
import Data.IORef (IORef,newIORef)
import Disk (Sector,sectorSize)
import Disk qualified
import Types (SectorIndex,SectorOffset,W8,CN(..))
import Volume (Vol(..),getVolumeInfo)

instance Functor M where fmap = liftM
instance Applicative M where pure = Pure; (<*>) = ap
instance Monad M where (>>=) = Bind
instance MonadIO M where liftIO = LiftDisk . liftIO

volume :: M Vol
volume = Volume

getLastAllocatedCN :: M (IORef CN)
getLastAllocatedCN = LastAllocatedCN

loadSector :: SectorIndex -> M Sector
loadSector = LoadSector

saveSector :: SectorIndex -> Sector -> M ()
saveSector = SaveSector

readInSector :: Sector -> SectorOffset -> M W8
readInSector sec so = LiftDisk (Disk.readInSector sec so)

writeInSector :: Sector -> SectorOffset -> W8 -> M ()
writeInSector sec so b = LiftDisk (Disk.writeInSector sec so b)

data M a where
  Pure :: a -> M a
  Bind :: M a -> (a -> M b) -> M b
  LiftDisk :: Disk.M a -> M a
  LoadSector :: SectorIndex -> M Sector
  SaveSector :: SectorIndex -> Sector -> M ()
  Volume :: M Vol
  LastAllocatedCN :: M (IORef CN)

data Cache = Cache
  { theSI:: SectorIndex
  , theSec :: Sector
  , dirty :: Bool
  }

initCache :: Disk.M Cache
initCache = do
  let theSI = -1
  theSec <- Disk.newSector
  pure Cache { theSI, theSec, dirty = False }

exec :: forall a. FilePath -> M a -> IO a
exec path m0 = Disk.exec path do
  s0 <- initCache
  vol <- getVolumeInfo 0
  --liftIO $ print vol
  lastAllocatedCN <- liftIO $ newIORef (CN 2)
  let
    k0 c a = do
      flushCache c
      pure a
    loop :: forall a b. Cache -> M a -> (Cache -> a -> Disk.M b) -> Disk.M b
    loop c = \case
      Pure x -> \k -> k c x
      Bind m f -> \k -> loop c m \c a -> loop c (f a) k
      LiftDisk m -> \k -> m >>= k c
      LoadSector si -> \k -> do
        (sec,c) <- loadSectorC c si
        k c sec
      SaveSector si sec -> \k -> do
        c <- saveSectorC c si sec
        k c ()
      Volume -> \k -> do
        k c vol
      LastAllocatedCN -> \k -> do
        k c lastAllocatedCN

  loop s0 m0 k0


flushCache :: Cache -> Disk.M ()
flushCache Cache{theSI,theSec,dirty} = if
  | dirty -> do
      Disk.saveSector theSI theSec
  | otherwise -> do
      pure ()

saveSectorC :: Cache -> SectorIndex -> Sector -> Disk.M Cache
saveSectorC c@Cache{theSI} si sec = if
  | si == theSI -> do
      pure c { theSec = sec, dirty = True }
  | otherwise -> do
      flushCache c
      pure c { theSec = sec, dirty = False }

loadSectorC :: Cache -> SectorIndex -> Disk.M (Sector, Cache)
loadSectorC c@Cache{theSI,theSec} si = if
  | si == theSI -> do
      pure (theSec, c)
  | otherwise -> do
      flushCache c
      Disk.loadIntoSector si theSec
      pure (theSec, c { theSI = si, dirty = False })
