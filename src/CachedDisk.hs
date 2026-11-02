
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

type CC = (Cache,Cache,Cache)

exec :: forall a. FilePath -> M a -> IO a
exec path m0 = Disk.exec path do
  vol <- getVolumeInfo 0
  --liftIO $ print vol
  lastAllocatedCN <- liftIO $ newIORef (CN 2)

  let VolumeInfo{fat2Begin,rootDirBegin} = vol
  let
    inFat1 si = si < fat2Begin
    inFat2 si = si < rootDirBegin

  let
    select :: SectorIndex -> (CC -> Cache, CC -> Cache -> CC)
    select si = if
      | inFat1 si -> do
          let get (x,_,_) = x
          let set (_,b,c) a = (a,b,c)
          (get,set)
      | inFat2 si -> do
          let get (_,x,_) = x
          let set (a,_,c) b = (a,b,c)
          (get,set)
      | otherwise -> do
          let get (_,_,x) = x
          let set (a,b,_) c = (a,b,c)
          (get,set)

  let
    k0 (c1,c2,c3) a = do
      flushCache c1
      flushCache c2
      flushCache c3
      pure a
    loop :: forall a b. CC -> M a -> (CC -> a -> Disk.M b) -> Disk.M b
    loop cc = \case
      Pure x -> \k -> k cc x
      Bind m f -> \k -> loop cc m \cc a -> loop cc (f a) k
      LiftDisk m -> \k -> m >>= k cc
      LoadSector si -> \k -> do
        let (get,set) = select si
        let c = get cc
        (sec,c) <- loadSectorC c si
        let cc' = set cc c
        k cc' sec
      SaveSector si sec -> \k -> do
        let (get,set) = select si
        let c = get cc
        c <- saveSectorC c si sec
        let cc' = set cc c
        k cc' ()
      Volume -> \k -> do
        k cc vol
      LastAllocatedCN -> \k -> do
        k cc lastAllocatedCN

  c1 <- initCache
  c2 <- initCache
  c3 <- initCache
  let cc = (c1,c2,c3)
  loop cc m0 k0

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
