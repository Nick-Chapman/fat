
module CachedDisk
  ( M, exec
  , Sector, sectorSize
  , readSector, writeSector
  , readInSector, writeInSector
  ) where

import Control.Monad (ap,liftM)
import Control.Monad.IO.Class (MonadIO,liftIO)
import Disk (Sector,sectorSize)
import Disk qualified
import Text.Printf (printf)
import Types (SectorIndex,SectorOffset,W8)

instance Functor M where fmap = liftM
instance Applicative M where pure = Pure; (<*>) = ap
instance Monad M where (>>=) = Bind
instance MonadIO M where liftIO = LiftDisk . liftIO

readSector :: SectorIndex -> M Sector
readSector = ReadSector

writeSector :: SectorIndex -> Sector -> M ()
writeSector = WriteSector

readInSector :: Sector -> SectorOffset -> M W8
readInSector sec so = LiftDisk (Disk.readInSector sec so)

writeInSector :: Sector -> SectorOffset -> W8 -> M ()
writeInSector sec so b = LiftDisk (Disk.writeInSector sec so b)

data M a where
  Pure :: a -> M a
  Bind :: M a -> (a -> M b) -> M b
  LiftDisk :: Disk.M a -> M a
  ReadSector :: SectorIndex -> M Sector
  WriteSector :: SectorIndex -> Sector -> M ()

data Cache = Cache
  { theSI:: SectorIndex
  , theSec :: Sector
  , hitsR :: Int
  , batchedW :: Int
  , dirty :: Bool
  }

initCache :: Disk.M Cache
initCache = do
  let theSI = -1
  let theSec = undefined
  pure Cache { theSI, theSec, hitsR = 0, batchedW = 0, dirty = False }

exec :: forall a. FilePath -> M a -> IO a
exec path m0 = Disk.exec path do
  s0 <- initCache
  res <- loop s0 m0 k0
  pure res
  where
    k0 c@Cache{hitsR,batchedW} a = do
      flushCache c
      liftIO (printf "[#hitsR=%d,batchedW=%d]\n" hitsR batchedW)
      pure a
    loop :: forall a b. Cache -> M a -> (Cache -> a -> Disk.M b) -> Disk.M b
    loop c = \case
      Pure x -> \k -> k c x
      Bind m f -> \k -> loop c m \c a -> loop c (f a) k
      LiftDisk m -> \k -> m >>= k c
      ReadSector si -> \k -> do
        (sec,c) <- readSectorC c si
        k c sec
      WriteSector si sec -> \k -> do
        c <- writeSectorC c si sec
        k c ()

flushCache :: Cache -> Disk.M ()
flushCache Cache{theSI,theSec,dirty} = if
  | dirty -> do
      Disk.writeSector theSI theSec
  | otherwise -> do
      pure ()

writeSectorC :: Cache -> SectorIndex -> Sector -> Disk.M Cache
writeSectorC c@Cache{theSI,batchedW} si sec = if
  | si == theSI -> do
      pure c { theSec = sec, dirty = True,  batchedW = 1 + batchedW }
  | otherwise -> do
      flushCache c
      pure c { theSec = sec, dirty = False }

readSectorC :: Cache -> SectorIndex -> Disk.M (Sector, Cache)
readSectorC c@Cache{theSI,theSec,hitsR} si = if
  | si == theSI -> do
      pure (theSec, c { hitsR = 1 + hitsR })
  | otherwise -> do
      flushCache c
      sec <- Disk.readSector si
      pure (sec, c { theSI = si, theSec = sec, dirty = False })
