
module CachedDisk
  ( M (ReadSector,WriteSector), exec
  ) where

import Control.Monad (ap,liftM)
import Control.Monad.IO.Class (MonadIO,liftIO)
import Disk qualified
import Text.Printf (printf)
import Types

instance Functor M where fmap = liftM
instance Applicative M where pure = Pure; (<*>) = ap
instance Monad M where (>>=) = Bind
instance MonadIO M where liftIO = LiftIO

data M a where
  Pure :: a -> M a
  Bind :: M a -> (a -> M b) -> M b
  LiftIO :: IO a -> M a
  ReadSector :: SectorIndex -> M Sector
  WriteSector :: SectorIndex -> Sector -> M ()

data Cache = Cache
  { theSI:: SectorIndex
  , theSec :: Sector
  , hitsR :: Int
  , hitsW :: Int
  }

initCache :: Disk.M Cache
initCache = do
  let theSI = -1
  let theSec = undefined
  pure Cache { theSI, theSec, hitsR = 0, hitsW = 0 }

exec :: forall a. FilePath -> M a -> IO a
exec path m0 = Disk.exec path do
  s0 <- initCache
  res <- loop s0 m0 k0
  pure res
  where
    k0 Cache{hitsR,hitsW} a = do
      liftIO (printf "[#hitsR=%d,hitsW=%d]\n" hitsR hitsW)
      pure a
    loop :: forall a b. Cache -> M a -> (Cache -> a -> Disk.M b) -> Disk.M b
    loop c = \case
      Pure x -> \k -> k c x
      Bind m f -> \k -> loop c m \c a -> loop c (f a) k
      LiftIO io -> \k -> liftIO io >>= k c
      ReadSector si -> \k -> do
        (sec,c) <- readSector c si
        k c sec
      WriteSector si sec -> \k -> do
        c <- writeSector c si sec
        k c ()

cachingEnabled :: Bool
cachingEnabled = True

writeSector :: Cache -> SectorIndex -> Sector -> Disk.M Cache
writeSector c@Cache{theSI} si sec = do
  Disk.WriteSector si sec -- TODO: delay write to disk (and tick hitsW)
  if
    | cachingEnabled && si == theSI -> do
        pure c { theSec = sec }
    | otherwise -> do
        pure c

readSector :: Cache -> SectorIndex -> Disk.M (Sector, Cache)
readSector c@Cache{theSI,theSec,hitsR} si = if
  | cachingEnabled && si == theSI -> do
      pure (theSec, c { hitsR = 1 + hitsR })
  | otherwise -> do
      sec <- Disk.ReadSector si
      pure (sec, c { theSI = si, theSec = sec })
