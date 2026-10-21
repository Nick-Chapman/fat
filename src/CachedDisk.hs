
module CachedDisk
  ( M (Message,ReadSector), exec
  ) where

import Text.Printf (printf)
import Control.Monad (ap,liftM)
import Disk qualified
import Types

instance Functor M where fmap = liftM
instance Applicative M where pure = Pure; (<*>) = ap
instance Monad M where (>>=) = Bind

data M a where
  Pure :: a -> M a
  Bind :: M a -> (a -> M b) -> M b
  ReadSector :: SectorIndex -> M Sector
  Message :: String -> M ()

data Cache = Cache
  { lastSI:: SectorIndex
  , lastSec :: Sector
  , hits :: Int
  }

initCache :: Disk.M Cache
initCache = do
  let lastSI = 0
  lastSec <- Disk.ReadSector lastSI
  pure Cache { lastSI, lastSec, hits = 0 }

exec :: forall a. M a -> FilePath -> IO a
exec m0 path = Disk.exec path $ do
  s0 <- initCache
  res <- loop s0 m0 k0
  pure res
  where
    k0 Cache{hits} a = do
      Disk.Message (printf "[#hits=%d]\n" hits)
      pure a
    loop :: forall a b. Cache -> M a -> (Cache -> a -> Disk.M b) -> Disk.M b
    loop c = \case
      Pure x -> \k -> k c x
      Bind m f -> \k -> loop c m \c a -> loop c (f a) k
      Message str -> \k -> do Disk.Message str; k c ()
      ReadSector si -> \k -> do
        (sec,c) <- readSector c si
        k c sec

readSector :: Cache -> SectorIndex -> Disk.M (Sector, Cache)
readSector c@Cache{lastSI,lastSec,hits} si = if
  | si == lastSI -> do
    pure (lastSec, c { hits = 1 + hits })
  | otherwise -> do
      sec <- Disk.ReadSector si
      pure (sec, c { lastSI = si, lastSec = sec })
