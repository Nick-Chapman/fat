
module CachedDisk
  ( M (ReadSector), exec
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

data Cache = Cache
  { siA:: SectorIndex
  , secA :: Sector
  , hitsA :: Int
--  , siB:: SectorIndex
--  , secB :: Sector
--  , hitsB :: Int
  }

initCache :: Disk.M Cache
initCache = do
  let siA = -1
  --let siB = -1
  let secA = undefined
  --let secB = undefined
  --pure Cache { siA, secA, siB, secB, hitsA = 0, hitsB = 0 }
  pure Cache { siA, secA, hitsA = 0}

exec :: forall a. FilePath -> M a -> IO a
exec path m0 = Disk.exec path do
  s0 <- initCache
  res <- loop s0 m0 k0
  pure res
  where
    k0 Cache{hitsA
            -- ,hitsB
            } a = do
     -- liftIO (printf "[#hits=%d+%d]\n" hitsA hitsB)
      liftIO (printf "[#hits=%d]\n" hitsA)
      pure a
    loop :: forall a b. Cache -> M a -> (Cache -> a -> Disk.M b) -> Disk.M b
    loop c = \case
      Pure x -> \k -> k c x
      Bind m f -> \k -> loop c m \c a -> loop c (f a) k
      LiftIO io -> \k -> liftIO io >>= k c
      ReadSector si -> \k -> do
        (sec,c) <- readSector c si
        k c sec

readSector :: Cache -> SectorIndex -> Disk.M (Sector, Cache)
readSector c@Cache{siA,secA,hitsA
                  -- ,siB,secB,hitsB
                  } si = if

--  | True && True -> if -- Always in A
--  | si `mod` 2 == 0 -> if -- Odd/Even Split
--  | si < 0x200 -> if -- Low/High Split
--  | si < 0x2 -> if -- surprisingly performs the best on my two examples

    | si == siA -> do
        pure (secA, c { hitsA = 1 + hitsA })
    | otherwise -> do
        sec <- Disk.ReadSector si
        pure (sec, c { siA = si, secA = sec })
{-
  | otherwise -> if
    | si == siB -> do
        pure (secB, c { hitsB = 1 + hitsB })
    | otherwise -> do
        sec <- Disk.ReadSector si
        pure (sec, c { siB = si, secB = sec })
-}
