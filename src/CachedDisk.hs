
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
  { siA:: SectorIndex
  , secA :: Sector
  , siB:: SectorIndex
  , secB :: Sector
  , hitsA :: Int
  , hitsB :: Int
  }

initCache :: Disk.M Cache
initCache = do
  let siA = -1
  let siB = -1
  let secA = undefined
  let secB = undefined
  pure Cache { siA, secA, siB, secB, hitsA = 0, hitsB = 0 }

exec :: forall a. M a -> FilePath -> IO a
exec m0 path = Disk.exec path $ do
  s0 <- initCache
  res <- loop s0 m0 k0
  pure res
  where
    k0 Cache{hitsA,hitsB} a = do
      Disk.Message (printf "[#hits=%d+%d]\n" hitsA hitsB)
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
readSector c@Cache{siA,secA,siB,secB,hitsA,hitsB} si = if

--  | True && True -> if -- Always in A
  | si `mod` 2 == 0 -> if -- Odd/Even Split
--  | si < 0x200 -> if -- Low/High Split
--  | si < 0x2 -> if -- surprisingly performs the best on my two examples

    | si == siA -> do
        pure (secA, c { hitsA = 1 + hitsA })
    | otherwise -> do
        sec <- Disk.ReadSector si
        pure (sec, c { siA = si, secA = sec })

  | otherwise -> if
    | si == siB -> do
        pure (secB, c { hitsB = 1 + hitsB })
    | otherwise -> do
        sec <- Disk.ReadSector si
        pure (sec, c { siB = si, secB = sec })
