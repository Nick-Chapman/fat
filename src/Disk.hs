
module Disk
  ( M (Message,ReadSector), execDisk
  ) where

import Control.Monad (ap,liftM)
import Data.ByteString qualified as B
import Prelude hiding (interact)
import Text.Printf (printf)

import Types

instance Functor M where fmap = liftM
instance Applicative M where pure = Pure; (<*>) = ap
instance Monad M where (>>=) = Bind

data M a where
  Pure :: a -> M a
  Bind :: M a -> (a -> M b) -> M b
  ReadSector :: SectorIndex -> M Sector
  Print :: Show a => a -> M ()
  Message :: String -> M ()

execDisk :: M a -> FilePath -> IO a
execDisk m0 path = do
  theBS <- B.readFile path
  let
    reallyReadSector :: DiskState -> SectorIndex -> IO (Sector,DiskState)
    reallyReadSector cache@DiskState{readCount} si = do
      --print ("ReadSector",n)
      let sec = mkSector theBS si
      pure (sec, cache { lastSI = si, lastSec = sec, readCount = 1 + readCount })

    go :: DiskState -> M a -> IO (a,DiskState)
    go c = \case
      Pure x -> pure (x,c)
      Bind m f -> do (a,c) <- go c m; go c (f a)
      Print x -> do print x; pure ((),c)
      Message x -> do putStr x; pure ((),c)
      ReadSector si -> do
        let DiskState {lastSI,lastSec,cacheHitCount} = c
        if si == lastSI
          then pure (lastSec,c { cacheHitCount = 1 + cacheHitCount })
          else reallyReadSector c si

  let si = 0 -- volume index
  let cu = state0
  (_sec,c0) <- reallyReadSector cu si
  (a,cFinal) <- go c0 m0
  let DiskState{readCount,cacheHitCount} = cFinal
  printf "[#reads=%d; #hits=%d]\n" readCount cacheHitCount
  pure a
    where
      state0 :: DiskState
      state0 = DiskState
        { readCount = 0
        , cacheHitCount = 0
        , lastSI = undefined
        , lastSec = undefined
        }


data DiskState = DiskState
  { readCount :: Int
  , cacheHitCount :: Int
  , lastSI:: SectorIndex
  , lastSec :: Sector
  }



