
module Disk
  ( M (ReadSector,WriteSector), exec
  ) where

import Control.Monad (ap,liftM)
import Control.Monad.IO.Class (MonadIO,liftIO)
import Data.ByteString qualified as BS (fromFilePath)
import Data.Map (Map)
import Data.Map qualified as Map
import GHC.IO.Device (SeekMode(AbsoluteSeek))
import System.Posix.IO.ByteString (OpenMode(..),OpenFileFlags(..),defaultFileFlags,openFd,closeFd,fdSeek,fdRead,fdWrite)
import System.Posix.Types (Fd,ByteCount,FileOffset)
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

data Tracking = Tracking
  { reads :: Map SectorIndex Int
  , writes :: Map SectorIndex Int
  }

initTracking :: Tracking
initTracking = Tracking { reads = Map.empty, writes = Map.empty }

exec :: FilePath -> M a -> IO a
exec path m0 = do
  let
    go :: Tracking -> M a -> IO (a,Tracking)
    go tracking = \case
      Pure x -> pure (x,tracking)
      Bind m f -> do (a,tracking) <- go tracking m; go tracking (f a)
      LiftIO io -> do a <- io; pure (a,tracking)
      ReadSector si -> do
        --print ("ReadSector",si)
        let Tracking{reads} = tracking
        let tracking' = tracking { reads = Map.insertWithKey (\_ -> (+)) si 1 reads }
        sec <- readSector path si
        pure (sec, tracking')
      WriteSector si sec -> do
        --print ("WriteSector",si)
        let Tracking{writes} = tracking
        let tracking' = tracking { writes = Map.insertWithKey (\_ -> (+)) si 1 writes }
        writeSector path si sec
        pure ((),tracking')

  (a,Tracking{reads,writes}) <- go initTracking m0
  let numReads = sum (Map.elems reads)
  let numWrites = sum (Map.elems writes)
  --let _numRepeats = numReads - length (Map.keys reads)
  --let _hist = [ (fromIntegral si :: W16,n) | (si,n) <- Map.toList reads, n > 1 ]
  printf "[#reads=%d, #writes=%d]\n" numReads numWrites
  pure a


----------------------------------------------------------------------

readSector :: FilePath -> SectorIndex -> IO Sector
readSector diskImagePath i = do
  withSectorSeek diskImagePath i $ \fd -> do
    mkSector <$> fdRead fd (fromIntegral sectorSize)

writeSector :: FilePath -> SectorIndex -> Sector -> IO ()
writeSector diskImagePath i s = do
  withSectorSeek diskImagePath i $ \fd -> do
    _::ByteCount <- fdWrite fd (deSector s)
    pure ()

withSectorSeek :: FilePath -> SectorIndex -> (Fd -> IO a) -> IO a
withSectorSeek diskImagePath i f = do
  path <- BS.fromFilePath diskImagePath
  fd <- openFd path ReadWrite defaultFileFlags { creat = Just 0o640 }
  --setFdSize fd (fromIntegral (sectorSize * numSectors))
  _::FileOffset <- fdSeek fd AbsoluteSeek (fromIntegral i * fromIntegral sectorSize)
  res <- f fd
  closeFd fd
  pure res
