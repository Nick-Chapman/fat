
module Disk
  ( M, exec
  , Sector, sectorSize
  , readSector, writeSector
  , readInSector, writeInSector
  ) where

import Control.Monad (ap,liftM)
import Control.Monad.IO.Class (MonadIO,liftIO)
import Data.ByteString qualified as B
import Data.ByteString qualified as BS (fromFilePath)
import Data.Map (Map)
import Data.Map qualified as Map
import GHC.IO.Device (SeekMode(AbsoluteSeek))
import System.Posix.IO.ByteString (OpenMode(..),OpenFileFlags(..),defaultFileFlags,openFd,closeFd,fdSeek,fdRead,fdWrite)
import System.Posix.Types (Fd,ByteCount,FileOffset)
import Text.Printf (printf)
import Types (SectorIndex,SectorOffset(..),W8)

import Data.IORef --(IORef,newIORef,readIORef,writeIORef)

instance Functor M where fmap = liftM
instance Applicative M where pure = Pure; (<*>) = ap
instance Monad M where (>>=) = Bind
instance MonadIO M where liftIO = LiftIO


sectorSize :: Int
sectorSize = 512

newtype Sector = Sector (IORef [W8]) -- TODO: Use mutable array representation

mkSector :: B.ByteString -> IO Sector
mkSector bs = Sector <$> newIORef (map fromIntegral (B.unpack bs))

deSector :: Sector -> IO B.ByteString
deSector (Sector bsR) = do
  bs <- liftIO (readIORef bsR)
  pure $ B.pack (map fromIntegral bs)

readInSector :: Sector -> SectorOffset -> M W8
readInSector (Sector bsR) (SectorOffset n) = do
  bs <- liftIO (readIORef bsR)
  pure (bs !! fromIntegral n)

writeInSector :: Sector -> SectorOffset -> W8 -> M ()
writeInSector (Sector bsR) (SectorOffset n) b = do
  bs <- liftIO (readIORef bsR)
  let bs' = take (fromIntegral n) bs ++ [b] ++ drop (fromIntegral n + 1) bs
  liftIO (writeIORef bsR bs')

readSector :: SectorIndex -> M Sector
readSector = ReadSector

writeSector :: SectorIndex -> Sector -> M ()
writeSector = WriteSector

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
        sec <- readSectorIO path si
        pure (sec, tracking')
      WriteSector si sec -> do
        --print ("WriteSector",si)
        let Tracking{writes} = tracking
        let tracking' = tracking { writes = Map.insertWithKey (\_ -> (+)) si 1 writes }
        writeSectorIO path si sec
        pure ((),tracking')

  (a,Tracking{reads,writes}) <- go initTracking m0
  let numReads = sum (Map.elems reads)
  let numWrites = sum (Map.elems writes)
  --let _numRepeats = numReads - length (Map.keys reads)
  --let _hist = [ (fromIntegral si :: W16,n) | (si,n) <- Map.toList reads, n > 1 ]
  printf "[#reads=%d, #writes=%d]\n" numReads numWrites
  pure a


----------------------------------------------------------------------

readSectorIO :: FilePath -> SectorIndex -> IO Sector
readSectorIO diskImagePath i = do
  withSectorSeek diskImagePath i $ \fd -> do
    fdRead fd (fromIntegral sectorSize) >>= mkSector

writeSectorIO :: FilePath -> SectorIndex -> Sector -> IO ()
writeSectorIO diskImagePath i s = do
  withSectorSeek diskImagePath i $ \fd -> do
    bs <- deSector s
    _::ByteCount <- fdWrite fd bs
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
