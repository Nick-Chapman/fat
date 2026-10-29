
module Disk
  ( M, exec
  , Sector, sectorSize
  , newSector, loadIntoSector, saveSector
  , readInSector, writeInSector
  ) where

import Control.Monad (ap,liftM)
import Control.Monad.IO.Class (MonadIO,liftIO)
import Data.ByteString qualified as B
import Data.ByteString qualified as BS (fromFilePath)
import Data.IORef (IORef,newIORef,readIORef,writeIORef)
import Data.Map (Map)
import Data.Map qualified as Map
import GHC.IO.Device (SeekMode(AbsoluteSeek))
import System.Posix.IO.ByteString (OpenMode(..),OpenFileFlags(..),defaultFileFlags,openFd,closeFd,fdSeek,fdRead,fdWrite)
import System.Posix.Types (Fd,ByteCount,FileOffset)
import Text.Printf (printf)
import Types (SectorIndex,SectorOffset(..),W8)

instance Functor M where fmap = liftM
instance Applicative M where pure = Pure; (<*>) = ap
instance Monad M where (>>=) = Bind
instance MonadIO M where liftIO = LiftIO

sectorSize :: Int
sectorSize = 512

newtype Sector = Sector (IORef [W8]) -- TODO: Use mutable array representation

newSector :: M Sector
newSector = NewSector

loadIntoSector :: SectorIndex -> Sector -> M ()
loadIntoSector = LoadIntoSector

saveSector :: SectorIndex -> Sector -> M ()
saveSector = SaveSector

readInSector :: Sector -> SectorOffset -> M W8
readInSector (Sector bsR) (SectorOffset n) = do
  bs <- liftIO (readIORef bsR)
  pure (bs !! fromIntegral n)

writeInSector :: Sector -> SectorOffset -> W8 -> M ()
writeInSector (Sector bsR) (SectorOffset n) b = do
  bs <- liftIO (readIORef bsR)
  let bs' = take (fromIntegral n) bs ++ [b] ++ drop (fromIntegral n + 1) bs
  liftIO (writeIORef bsR bs')

-- TODO: Is the GADT buying anything?
data M a where
  Pure :: a -> M a
  Bind :: M a -> (a -> M b) -> M b
  LiftIO :: IO a -> M a
  NewSector :: M Sector
  LoadIntoSector :: SectorIndex -> Sector -> M ()
  SaveSector :: SectorIndex -> Sector -> M ()

data Tracking = Tracking
  { loads :: Map SectorIndex Int
  , saves :: Map SectorIndex Int
  , countSlots :: Int
  }

initTracking :: Tracking
initTracking = Tracking { loads = Map.empty, saves = Map.empty, countSlots = 0 }

exec :: FilePath -> M a -> IO a
exec path m0 = do
  let
    go :: Tracking -> M a -> IO (a,Tracking)
    go tracking = \case
      Pure x -> pure (x,tracking)
      Bind m f -> do (a,tracking) <- go tracking m; go tracking (f a)
      LiftIO io -> do a <- io; pure (a,tracking)
      NewSector -> do
        sec <- Sector <$> newIORef undefined
        let Tracking{countSlots} = tracking
        pure (sec,tracking { countSlots = 1 + countSlots })
      LoadIntoSector si sec -> do
        --print ("LoadSector",si)
        let Tracking{loads} = tracking
        let tracking' = tracking { loads = Map.insertWithKey (\_ -> (+)) si 1 loads }
        loadIntoSectorIO path si sec
        pure ((), tracking')
      SaveSector si sec -> do
        --print ("SaveSector",si)
        let Tracking{saves} = tracking
        let tracking' = tracking { saves = Map.insertWithKey (\_ -> (+)) si 1 saves }
        saveSectorIO path si sec
        pure ((),tracking')

  (a,Tracking{loads,saves,countSlots}) <- go initTracking m0
  let numLoads = sum (Map.elems loads)
  let numSaves = sum (Map.elems saves)
  --let _numRepeats = numLoads - length (Map.keys loads)
  --let _hist = [ (fromIntegral si :: W16,n) | (si,n) <- Map.toList loads, n > 1 ]
  printf "[#loads=%d, #saves=%d, #slots=%d]\n" numLoads numSaves countSlots
  pure a


----------------------------------------------------------------------

loadIntoSectorIO :: FilePath -> SectorIndex -> Sector -> IO ()
loadIntoSectorIO diskImagePath i (Sector bsR) = do
  withSectorSeek diskImagePath i $ \fd -> do
    b <- fdRead fd (fromIntegral sectorSize)
    let bs = map fromIntegral (B.unpack b)
    writeIORef bsR bs

saveSectorIO :: FilePath -> SectorIndex -> Sector -> IO ()
saveSectorIO diskImagePath i (Sector bsR) = do
  withSectorSeek diskImagePath i $ \fd -> do
    bs <- readIORef bsR
    let b = B.pack (map fromIntegral bs)
    _::ByteCount <- fdWrite fd b
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
