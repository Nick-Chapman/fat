
module Disk
  ( M, exec
  , Sector, sectorSize
  , loadSector, saveSector
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

-- TODO: clearer "From" insetad of "In" ?
readInSector :: Sector -> SectorOffset -> M W8
readInSector (Sector bsR) (SectorOffset n) = do
  bs <- liftIO (readIORef bsR)
  pure (bs !! fromIntegral n)

writeInSector :: Sector -> SectorOffset -> W8 -> M ()
writeInSector (Sector bsR) (SectorOffset n) b = do
  bs <- liftIO (readIORef bsR)
  let bs' = take (fromIntegral n) bs ++ [b] ++ drop (fromIntegral n + 1) bs
  liftIO (writeIORef bsR bs')

loadSector :: SectorIndex -> M Sector
loadSector = LoadSector

saveSector :: SectorIndex -> Sector -> M ()
saveSector = SaveSector

-- TODO: Is the GADT buying anything?
data M a where
  Pure :: a -> M a
  Bind :: M a -> (a -> M b) -> M b
  LiftIO :: IO a -> M a
  LoadSector :: SectorIndex -> M Sector
  SaveSector :: SectorIndex -> Sector -> M ()

data Tracking = Tracking
  { loads :: Map SectorIndex Int
  , saves :: Map SectorIndex Int
  }

initTracking :: Tracking
initTracking = Tracking { loads = Map.empty, saves = Map.empty }

exec :: FilePath -> M a -> IO a
exec path m0 = do
  let
    go :: Tracking -> M a -> IO (a,Tracking)
    go tracking = \case
      Pure x -> pure (x,tracking)
      Bind m f -> do (a,tracking) <- go tracking m; go tracking (f a)
      LiftIO io -> do a <- io; pure (a,tracking)
      LoadSector si -> do
        --print ("LoadSector",si)
        let Tracking{loads} = tracking
        let tracking' = tracking { loads = Map.insertWithKey (\_ -> (+)) si 1 loads }
        sec <- loadSectorIO path si
        pure (sec, tracking')
      SaveSector si sec -> do
        --print ("SaveSector",si)
        let Tracking{saves} = tracking
        let tracking' = tracking { saves = Map.insertWithKey (\_ -> (+)) si 1 saves }
        saveSectorIO path si sec
        pure ((),tracking')

  (a,Tracking{loads,saves}) <- go initTracking m0
  let numLoads = sum (Map.elems loads)
  let numSaves = sum (Map.elems saves)
  --let _numRepeats = numLoads - length (Map.keys loads)
  --let _hist = [ (fromIntegral si :: W16,n) | (si,n) <- Map.toList loads, n > 1 ]
  printf "[#loads=%d, #saves=%d]\n" numLoads numSaves
  pure a


----------------------------------------------------------------------

loadSectorIO :: FilePath -> SectorIndex -> IO Sector
loadSectorIO diskImagePath i = do
  withSectorSeek diskImagePath i $ \fd -> do
    fdRead fd (fromIntegral sectorSize) >>= mkSector

saveSectorIO :: FilePath -> SectorIndex -> Sector -> IO ()
saveSectorIO diskImagePath i s = do
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
