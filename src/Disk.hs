
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
import Data.Map (Map)
import Data.Map qualified as Map
import GHC.IO.Device (SeekMode(AbsoluteSeek))
import GHC.IOArray (IOArray,newIOArray,readIOArray,writeIOArray)
import System.Posix.IO.ByteString (OpenMode(..),OpenFileFlags(..),defaultFileFlags,openFd,closeFd,fdSeek,fdRead,fdWrite)
import System.Posix.Types (Fd,ByteCount,FileOffset)
import Text.Printf (printf)
import Types (SectorIndex,SectorOffset(..),W8(..))

sectorSize :: Int
sectorSize = 512

data Sector = Sector (IOArray SectorOffset W8)

newSector :: M Sector
newSector = NewSector

loadIntoSector :: SectorIndex -> Sector -> M ()
loadIntoSector = LoadIntoSector

saveSector :: SectorIndex -> Sector -> M ()
saveSector = SaveSector

readInSector :: Sector -> SectorOffset -> M W8
readInSector (Sector arr) so = do
  liftIO (readIOArray arr so)

writeInSector :: Sector -> SectorOffset -> W8 -> M ()
writeInSector (Sector arr) so b = do
  liftIO (writeIOArray arr so b)

instance Functor M where fmap = liftM
instance Applicative M where pure = Pure; (<*>) = ap
instance Monad M where (>>=) = Bind
instance MonadIO M where liftIO = LiftIO

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
        sec <- Sector <$> newIOArray (0,fromIntegral sectorSize-1) 0
        let Tracking{countSlots} = tracking
        pure (sec,tracking { countSlots = 1 + countSlots })
      LoadIntoSector si sec -> do
        --print ("LoadSectorInto",si)
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


loadIntoSectorIO :: FilePath -> SectorIndex -> Sector -> IO ()
loadIntoSectorIO diskImagePath i (Sector arr) = do
  withSectorSeek diskImagePath i $ \fd -> do
    b <- fdRead fd (fromIntegral sectorSize)
    sequence_ [ writeIOArray arr (fromIntegral i) (W8 (B.index b i))
              | i <- [0 .. sectorSize - 1 ]
              ]

saveSectorIO :: FilePath -> SectorIndex -> Sector -> IO ()
saveSectorIO diskImagePath i (Sector arr) = do
  withSectorSeek diskImagePath i $ \fd -> do
    bs <- sequence [ readIOArray arr (fromIntegral i)
                   | i <- [0 .. sectorSize - 1 ]
                   ]
    _::ByteCount <- fdWrite fd (B.pack [ b | W8 b <- bs ])
    pure ()

withSectorSeek :: FilePath -> SectorIndex -> (Fd -> IO a) -> IO a
withSectorSeek diskImagePath i f = do
  path <- BS.fromFilePath diskImagePath
  fd <- openFd path ReadWrite defaultFileFlags { creat = Just 0o640 }
  _::FileOffset <- fdSeek fd AbsoluteSeek (fromIntegral i * fromIntegral sectorSize)
  res <- f fd
  closeFd fd
  pure res
