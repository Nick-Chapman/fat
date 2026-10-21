
module Disk
  ( M (ReadSector), exec
  ) where

import Control.Monad (ap,liftM)
import Control.Monad.IO.Class (MonadIO,liftIO)
import Data.ByteString qualified as B
import Data.Map as Map
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

data Tracking = Tracking
  { reads :: Map SectorIndex Int
  }

initTracking :: Tracking
initTracking = Tracking { reads = Map.empty }

exec :: FilePath -> M a -> IO a
exec path m0 = do
  theBS <- B.readFile path
  let
    go :: Tracking -> M a -> IO (a,Tracking)
    go c = \case
      Pure x -> pure (x,c)
      Bind m f -> do (a,c) <- go c m; go c (f a)
      LiftIO io -> do a <- io; pure (a,c)
      ReadSector si -> do
        --print ("ReadSector",si)
        let Tracking{reads} = c
        let c' = c { reads = Map.insertWithKey (\_ -> (+)) si 1 reads }
        pure (mkSector theBS si, c')

  (a,Tracking{reads}) <- go initTracking m0
  let numReads = sum (Map.elems reads)
  let numRepeats = numReads - length (Map.keys reads)
  let _hist = [ (fromIntegral si :: W16,n) | (si,n) <- Map.toList reads, n > 1 ]
  printf "[#reads=%d, #reps=%d]\n" numReads numRepeats
  printf "[reps:%s]\n" $ show _hist
  pure a
