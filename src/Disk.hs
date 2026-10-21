
module Disk
  ( M (Message,ReadSector), exec
  ) where

import Control.Monad (ap,liftM)
import Data.ByteString qualified as B
import Text.Printf (printf)
import Data.Set as Set
import Types

instance Functor M where fmap = liftM
instance Applicative M where pure = Pure; (<*>) = ap
instance Monad M where (>>=) = Bind

data M a where
  Pure :: a -> M a
  Bind :: M a -> (a -> M b) -> M b
  ReadSector :: SectorIndex -> M Sector
  Message :: String -> M ()

data Tracking = Tracking
  { numReads :: Int
  , indexes :: Set SectorIndex -- which have ever been read
  }

initTracking :: Tracking
initTracking = Tracking { numReads = 0, indexes = Set.empty }

exec :: FilePath -> M a -> IO a
exec path m0 = do
  theBS <- B.readFile path
  let
    go :: Tracking -> M a -> IO (a,Tracking)
    go c = \case
      Pure x -> pure (x,c)
      Bind m f -> do (a,c) <- go c m; go c (f a)
      Message x -> do putStr x; pure ((),c)
      ReadSector si -> do
        --print ("ReadSector",si)
        let Tracking{numReads,indexes} = c
        let c' = c { numReads = 1 + numReads
                   , indexes = Set.insert si indexes }
        pure (mkSector theBS si, c')

  (a,Tracking{numReads,indexes}) <- go initTracking m0
  printf "[#reads=%d, #unique=%d]\n" numReads (Set.size indexes)
  pure a
