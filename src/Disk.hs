
module Disk
  ( M (Message,ReadSector), exec
  ) where

import Control.Monad (ap,liftM)
import Data.ByteString qualified as B
import Text.Printf (printf)

import Types

instance Functor M where fmap = liftM
instance Applicative M where pure = Pure; (<*>) = ap
instance Monad M where (>>=) = Bind

data M a where
  Pure :: a -> M a
  Bind :: M a -> (a -> M b) -> M b
  ReadSector :: SectorIndex -> M Sector
  Message :: String -> M ()

data Counts = Counts { numReads :: Int }

exec :: FilePath -> M a -> IO a
exec path m0 = do
  theBS <- B.readFile path
  let
    go :: Counts -> M a -> IO (a,Counts)
    go c = \case
      Pure x -> pure (x,c)
      Bind m f -> do (a,c) <- go c m; go c (f a)
      Message x -> do putStr x; pure ((),c)
      ReadSector si -> do
        --print ("ReadSector",si)
        let Counts{numReads} = c
        pure (mkSector theBS si, c { numReads = 1 + numReads })

  (a,Counts{numReads}) <- go Counts { numReads = 0 } m0
  printf "[#reads=%d]\n" numReads
  pure a
