
module GetSet where

import TheDisk (M, readSector, writeSector, readInSector, writeInSector)
import Types (HL(..),SectorIndex,SectorOffset,W8,W16,W32,mk16,mk32,split16)

get32 :: SectorIndex -> SectorOffset -> M W32
get32 si so = do
  lo <- get16 si so
  hi <- get16 si (so+2)
  pure (mk32 HL {hi,lo})

get16 :: SectorIndex -> SectorOffset -> M W16
get16 si so = do
  lo <- get8 si so
  hi <- get8 si (so+1)
  pure (mk16 HL{hi,lo})

get8 :: SectorIndex -> SectorOffset -> M W8
get8 si so = do
  sec <- readSector si
  readInSector sec so

set16 :: SectorIndex -> SectorOffset -> W16 -> M ()
set16 si so w = do
  let HL{hi,lo} = split16 w
  set8 si so lo
  set8 si (so+1) hi

set8 :: SectorIndex -> SectorOffset -> W8 -> M ()
set8 si so byte = do
  sec <- readSector si
  writeInSector sec so byte
  writeSector si sec

getBytes :: SectorIndex -> SectorOffset -> Int -> M [W8]
getBytes si so n = sequence [ get8 si (so + fromIntegral i) | i <- [0.. n-1] ]
