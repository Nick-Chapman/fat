
module GetSet where

import TheDisk --(M, readSector)
import Types -- (HL(..),SectorIndex,SectorOffset,W8,W16,mk16,sectorSize,readInSector)

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
  let byte = readInSector sec so
  pure byte

set16 :: SectorIndex -> SectorOffset -> W16 -> M ()
set16 si so w = do
  let HL{hi,lo} = split16 w
  set8 si so lo
  set8 si (so+1) hi

set8 :: SectorIndex -> SectorOffset -> W8 -> M ()
set8 si so byte = do
  sec <- readSector si
  let sec' = writeInSector sec so byte
  writeSector si sec'

getBytes :: SectorIndex -> SectorOffset -> Int -> M [W8]
getBytes si so n = sequence [ get8 si (so + fromIntegral i) | i <- [0.. n-1] ]
