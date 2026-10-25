
module Fat16
  ( VolumeInfo
  , DirRecord(..) -- TODO: dont expose this
  , CN
  , getVolumeInfo
  , getRootDirInfo
  , getDirInfo
  , getFileContents

  , echoRedirect
  ) where

import TheDisk (M (ReadSector,WriteSector))
import Control.Exception (assert)
import Control.Monad.IO.Class (liftIO)
import Data.Bits ((.&.),(.|.),shiftL,rotateR)
import Prelude hiding (interact)
import Types
import Data.ByteString.Internal (c2w)
import Data.Char (toUpper)

----------------------------------------------------------------------
-- DEV: echo xxx > yyy (at root) -- match the bytes changed
-- start by trying to change any bytes!
echoRedirect :: Vol -> String -> String -> M ()
echoRedirect vol contents longName = do -- echo contents > longName

  --see (longName,contents)

  n <- numRootDirInfo vol
  let so :: SectorOffset = fi (32*n)
  --see (n,so)

  cn <- nextFreeCluster vol
  markClusterUsed vol cn
  let si = siFromClusterNumber vol cn

  --see ("next free cluster",cn)
  --see ("@ sector index",si)

  set8 si 3 0x0a
  assert (length contents < 256) $ do
  writeBytesInSector si contents

  let VolumeInfo{rootDirBegin} = vol

  let si = rootDirBegin

--  let longName = "xxxxxxxxxxxxxyyyyyyyyyyyyy"

  let max8 s = if length s > 8 then take 6 s ++ "~1" else take 8 (s ++ repeat ' ')
  let shortName8 = max8 (map toUpper longName)

  let extension3 = "   " -- TODO: split longname on dot

  let attrByte = 0x20 -- archive flag
  let date = Date { year = 1997, month = 12, day = 29 }
  let time = Time { hour = 12, minute = 25, second = 02 }
  let startingCluster = cn
  let fileSize = fi (length contents)
  let entry =
        Entry { shortName8, extension3, attrByte
              , bdate = date
              , btime = time
              , adate = date
              , mtime = time
              , mdate = date
              , startingCluster, fileSize
              }

  let
    chop :: String -> [String]
    chop s =
      if length s <= 13 then [s] else
        take 13 s : chop (drop 13 s)

  let parts = chop longName

  let headerBytes = take (length parts - 1) [1..] ++ [fi (0x40 + length parts)]


  let shortName = shortName8 ++ extension3
  let checkSum :: W8 = foldl f 0 shortName
        where
          f :: W8 -> Char -> W8
          f acc c = acc `rotateR` 1 + fi (c2w c)

  --see (shortName, "checkSum:",checkSum)

  sequence_
    [ writeLongPart si (so+ 32*i) h part checkSum
    | (i,(h,part)) <- zip [0..] (reverse (zip headerBytes parts))
    ]

  writeEntry si (so + 32 * fi (length parts)) entry

numRootDirInfo :: VolumeInfo -> M Int
numRootDirInfo VolumeInfo{rootDirBegin} = loop 0 rootDirBegin 0
  where
    loop :: Int -> SectorIndex -> Int -> M Int
    loop n si i = if
      | i >= 16 -> do
          loop n (si+1) 0
      | otherwise -> do
          let so = fi bytesPerDirEntry * fi i
          DirRecord{kind} <- getDirRecord "" si so
          case kind of
            DRK_EndOfDir -> pure n
            DRK_Unused -> loop (n+1) si (i+1)
            DRK_Long -> do
              --part <- getLongNamePart si so
              loop (n+1) si (i+1)
            DRK_Normal -> do
              loop (n+1) si (i+1)


writeLongPart :: SectorIndex -> SectorOffset -> W8 -> String -> W8 -> M ()
writeLongPart si so byte0 str checkSum = do
  assert (length str <=13) $ do
  let pad3 = [0xf,0,checkSum]
  let bs = byte0 : concat [ [b,0] | b <- bytesString str ] ++ [0,0] ++ repeat 0xff
  let stream = take 11 bs ++ pad3 ++ take 12 (drop 11 bs) ++ [0,0] ++ drop 23 bs
  writeBytes "long" 32 si so (take 32 stream)



data Entry = Entry
  { shortName8 :: String
  , extension3 :: String
  , attrByte :: W8
  , bdate :: Date -- birth
  , btime :: Time -- birth
  , adate :: Date -- access
  , mtime :: Time -- mod
  , mdate :: Date -- mod
  , startingCluster :: CN
  , fileSize :: W32
  }
  deriving Show

writeEntry :: SectorIndex -> SectorOffset -> Entry -> M ()
writeEntry si so e = do
  let Entry {shortName8,extension3,attrByte,btime,bdate,adate,mtime,mdate,startingCluster,fileSize} = e
  writeBytes "entry" 32 si so $
    bytesString shortName8
    ++ bytesString extension3
    ++ [attrByte]
    ++ [0] -- reserved for NT
    ++ [0] -- birth 1/100s of a second
    ++ bytes16 (timeWord btime)
    ++ bytes16 (dateWord bdate)
    ++ bytes16 (dateWord adate)
    ++ [0,0] -- high 16bits of cluster number; always 0 for FAT-16
    ++ bytes16 (timeWord mtime)
    ++ bytes16 (dateWord mdate)
    ++ bytes16 (fi startingCluster)
    ++ bytes32 fileSize


writeBytes :: String -> Int -> SectorIndex -> SectorOffset -> [W8] -> M ()
writeBytes _who w si so bytes = do
  --see (who,length bytes, w)
  assert (length bytes == w) $ do
  sequence_ [ set8 si (so + i) b | (i,b) <- zip [0..] bytes ]

bytes16 :: W16 -> [W8]
bytes16 w = [lo,hi] where HL{lo,hi} = split16 w

bytes32 :: W32 -> [W8]
bytes32 w = bytes16 lo ++ bytes16 hi where HL{lo,hi} = split32 w

bytesString :: String -> [W8]
bytesString str = [ fi (c2w c) | c <- str ]

data Date = Date
  { year  :: Int -- (7 bits) 0..119 (1980..2099)
  , month :: Int -- (4 bits) 1..12
  , day   :: Int -- (5 bits) 1..31
  }
  deriving Show

dateWord :: Date -> W16
dateWord Date{year,month,day} =
  (((fi year - 1980) `mod` 128) `shiftL` 9) .|.
  ((fi month `mod` 16) `shiftL` 5) .|.
  (fi day `mod` 32)

data Time = Time
  { hour   :: Int -- (5 bits) 0..23
  , minute :: Int -- (6 bits) 0..59
  , second :: Int -- (5 bits) 0..29 (two seconds)
  }
  deriving Show

timeWord :: Time -> W16
timeWord Time{hour,minute,second} =
  ((fi hour `mod` 32) `shiftL` 11) .|.
  ((fi minute `mod` 64) `shiftL` 5) .|.
  (fi (second `div` 2) `mod` 32)


----------------------------------------------------------------------

type Vol = VolumeInfo


writeBytesInSector :: SectorIndex -> String -> M ()
writeBytesInSector si string = do
  let n = length string
  assert (n <= 256) $ do
  let bytes :: [W8] = [ fromIntegral (c2w c) | c <- string ]
  sequence_ [ set8 si i b | (i,b) <- zip [0..] bytes ]
  pure ()

markClusterUsed :: Vol -> CN -> M ()
markClusterUsed vol cn = do
  let VolumeInfo{sectorsPerFat} = vol
  (si,so) <- cn2sio vol cn
  set16 si so 0xffff
  set16 (si + fi sectorsPerFat) so 0xffff

nextFreeCluster :: Vol -> M CN
nextFreeCluster vol = loop (CN 3) -- 2 is always unused?
  where
    loop :: CN -> M CN
    loop cn = do
      cn' <- stepFatChain vol cn
      if cn' /= 0
        then loop (cn+1)
        else pure cn


_see :: Show a => a -> M ()
_see = liftIO .print


----------------------------------------------------------------------

fi :: (Integral a, Num b) => a -> b
fi x = fromIntegral x

bytesPerDirEntry :: Int
bytesPerDirEntry = 32

data VolumeInfo = VolumeInfo
  { sectorsPerCluster :: W8
--  , numberReservedSectors :: W16
--  , numberOfFats :: W8
--  , numberOfRootEntries :: W16
  , sectorsPerFat :: W16
--  , sig :: W16
--  , sectorsForRootDir :: W16
  , fatBegin :: SectorIndex
  , rootDirBegin :: SectorIndex
  , clusterBegin :: SectorIndex
  } deriving Show


getVolumeInfo :: SectorIndex -> M VolumeInfo
getVolumeInfo si = do
  bytesPerSector <- get16 si 0xB
  sectorsPerCluster <- get8 si 0xD
  numberReservedSectors <- get16 si 0xE
  numberOfFats <- get8 si 0x10
  numberOfRootEntries <- get16 si 0x11
  sectorsPerFat <- get16 si 0x16
  sig <- get16 si 0x1FE

  assert (fi bytesPerSector == sectorSize) $ do
  --assert (sectorsPerCluster == 1) $ do
  assert (numberOfFats == 2) $ do
  --assert (numberReservedSectors == 1) $ do
  assert (sig == 0xAA55) $ do

  let numberOfDirEntriesPerSector = fi (sectorSize `div` bytesPerDirEntry)
  assert (numberOfDirEntriesPerSector == 16) $ do
  assert (numberOfRootEntries `mod` numberOfDirEntriesPerSector == 0) $ do
  let sectorsForRootDir = numberOfRootEntries `div` numberOfDirEntriesPerSector

  let fatBegin = si + fi numberReservedSectors
  let rootDirBegin = fatBegin + fi sectorsPerFat * fi numberOfFats
  let clusterBegin = rootDirBegin + fi sectorsForRootDir

  pure VolumeInfo { sectorsPerCluster
                  --, numberReservedSectors
                  --, numberOfFats
                  --, numberOfRootEntries
                  , sectorsPerFat
                  --, sig
                  --, sectorsForRootDir
                  , fatBegin
                  , rootDirBegin
                  , clusterBegin
                  }

getRootDirInfo :: VolumeInfo -> M [DirRecord]
getRootDirInfo VolumeInfo{rootDirBegin} = loop "" rootDirBegin 0
  where
    loop :: String -> SectorIndex -> Int -> M [DirRecord]
    loop acc si i = if
      | i >= 16 -> do
          loop acc (si+1) 0
      | otherwise -> do
          let so = fi bytesPerDirEntry * fi i
          dr@DirRecord{kind} <- getDirRecord acc si so
          case kind of
            DRK_EndOfDir -> pure []
            DRK_Unused -> loop acc si (i+1)
            DRK_Long -> do
              part <- getLongNamePart si so
              loop (part++acc) si (i+1)
            DRK_Normal -> do
              drs <- loop "" si (i+1)
              pure (dr:drs)

getDirInfo :: VolumeInfo -> CN -> M [DirRecord]
getDirInfo vol cn = loop "" cn 0
  where
    VolumeInfo{sectorsPerCluster} = vol
    recordsPerCluster = 16 * fromIntegral sectorsPerCluster
    loop :: String -> CN -> Int -> M [DirRecord]
    loop acc cn i = if
      | i >= recordsPerCluster -> do
          cn' <- stepFatChain vol cn
          loop acc cn' 0
      | otherwise -> do
          let si = siFromClusterNumber vol cn
          let so = fi (bytesPerDirEntry * i)
          dr@DirRecord{kind} <- getDirRecord acc si so
          case kind of
            DRK_EndOfDir -> pure []
            DRK_Unused -> loop acc cn (i+1)
            DRK_Long -> do
              part <- getLongNamePart si so
              loop (part++acc) cn (i+1)
            DRK_Normal -> do
              drs <- loop "" cn (i+1)
              pure (dr:drs)

getLongNamePart :: SectorIndex -> SectorOffset -> M String
getLongNamePart si so = do
  bs <- sequence [ get8 si (so+i) | i <- [1,3,5,7,9, 14,16,18,20,22,24, 28,30] ]
  pure [ toChar b | b <- takeWhile (/=0) bs ]

getDirRecord :: String -> SectorIndex -> SectorOffset -> M DirRecord
getDirRecord acc si so = do
  firstByte <- get8 si so
  attr <- get8 si (so+0xB)
  let
    kind = if
      | firstByte == 0x00 -> DRK_EndOfDir
      | firstByte == 0xE5 -> DRK_Unused
      | (attr .&. 0xF) == 0xF -> DRK_Long
      | otherwise -> DRK_Normal
  firstCluster <- CN <$> get16 si (so + 0x1A)
  fileSize <- get32 si (so + 0x1C)
  shortName <- map toChar <$> getBytes si so 11
  let longName = if
        | shortName == ".          " -> "."
        | shortName == "..         " -> ".."
        | otherwise -> acc
  let isDir = (attr .&. 0x10) /= 0
  pure DirRecord { kind,attr,firstCluster,fileSize,shortName,longName,isDir }

getFileContents :: VolumeInfo -> DirRecord -> M String
getFileContents vol dr = do
  let VolumeInfo{sectorsPerCluster} = vol
  let DirRecord{fileSize,firstCluster} = dr
  let clusterSize = sectorSize * fi sectorsPerCluster
  let
    loop :: CN -> Int -> M String
    loop cn size = if
      | size <= clusterSize -> do
          let si = siFromClusterNumber vol cn
          map toChar <$> getBytes si 0 (fi size)
      | otherwise -> do
          let si = siFromClusterNumber vol cn
          start <- map toChar <$> getBytes si 0 clusterSize
          cn' <- stepFatChain vol cn
          rest <- loop cn' (size - clusterSize)
          pure (start ++ rest)
  loop firstCluster (fi fileSize)

stepFatChain ::VolumeInfo -> CN -> M CN
stepFatChain vol cn = do
  (si,so) <- cn2sio vol cn
  cn' <- CN <$> get16 si so
  --see("stepFatChain",cn,(si,so),"-->",cn')
  pure cn'


cn2sio :: Vol -> CN -> M (SectorIndex,SectorOffset)
cn2sio vol cn = do
  let VolumeInfo{fatBegin} = vol
  let bytePerChainWord :: Int = 2
  let n = bytePerChainWord * fi cn
  let si = fi (n `div` sectorSize) + fatBegin
  let so = fi (n `mod` sectorSize)
  --see ("cn2sio",cn,"->",si,so)
  pure (si,so)

siFromClusterNumber :: VolumeInfo -> CN -> SectorIndex
siFromClusterNumber vol cn = do
  let VolumeInfo{sectorsPerCluster,clusterBegin} = vol
  clusterBegin + (fi cn - 2) * fi sectorsPerCluster

data DirRecord = DirRecord
  { kind :: DirRecordKind
  , attr :: W8
  , firstCluster :: CN
  , fileSize :: W32
  , shortName :: String
  , longName :: String
  , isDir :: Bool
  } deriving Show

-- Cluster Number
newtype CN = CN W16 deriving (Num,Integral,Real,Enum,Ord,Eq,Show)

data DirRecordKind = DRK_Normal | DRK_Long | DRK_Unused | DRK_EndOfDir
  deriving (Show,Eq)

getBytes :: SectorIndex -> SectorOffset -> Int -> M [W8]
getBytes si so n = sequence [ get8 si (so + fi i) | i <- [0.. n-1] ]

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
  sec <- ReadSector si
  let byte = readInSector sec so
  pure byte


set16 :: SectorIndex -> SectorOffset -> W16 -> M ()
set16 si so w = do
  let HL{hi,lo} = split16 w
  set8 si so lo
  set8 si (so+1) hi

set8 :: SectorIndex -> SectorOffset -> W8 -> M ()
set8 si so byte = do
  sec <- ReadSector si
  let sec' = writeInSector sec so byte
  WriteSector si sec'
