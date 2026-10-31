
module Fat16
  ( Entry (..)
  , Item(..)
  , File(..)
  , Dir(..)
  , getDirEntries
  , getFileContents
  , echoRedirect
  , mkDir
  , touch
  , rmCommand
  ) where

import Control.Exception (assert)
import Control.Monad.IO.Class (liftIO)
import Data.Bits ((.&.),rotateR)
import Data.ByteString.Internal (c2w)
import Data.Char (toUpper)
import Data.List.Split (splitOn)
import GetSet (get8,get16,get32,set8,set16,set32,getBytes)
import Prelude hiding (interact)
import TheDisk (M,sectorSize)
import Types (W8,W16,W32,SectorIndex,SectorOffset,toChar,bytes16,bytes32,bytesString)
import Volume (bytesPerDirEntry,Vol(..))
import DateTime (Date,Time,dateWord,timeWord)

data Entry = Entry { longName :: String, item :: Item }

data Item = IDir Dir | IFile File

data Dir = Root | Sub { firstCluster :: CN }
data File = File { fileSize :: W32, firstCluster :: CN }

getDirEntries :: Vol -> Dir -> M [Entry]
getDirEntries vol dir = map entryOfDR <$> getDirInfo vol dir

entryOfDR :: DirRecord -> Entry
entryOfDR DirRecord{longName,isDir,fileSize,firstCluster} =
  Entry { longName
        , item =
          if isDir then IDir (Sub {firstCluster})
          else IFile (File{fileSize,firstCluster})
        }

touch :: Vol -> (Date,Time) -> String -> Dir -> M ()
touch vol dt longName dir = do
  let entry = makeStandardEntry dt 0 (makeShort longName) (FileOfSize 0)
  let bytes = encodeEntry longName entry
  writeDirEntry vol bytes dir

echoRedirect :: Vol -> (Date,Time) -> String -> String -> Dir -> M ()
echoRedirect vol dt contents longName dir = do
  let fileSize = fi (length contents)
  let entry = makeStandardEntry dt 0 (makeShort longName) (FileOfSize fileSize)
  let bytes = encodeEntry longName entry
  -- to get byte match: nextFreeCluster; writeDirEntry
  writeDirEntry vol bytes dir
  cn <- nextFreeCluster vol
  setStartingCluster vol longName cn dir
  writeBytesInClusterChain vol cn (bytesString contents)

mkDir :: Vol -> (Date,Time) -> String -> Dir -> M ()
mkDir vol dt longName dir = do
  -- to get byte match: nextFreeCluster; writeDirEntry
  cn <- nextFreeCluster vol
  let entry = makeStandardEntry dt cn (makeShort longName) Subdir
  let bytes = encodeEntry longName entry
  writeDirEntry vol bytes dir
  writeBytesInClusterChain vol cn (emptyDirBytes dt cn dir)

emptyDirBytes :: (Date,Time) -> CN -> Dir -> [W8]
emptyDirBytes dt cn parent =
  (encodeStandardEntry $ makeStandardEntry dt cn "." Subdir) ++
  (encodeStandardEntry $ makeStandardEntry dt (dir2cn parent) ".." Subdir)

dir2cn :: Dir -> CN
dir2cn = \case
  Root -> 0
  Sub cn -> cn

setStartingCluster :: Vol -> String -> CN -> Dir -> M ()
setStartingCluster vol longName cn dir =
  findEntryAddr vol longName dir >>= \case
    Nothing -> undefined "impossible, I just added it"
    Just (si,so) -> do
      set16 si (so + 0x1A) (fi cn) -- Ox1A is offset of start-cluster in normal entry

----------------------------------------------------------------------
-- RM

rmCommand :: Vol -> String -> Dir -> M ()
rmCommand vol longName dir = do
  findEntryAddresses vol longName dir >>= \case
    Nothing -> pure () -- nothing to remove
    Just (longAs,addr) -> do
      markEntryAddrUnused addr
      mapM_ markEntryAddrUnused longAs
      cn <- startingClusterFromAddr addr
      zeroOutClusterAndSizeInfo addr
      zeroBytesInClusterChain vol cn
      clearFatChain vol cn

startingClusterFromAddr :: Addr -> M CN
startingClusterFromAddr (si,so) = fi <$> get16 si (so + 0x1A)

markEntryAddrUnused :: Addr -> M ()
markEntryAddrUnused (si,so) = set8 si so 0xe5

zeroOutClusterAndSizeInfo :: Addr -> M ()
zeroOutClusterAndSizeInfo (si,so) = do
  set16 si (so + 0x1A) 0 -- zero the startingCluster
  set32 si (so + 0x1C) 0 -- zero the file size

clearFatChain :: Vol -> CN -> M ()
clearFatChain vol = loop
  where
    loop :: CN -> M ()
    loop cn = do
      (si,so) <- cn2sio vol cn
      cn' <- CN <$> get16 si so
      set16 si so 0
      let VolumeInfo{sectorsPerFat} = vol
      set16 (si + fi sectorsPerFat) so 0 -- backup FAT
      if cn' == 0xffff then pure () else loop cn'

zeroBytesInClusterChain :: Vol -> CN -> M ()
zeroBytesInClusterChain vol cn = loop cn
  where
    loop :: CN -> M ()
    loop cn = do
      let si = siFromClusterNumber vol cn
      let bytes = replicate 512 0x0
      writeSectorBytes si 0 bytes
      cn' <- stepFatChain vol cn
      if
        | cn' == 0xffff -> do
            pure ()
        | otherwise -> do
            loop cn'


----------------------------------------------------------------------
-- FIND

type Addr = (SectorIndex,SectorOffset)

findEntryAddr :: Vol -> String -> Dir -> M (Maybe Addr)
findEntryAddr vol sought = \case
  Root -> findRootEntryAddr vol sought
  Sub cn -> findSubEntryAddr vol sought cn

findRootEntryAddr :: Vol -> String -> M (Maybe Addr)
findRootEntryAddr VolumeInfo{rootDirBegin} sought = loop "" rootDirBegin 0
  where
    loop :: String -> SectorIndex -> Int -> M (Maybe Addr)
    loop acc si i = if
      | i >= 16 -> do
          loop acc (si+1) 0
      | otherwise -> do
          let so = fi bytesPerDirEntry * fi i
          DirRecord{kind} <- getDirRecord acc si so
          case kind of
            DRK_EndOfDir -> pure Nothing
            DRK_Unused -> loop acc si (i+1)
            DRK_Long -> do
              part <- getLongNameChunk (si,so)
              loop (part++acc) si (i+1)
            DRK_Normal -> if
              | acc == sought -> pure (Just (si,so))
              | otherwise -> loop "" si (i+1)

findSubEntryAddr :: Vol -> String -> CN -> M (Maybe Addr)
findSubEntryAddr vol sought cn = loop "" cn 0
  where
    VolumeInfo{sectorsPerCluster} = vol
    recordsPerCluster = 16 * fromIntegral sectorsPerCluster
    loop :: String -> CN -> Int -> M (Maybe Addr)
    loop acc cn i = if
      | i >= recordsPerCluster -> do
          cn' <- stepFatChain vol cn
          if
            | (cn' /= 0xffff) ->  loop acc cn' 0
            | otherwise -> pure Nothing
      | otherwise -> do
          let si = siFromClusterNumber vol cn -- TODO: avoid repeat each step
          let so = fi (bytesPerDirEntry * i)
          DirRecord{kind} <- getDirRecord acc si so
          case kind of
            DRK_EndOfDir -> pure Nothing
            DRK_Unused -> loop acc cn (i+1)
            DRK_Long -> do
              part <- getLongNameChunk (si,so)
              loop (part++acc) cn (i+1)
            DRK_Normal -> if
              | acc == sought -> pure (Just (si,so))
              | otherwise -> loop "" cn (i+1)

----------------------------------------------------------------------
-- FAT

nextFreeCluster :: Vol -> M CN
nextFreeCluster vol = do
  cn <- nextFreeCluster1 vol
  --_see ("nextFreeCluster->",cn)
  pure cn

nextFreeCluster1 :: Vol -> M CN
nextFreeCluster1 vol = loop (CN 3) -- 2 is always unused?
  where
    loop :: CN -> M CN
    loop cn = do
      cn' <- stepFatChain vol cn
      if cn' /= 0
        then loop (cn+1)
        else
        do
          markFinalCluster vol cn
          pure cn

markFinalCluster :: Vol -> CN -> M ()
markFinalCluster vol cn = setNextCluser vol cn 0xffff

setNextCluser :: Vol -> CN -> W16 -> M ()
setNextCluser vol cn w16 = do
  --_see ("setNextCluser",cn,"=",w16)
  let VolumeInfo{sectorsPerFat} = vol
  (si,so) <- cn2sio vol cn
  set16 si so w16
  set16 (si + fi sectorsPerFat) so w16 -- backup FAT

----------------------------------------------------------------------
-- CONTENTS

writeBytesInClusterChain :: Vol -> CN -> [W8] -> M ()
writeBytesInClusterChain vol cn = loop cn
  where
    loop :: CN -> [W8] -> M ()
    loop cn bytes = do
      let si = siFromClusterNumber vol cn
      if length bytes <= sectorSize then writeSectorBytes si 0 bytes else do
        let (front,rest) = splitAt sectorSize bytes
        writeSectorBytes si 0 front
        cn' <- nextFreeCluster vol
        setNextCluser vol cn (fi cn')
        loop cn' rest

writeSectorBytes :: SectorIndex -> SectorOffset -> [W8] -> M ()
writeSectorBytes si so0 bytes = do
  assert (fi so0 + length bytes <= sectorSize) $ do
  sequence_ [ set8 si so b | (so,b) <- zip [so0..] bytes ]

----------------------------------------------------------------------
-- DIRECTORY write

writeDirEntry :: Vol -> [W8] -> Dir -> M ()
writeDirEntry vol bytes = \case
  Root -> writeRootDirEntry vol bytes
  Sub cn -> writeSubDirEntry vol bytes cn

writeRootDirEntry :: Vol -> [W8] -> M ()
writeRootDirEntry vol bytes = do
  (si,so) <- findRootDirNextEntry vol
  let remainingSpaceInSector = sectorSize - fi so
  if (length bytes <= remainingSpaceInSector) then writeSectorBytes si so bytes else do
    let (front,rest) = splitAt remainingSpaceInSector bytes
    -- Can a long name entry cause a further sector boundary crossing?
    assert (length rest <= sectorSize) $ do -- assert not
    writeSectorBytes si so front
    writeSectorBytes (si+1) 0 rest

findRootDirNextEntry :: Vol -> M (SectorIndex,SectorOffset)
findRootDirNextEntry VolumeInfo{rootDirBegin} = loop rootDirBegin 0
  where
    loop :: SectorIndex -> SectorOffset -> M (SectorIndex,SectorOffset)
    loop si so = if
      | fi so >= sectorSize -> do
          loop (si+1) 0
      | otherwise -> do
          DirRecord{kind} <- getDirRecord "" si so
          case kind of
            DRK_EndOfDir -> pure (si,so)
            _ -> loop si (so + fi bytesPerDirEntry)

writeSubDirEntry :: Vol -> [W8] -> CN -> M ()
writeSubDirEntry vol bytes cn = do
  (cn,si,so) <- findSubDirNextEntry vol cn -- we might already be in a differennt cluster
  let remainingSpaceInSector = sectorSize - fi so
  if (length bytes <= remainingSpaceInSector) then writeSectorBytes si so bytes else do
    let (front,rest) = splitAt remainingSpaceInSector bytes
    -- Can a long name entry cause a further sector boundary crossing?
    assert (length rest <= sectorSize) $ do -- assert not
    writeSectorBytes si so front
    cn' <- nextCluster vol cn
    let si' = siFromClusterNumber vol cn'
    writeSectorBytes si' 0 rest

findSubDirNextEntry :: Vol -> CN -> M (CN,SectorIndex,SectorOffset)
findSubDirNextEntry vol cn = loop cn (siFromClusterNumber vol cn) 0
  where
    loop :: CN -> SectorIndex -> SectorOffset -> M (CN,SectorIndex,SectorOffset)
    loop cn si so = if
      | fi so >= sectorSize -> do
          cn' <- nextCluster vol cn
          let si' = siFromClusterNumber vol cn'
          loop cn' si' 0
      | otherwise -> do
          DirRecord{kind} <- getDirRecord "" si so
          case kind of
            DRK_EndOfDir -> pure (cn,si,so)
            _ -> loop cn si (so + fi bytesPerDirEntry)

nextCluster :: Vol -> CN -> M CN
nextCluster vol cn = do
  cn' <- nextCluster1 vol cn
  --_see ("nextCluser",cn,"->",cn')
  pure cn'

nextCluster1 :: Vol -> CN -> M CN
nextCluster1 vol cn = do
  cn' <- stepFatChain vol cn
  if
    | cn' /= 0xffff -> do
        pure cn'
    | otherwise -> do
        cn' <- nextFreeCluster vol
        --let cn' = 1 + cn'0
        setNextCluser vol cn (fi cn')
        pure cn'

----------------------------------------------------------------------
-- DIRECTORY encode

encodeEntry :: String -> StandardEntry -> [W8]
encodeEntry longName entry = do
  let StandardEntry{shortName} = entry
  let chop = \case [] -> []; str -> let (h,t) = splitAt 13 str in h : chop t
  let parts = chop longName
  encodeLongNameEnties shortName parts ++ encodeStandardEntry entry

encodeLongNameEnties :: String -> [String] -> [W8]
encodeLongNameEnties shortName parts = do
  let n = length parts
  let headerBytes = ([fi (0x40 + n)] ++ reverse [ 1 .. fi (n - 1) ])
  let rotAdd acc c = acc `rotateR` 1 + fi (c2w c)
  let checkSum = foldl rotAdd 0 shortName
  concat [ encodeLongPart byte0 part checkSum
         | (byte0,part) <- zip headerBytes (reverse parts)
         ]

encodeLongPart :: W8 -> String -> W8 -> [W8]
encodeLongPart byte0 str checkSum = do
  assert (length str <= 13) $ do
  let pad3 = [0xf,0,checkSum]
  let bs = byte0 : concat [ [b,0] | b <- bytesString str ] ++ [0,0] ++ repeat 0xff
  let stream = take 11 bs ++ pad3 ++ take 12 (drop 11 bs) ++ [0,0] ++ drop 23 bs
  take 32 stream

data StandardEntry = StandardEntry
  { shortName :: String
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

data Thing = FileOfSize W32 | Subdir

makeShort :: String -> String
makeShort longName = do
  let
    (moreThanOneDot,base,ext) = case splitOn "." (reverse longName) of
      [] -> undefined -- empty name not allowed
      [_] -> (False,longName,"")
      [extR,baseR] -> (False,reverse baseR, reverse extR)
      extR:restR -> (True,reverse (concat restR), reverse extR)
  let wontFit = length base > 8 || length ext > 3
  let needTilda = wontFit || moreThanOneDot
  let baseTilda = if needTilda then take 6 base ++ "~1" else base
  let short8 = trunOrPadSpace 8 (map toUpper baseTilda)
  let ext3 = trunOrPadSpace 3 (map toUpper ext)
  short8 ++ ext3

makeStandardEntry :: (Date,Time) -> CN -> String -> Thing -> StandardEntry
makeStandardEntry (date,time) startingCluster shortName thing = do
  let (fileSize,attrByte) = case thing of
        FileOfSize n -> (n, 0x20)
        Subdir -> (0, 0x10)
  StandardEntry { shortName
                , attrByte
                , bdate = date
                , btime = time
                , adate = date
                , mtime = time
                , mdate = date
                , startingCluster
                , fileSize
                }

encodeStandardEntry :: StandardEntry -> [W8]
encodeStandardEntry e = do
  let StandardEntry {shortName,attrByte,btime,bdate,adate,mtime,mdate,startingCluster,fileSize} = e
  bytesString (trunOrPadSpace 11 shortName)
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

trunOrPadSpace :: Int -> String -> String
trunOrPadSpace n s = take n (s ++ repeat ' ')

_see :: Show a => a -> M ()
_see = liftIO .print

fi :: (Integral a, Num b) => a -> b
fi x = fromIntegral x

getDirInfo :: Vol -> Dir -> M [DirRecord]
getDirInfo vol = \case
  Root -> getRootDirInfo vol
  Sub cn -> getSubDirInfo vol cn

getRootDirInfo :: Vol -> M [DirRecord]
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
              part <- getLongNameChunk (si,so)
              loop (part++acc) si (i+1)
            DRK_Normal -> do
              drs <- loop "" si (i+1)
              pure (dr:drs)

getSubDirInfo :: Vol -> CN -> M [DirRecord]
getSubDirInfo vol cn = loop "" cn 0
  where
    VolumeInfo{sectorsPerCluster} = vol
    recordsPerCluster = 16 * fromIntegral sectorsPerCluster
    loop :: String -> CN -> Int -> M [DirRecord]
    loop acc cn i = if
      | i >= recordsPerCluster -> do
          cn' <- stepFatChain vol cn
          if
            | cn' /= 0xffff -> do
                loop acc cn' 0
            | otherwise -> do
                pure []
      | otherwise -> do
          let si = siFromClusterNumber vol cn -- TODO: avoid repeat each step
          let so = fi (bytesPerDirEntry * i)
          dr@DirRecord{kind} <- getDirRecord acc si so
          case kind of
            DRK_EndOfDir -> pure []
            DRK_Unused -> loop acc cn (i+1)
            DRK_Long -> do
              part <- getLongNameChunk (si,so)
              loop (part++acc) cn (i+1)
            DRK_Normal -> do
              drs <- loop "" cn (i+1)
              pure (dr:drs)

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

getFileContents :: Vol -> File -> M String
getFileContents vol dr = do
  let VolumeInfo{sectorsPerCluster} = vol
  let File{fileSize,firstCluster} = dr
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
          assert (cn' /= 0xffff) $ do
          rest <- loop cn' (size - clusterSize)
          pure (start ++ rest)
  loop firstCluster (fi fileSize)

-- This become a big performace drain as the disk fill up.
-- TOOO: support some kind of FAT caching
stepFatChain ::Vol -> CN -> M CN
stepFatChain vol cn = do
  (si,so) <- cn2sio vol cn
  cn' <- CN <$> get16 si so
  -- _see("stepFatChain",cn,(si,so),"-->",cn')
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

----------------------------------------------------------------------
-- new directory stuff -- TODO: make this subsume all previous dir decodin

findEntryAddresses :: Vol -> String -> Dir -> M (Maybe ([Addr],Addr))
findEntryAddresses vol sought dir = do
  findSlotGroup vol sought dir >>= \case
    Nothing -> pure Nothing
    Just (longs,short) -> do
      pure $ Just (map longAddr longs, shortAddr short)

findSlotGroup :: Vol -> String -> Dir -> M (Maybe SlotGroup)
findSlotGroup vol sought dir = do
  let
    loop :: [SlotGroup] -> Maybe SlotGroup
    loop = \case
      [] -> Nothing
      g:gs -> if longNameOfGroup g == sought then Just g else loop gs
  slots <- dirEntrySlots vol dir
  pure $ loop (groupSlots slots)

type SlotGroup = ([LongSlot],ShortSlot)

longNameOfGroup :: SlotGroup -> String
longNameOfGroup (longs,_) =
  concat [ nameChunk | LongSlot{nameChunk} <- reverse longs ]

groupSlots :: [EntrySlot] -> [SlotGroup]
groupSlots = loop []
  where
    loop :: [LongSlot] -> [EntrySlot] -> [SlotGroup]
    loop longs = \case
      [] -> undefined -- we always have a end-slot
      slot:slots -> do
        case slot of
          ES_Long long -> loop (long:longs) slots
          ES_Short short -> (reverse longs, short) : loop [] slots
          ES_Open{} -> undefined -- TODO
          ES_OpenEnd -> do
            assert (case longs of [] -> True; _ -> False) $ do
            []

dirEntrySlots :: Vol -> Dir -> M [EntrySlot]
dirEntrySlots vol dir = do
  addrs <- dirEntryAdresses vol dir
  sequence [ decodeEntrySlot vol addr | addr <- addrs ]

dirEntryAdresses :: Vol -> Dir -> M [Addr]
dirEntryAdresses vol dir = do
  sis <- dirSectors vol dir
  pure [ addr | si <- sis, addr <- entryAddressesAtSector si ]

dirSectors :: Vol -> Dir -> M [SectorIndex]
dirSectors vol = \case
  Root -> pure $ rootSectors vol
  Sub cn -> expandClusterChainFrom vol cn

rootSectors :: Vol -> [SectorIndex]
rootSectors vol = do
  let VolumeInfo{rootDirBegin} = vol
  let n = 1 -- TODO
  take n [rootDirBegin .. ]

expandClusterChainFrom :: Vol -> CN -> M [SectorIndex]
expandClusterChainFrom vol = loop
  where
    loop cn = do
      let si = siFromClusterNumber vol cn
      (si:) <$> do
        nextCN vol cn >>= \case
          Nothing -> pure []
          Just cn' -> loop cn'

-- This is the core computation...
siFromClusterNumber :: Vol -> CN -> SectorIndex
siFromClusterNumber vol cn = do
  let VolumeInfo{sectorsPerCluster,clusterBegin} = vol
  clusterBegin + (fi cn - 2) * fi sectorsPerCluster

nextCN :: Vol -> CN -> M (Maybe CN)
nextCN _ _ = undefined -- TODO

entryAddressesAtSector :: SectorIndex -> [Addr] -- # (512/32 = 16)
entryAddressesAtSector si = do
  let sizeDirSlot = 32
  let numEntrySlotsInSector = sectorSize `div` sizeDirSlot
  [ (si,32 * so) | so <- take numEntrySlotsInSector [0..] ]

decodeEntrySlot :: Vol -> Addr -> M EntrySlot
decodeEntrySlot _vol addr@(si,so) = do
  firstByte <- get8 si so
  attr <- get8 si (so+0xB)
  if
    | firstByte == 0x00 -> pure ES_OpenEnd
    | firstByte == 0xE5 -> pure ES_Open
    | (attr .&. 0xF) == 0xF -> do
        nameChunk <- getLongNameChunk addr
        pure (ES_Long (LongSlot {addr,nameChunk}))
    | otherwise -> do
        pure (ES_Short (ShortSlot {addr}))

-- grabbed this from earlier coding
getLongNameChunk :: Addr -> M String
getLongNameChunk (si,so) = do
  bs <- sequence [ get8 si (so+i) | i <- [1,3,5,7,9, 14,16,18,20,22,24, 28,30] ]
  pure [ toChar b | b <- takeWhile (/=0) bs ]

data EntrySlot
  = ES_Long LongSlot
  | ES_Short ShortSlot
  | ES_Open
  | ES_OpenEnd

data LongSlot = LongSlot { addr :: Addr, nameChunk :: String }
data ShortSlot = ShortSlot { addr :: Addr }

longAddr :: LongSlot -> Addr
longAddr LongSlot{addr} = addr

shortAddr :: ShortSlot -> Addr
shortAddr ShortSlot{addr} = addr
