
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
  , removeDirEntry
  ) where

import CachedDisk (M,sectorSize,volume,getLastAllocatedCN)
import Control.Exception (assert)
import Control.Monad (when)
import Control.Monad.IO.Class (liftIO)
import Data.Bits ((.&.),rotateR)
import Data.ByteString.Internal (c2w)
import Data.Char (toUpper)
import Data.IORef (readIORef,writeIORef)
import Data.List.Split (splitOn)
import DateTime (Date,Time,dateWord,timeWord,fineTimeByte)
import GetSet (get8,get16,get32,set8,set16,set32,getBytes)
import Prelude hiding (interact)
import Types (W8,W16,W32,SectorIndex,SectorOffset,toChar,bytes16,bytes32,bytesString)
import Volume (bytesPerDirEntry,Vol(..),CN(..))

data Entry = Entry { longName :: String, item :: Item } deriving (Eq,Show)
data Item = IDir Dir | IFile File deriving (Eq,Show)
data Dir = Root | Sub { firstCluster :: CN } deriving (Eq,Show)
data File = File { fileSize :: W32, firstCluster :: CN } deriving (Eq,Show)

getDirEntries :: Dir -> M [Entry]
--getDirEntries = case switch of Old -> _old_getDirEntries; New -> _new_getDirEntries
getDirEntries = new_getDirEntries

{-
_comp_getDirEntries :: Dir -> M [Entry]
_comp_getDirEntries dir = do
  --liftIO $ print ("getDirEntries", dir)
  es1 <- _old_getDirEntries dir
  es2 <- _new_getDirEntries dir
  if (es1 == es2) then pure es1 else do
    liftIO (print ("es1...", length es1))
    liftIO $ mapM_ print es1
    liftIO (print ("es2...", length es2))
    liftIO $ mapM_ print es2
    undefined

_old_getDirEntries :: Dir -> M [Entry]
_old_getDirEntries dir = map entryOfDR <$> getDirInfo dir
-}


-- TODO: understand why fused groupSlots+entryOfGroup is an improvement
data Switch = Old | New
switch :: Switch
switch = New

new_getDirEntries :: Dir -> M [Entry]
new_getDirEntries dir = do
  slots <- dirEntrySlots dir
  case switch of
    Old -> do
      groups <- groupSlots slots
      res <- traverseLinks entryOfGroup groups
      fromLinks res
    New -> do
      res <- groupSlotsIntoEntries slots
      fromLinks res

entryOfGroup :: SlotGroup -> M Entry
entryOfGroup g@(SlotGroup _ short) = do
  item <- itemAtShortSlot short
  shortName <- shortNameAtShortSlot short
  longName <- if
    | shortName == ".          " -> pure "."
    | shortName == "..         " -> pure ".."
    | otherwise -> longNameOfGroup g

  pure Entry { longName, item }

{-
entryOfDR :: DirRecord -> Entry
entryOfDR DirRecord{longName,isDir,fileSize,firstCluster} =
  Entry { longName
        , item =
          if isDir then IDir (if firstCluster == 0 then Root else Sub {firstCluster})
          else IFile (File{fileSize,firstCluster})
        }
-}

touch :: (Date,Time) -> String -> Dir -> M ()
touch dt longName dir = do
  let entry = makeStandardEntry dt 0 (makeShort longName) (FileOfSize 0)
  writeDirEntry longName entry dir

echoRedirect :: (Date,Time) -> String -> String -> Dir -> M ()
echoRedirect dt contents longName dir = do
  let fileSize = fi (length contents)
  let entry = makeStandardEntry dt 0 (makeShort longName) (FileOfSize fileSize)
  -- to get byte match: allocteCluster; writeDirEntry
  writeDirEntry longName entry dir
  cn <- allocteCluster
  setStartingCluster longName cn dir
  writeBytesInClusterChain cn (bytesString contents)

mkDir :: (Date,Time) -> String -> Dir -> M ()
mkDir dt longName dir = do
  -- to get byte match: allocteCluster; writeDirEntry
  cn <- allocteCluster
  let entry = makeStandardEntry dt cn (makeShort longName) Subdir
  writeDirEntry longName entry dir
  writeBytesInClusterChain cn (emptyDirBytes dt cn dir)

emptyDirBytes :: (Date,Time) -> CN -> Dir -> [W8]
emptyDirBytes dt cn parent =
  (encodeStandardEntry $ makeStandardEntry dt cn "." Subdir) ++
  (encodeStandardEntry $ makeStandardEntry dt (dir2cn parent) ".." Subdir)

dir2cn :: Dir -> CN
dir2cn = \case
  Root -> 0
  Sub cn -> cn

setStartingCluster :: String -> CN -> Dir -> M ()
setStartingCluster longName cn dir =
  findEntryAddr longName dir >>= \case
    Nothing -> undefined "impossible, I just added it"
    Just (si,so) -> do
      set16 si (so + 0x1A) (fi cn) -- Ox1A is offset of start-cluster in normal entry


----------------------------------------------------------------------
-- RM

removeDirEntry :: Bool -> String -> Dir -> M ()
removeDirEntry zeroingContents longName dir = do
  findEntryAddresses longName dir >>= \case
    Nothing -> pure () -- nothing to remove
    Just (longAs,addr) -> do
      markEntryAddrUnused addr
      mapM_ markEntryAddrUnused longAs
      cn <- startingClusterFromAddr addr
      when zeroingContents $ do
        zeroOutClusterAndSizeInfo addr
        zeroBytesInClusterChain cn
      clearFatChain cn

startingClusterFromAddr :: Addr -> M CN
startingClusterFromAddr (si,so) = fi <$> get16 si (so + 0x1A)

markEntryAddrUnused :: Addr -> M ()
markEntryAddrUnused (si,so) = set8 si so 0xe5

zeroOutClusterAndSizeInfo :: Addr -> M ()
zeroOutClusterAndSizeInfo (si,so) = do
  set16 si (so + 0x1A) 0 -- zero the startingCluster
  set32 si (so + 0x1C) 0 -- zero the file size

----------------------------------------------------------------------
-- FIND

{-_old_findEntryAddr :: String -> Dir -> M (Maybe Addr) -- TODO: kill
_old_findEntryAddr sought = \case
  Root -> findRootEntryAddr sought
  Sub cn -> findSubEntryAddr sought cn

findRootEntryAddr :: String -> M (Maybe Addr)
findRootEntryAddr sought = do
  VolumeInfo{rootDirBegin} <- volume
  loop "" rootDirBegin 0
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

findSubEntryAddr :: String -> CN -> M (Maybe Addr)
findSubEntryAddr sought cn = loop "" cn 0
  where
    loop :: String -> CN -> Int -> M (Maybe Addr)
    loop acc cn i = do
     VolumeInfo{sectorsPerCluster} <- volume
     let recordsPerCluster = 16 * fromIntegral sectorsPerCluster
     if
      | i >= recordsPerCluster -> do
          stepFatChainL "findSubEntryAddr" cn >>= \case
            Link_UnAllocated -> error "findSubEntryAddr/UnAllocated"
            Link_EndOfChain -> pure Nothing
            Link_NextCluster cn' -> loop acc cn' 0
      | otherwise -> do
          si <- siFromClusterNumber cn -- TODO: avoid repeat each step
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
-}
----------------------------------------------------------------------
-- CONTENTS

writeBytesInClusterChain :: CN -> [W8] -> M ()
writeBytesInClusterChain cn = loop cn
  where
    loop :: CN -> [W8] -> M ()
    loop cn bytes = do
      si <- siFromClusterNumber cn
      if length bytes <= sectorSize then writeSectorBytes si 0 bytes else do
        let (front,rest) = splitAt sectorSize bytes
        writeSectorBytes si 0 front
        cn' <- allocteCluster
        setNextCluser cn (fi cn')
        loop cn' rest

writeSectorBytes :: SectorIndex -> SectorOffset -> [W8] -> M ()
writeSectorBytes si so0 bytes = do
  assert (fi so0 + length bytes <= sectorSize) $ do
  sequence_ [ set8 si so b | (so,b) <- zip [so0..] bytes ]

----------------------------------------------------------------------
-- DIRECTORY write

writeDirEntry :: String -> StandardEntry -> Dir -> M ()
writeDirEntry longName entry dir = do
  let bytes = encodeEntry longName entry
  let size = 2 + ((length longName - 1) `div` 13)
  (dir,addr) <- findDirNextEntry size dir
  writeDirEntryBytesAddr bytes addr dir

writeDirEntryBytesAddr :: [W8] -> Addr -> Dir -> M ()
writeDirEntryBytesAddr bytes addr = \case
  Root -> writeRootDirEntryBytesAddr bytes addr
  Sub cn -> writeSubDirEntryBytesAddr bytes cn addr

writeRootDirEntryBytesAddr :: [W8] -> Addr -> M ()
writeRootDirEntryBytesAddr bytes (si,so) = do
  let remainingSpaceInSector = sectorSize - fi so
  if (length bytes <= remainingSpaceInSector) then writeSectorBytes si so bytes else do
    let (front,rest) = splitAt remainingSpaceInSector bytes
    -- Can a long name entry cause a further sector boundary crossing?
    assert (length rest <= sectorSize) $ do -- assert not
    writeSectorBytes si so front
    writeSectorBytes (si+1) 0 rest

writeSubDirEntryBytesAddr :: [W8] -> CN -> Addr -> M ()
writeSubDirEntryBytesAddr bytes cn (si,so) = do
  let remainingSpaceInSector = sectorSize - fi so
  if (length bytes <= remainingSpaceInSector) then writeSectorBytes si so bytes else do
    let (front,rest) = splitAt remainingSpaceInSector bytes
    -- Can a long name entry cause a further sector boundary crossing?
    assert (length rest <= sectorSize) $ do -- assert not
    writeSectorBytes si so front
    cn' <- nextCluster cn
    si' <- siFromClusterNumber cn'
    writeSectorBytes si' 0 rest

findDirNextEntry :: Int -> Dir -> M (Dir,Addr)
findDirNextEntry size = \case
  Root -> do
    addr <- findRootDirNextEntry size
    pure (Root,addr)
  Sub cn -> do
    (cn,addr) <- findSubDirNextEntry size cn
    pure (Sub cn, addr)

findRootDirNextEntry :: Int -> M Addr
findRootDirNextEntry size = do
  VolumeInfo{rootDirBegin} <- volume
  loop0 (rootDirBegin,0)
  where
    loop0 :: Addr -> M Addr
    loop0 addr = loop size addr addr

    loop :: Int -> Addr -> Addr -> M Addr
    loop need candidateAddr (si,so) = if
      | fi so >= sectorSize -> do
          loop need candidateAddr (si+1, 0)
      | otherwise -> do
          DirRecord{kind} <- getDirRecord "" si so
          case kind of
            DRK_EndOfDir -> pure candidateAddr
            DRK_Unused -> do
              if need == 1
                then pure candidateAddr
                else loop (need-1) candidateAddr (si, so + fi bytesPerDirEntry)
            _ ->
              loop0 (si, so + fi bytesPerDirEntry)

findSubDirNextEntry :: Int -> CN -> M (CN,Addr)
findSubDirNextEntry size cn = do
  si <- siFromClusterNumber cn
  loop0 cn (si, 0)
  where
    loop0 :: CN -> Addr -> M (CN,Addr)
    loop0 cn a = loop size (cn,a) cn a

    loop :: Int -> (CN,Addr) -> CN -> Addr -> M (CN,Addr)
    loop need candidateRes cn (si,so) = if
      | fi so >= sectorSize -> do
          cn' <- nextCluster cn
          si' <- siFromClusterNumber cn'
          loop need candidateRes cn' (si', 0)
      | otherwise -> do
          DirRecord{kind} <- getDirRecord "" si so
          case kind of
            DRK_EndOfDir -> pure (cn,(si,so))
            DRK_Unused -> do
              if need == 1
                then pure candidateRes
                else loop (need-1) candidateRes cn (si, so + fi bytesPerDirEntry)
            _ ->
              loop0 cn (si, so + fi bytesPerDirEntry)


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
  let StandardEntry {shortName
                    ,attrByte
                    ,btime
                    ,bdate
                    ,adate
                    ,mtime
                    ,mdate
                    ,startingCluster
                    ,fileSize
                    } = e
  bytesString (trunOrPadSpace 11 shortName)
    ++ [attrByte]
    ++ [0] -- reserved for NT
    ++ [fineTimeByte btime] -- 0..199 (hundrethds + 100 if on odd second)
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

--_see :: Show a => a -> M ()
--_see = liftIO .print -- TODO: kill comments out debug all over

fi :: (Integral a, Num b) => a -> b
fi x = fromIntegral x

{-
getDirInfo :: Dir -> M [DirRecord]
getDirInfo = \case
  Root -> getRootDirInfo
  Sub cn -> getSubDirInfo cn

getRootDirInfo :: M [DirRecord]
getRootDirInfo = do
  VolumeInfo{rootDirBegin} <- volume
  loop "" rootDirBegin 0
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

getSubDirInfo :: CN -> M [DirRecord]
getSubDirInfo cn = loop "" cn 0
  where
    loop :: String -> CN -> Int -> M [DirRecord]
    loop acc cn i = do
     VolumeInfo{sectorsPerCluster} <- volume
     let recordsPerCluster = 16 * fromIntegral sectorsPerCluster
     if
      | i >= recordsPerCluster -> do
          stepFatChainL "getSubDirInfo" cn >>= \case
            Link_UnAllocated -> error "getSubDirInfo/UnAllocated"
            Link_EndOfChain -> pure []
            Link_NextCluster cn' -> loop acc cn' 0
      | otherwise -> do
          si <- siFromClusterNumber cn -- TODO: avoid repeat each step
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
-}

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
  pure DirRecord { kind
                 -- ,attr
                 ,firstCluster,fileSize
                 --,shortName
                 ,longName,isDir
                 }

getFileContents :: File -> M String
getFileContents dr = do
  VolumeInfo{sectorsPerCluster} <- volume
  let File{fileSize,firstCluster} = dr
  let clusterSize = sectorSize * fi sectorsPerCluster
  let
    loop :: CN -> Int -> M String
    loop cn size = if
      | size <= clusterSize -> do
          si <- siFromClusterNumber cn
          map toChar <$> getBytes si 0 (fi size)
      | otherwise -> do
          si <- siFromClusterNumber cn
          start <- map toChar <$> getBytes si 0 clusterSize
          stepFatChainL "getFileContents" cn >>= \case
            Link_UnAllocated -> error "getFileContents/UnAllocated"
            Link_EndOfChain -> error "getFileContents/EndOfChain"
            Link_NextCluster cn' -> do
              rest <- loop cn' (size - clusterSize)
              pure (start ++ rest)
  loop firstCluster (fi fileSize)

data DirRecord = DirRecord
  { kind :: DirRecordKind
--  , attr :: W8
  , firstCluster :: CN
  , fileSize :: W32
--  , shortName :: String
  , longName :: String
  , isDir :: Bool
  } deriving Show


data DirRecordKind = DRK_Normal | DRK_Long | DRK_Unused | DRK_EndOfDir
  deriving (Show,Eq)

----------------------------------------------------------------------
-- new directory stuff -- TODO: make this subsume all previous dir decodin

findEntryAddr :: String -> Dir -> M (Maybe Addr)
findEntryAddr sought dir =
  findEntryAddresses sought dir >>= pure . \case
    Nothing -> Nothing
    Just (_,addr) -> Just addr


findEntryAddresses :: String -> Dir -> M (Maybe ([Addr],Addr))
findEntryAddresses sought dir = do
  findSlotGroup sought dir >>= \case
    Nothing -> pure Nothing
    Just (SlotGroup longs short) -> do
      pure $ Just (map longAddr longs, shortAddr short)

findSlotGroup :: String -> Dir -> M (Maybe SlotGroup)
findSlotGroup sought dir = do
  slots <- dirEntrySlots dir
  groups <- groupSlots slots >>= fromLinks -- TODO: here next
  let
    loop :: [SlotGroup] -> M (Maybe SlotGroup)
    loop = \case
      [] -> pure Nothing
      g:gs -> do
        longName <- longNameOfGroup g
        if longName == sought then pure (Just g) else loop gs
  loop groups

data SlotGroup = SlotGroup [LongSlot] ShortSlot

longNameOfGroup :: SlotGroup -> M String
longNameOfGroup (SlotGroup longs _) =
  pure $ concat [ nameChunk | LongSlot{nameChunk} <- reverse longs ]


{-
groupSlots :: Links EntrySlot -> M (Links SlotGroup)
groupSlots slotsL = do
  slots <- fromLinks slotsL
  let groups = groupSlots1 slots
  pure (toLinks groups)

groupSlots1 :: [EntrySlot] -> [SlotGroup]
groupSlots1 = loop []
  where
    loop :: [LongSlot] -> [EntrySlot] -> [SlotGroup]
    loop longs = \case
      -- We might not have an end slot; this sector might be full
      [] -> []
      slot:slots -> do
        case slot of
          ES_Long long -> loop (long:longs) slots
          ES_Short short -> (reverse longs, short) : loop [] slots
          --ES_Open{} -> undefined -- TODO -- FInally here now
          ES_Open{} -> loop longs slots -- just do minimal thing for now
          ES_OpenEnd -> do
            assert (case longs of [] -> True; _ -> False) $ do
            []
-}

groupSlotsIntoEntries :: Links EntrySlot -> M (Links Entry)
groupSlotsIntoEntries = loop []
  where
    loop :: [LongSlot] -> Links EntrySlot -> M (Links Entry)
    loop longs = \case
      Nil -> pure Nil
      Cons slot slotsm -> do
        case slot of
          ES_Long long -> do
            slots <- slotsm -- ???
            loop (long:longs) slots
          ES_Short short -> do
            let group = SlotGroup (reverse longs) short
            entry <- entryOfGroup group
            slots <- slotsm -- ???
            pure $ Cons entry (loop [] slots) -- ???
          ES_Open -> do
            slots <- slotsm -- ???
            loop longs slots
          ES_OpenEnd -> do
            assert (case longs of [] -> True; _ -> False) $ do
            pure Nil

groupSlots :: Links EntrySlot -> M (Links SlotGroup)
groupSlots = loop []
  where
    loop :: [LongSlot] -> Links EntrySlot -> M (Links SlotGroup)
    loop longs = \case
      Nil -> pure Nil
      Cons slot slotsm -> do
        case slot of
          ES_Long long -> do
            slots <- slotsm -- ???
            loop (long:longs) slots
          ES_Short short -> do
            let group = SlotGroup (reverse longs) short
            slots <- slotsm -- ???
            pure $ Cons group (loop [] slots) -- ???
          ES_Open -> do
            slots <- slotsm -- ???
            loop longs slots
          ES_OpenEnd -> do
            assert (case longs of [] -> True; _ -> False) $ do
            pure Nil

dirEntrySlots :: Dir -> M (Links EntrySlot)
dirEntrySlots dir = do
  addrs <- dirEntryAdresses dir
  traverseLinks decodeEntrySlot addrs

dirEntryAdresses :: Dir -> M (Links Addr)
dirEntryAdresses dir = dirSectors dir >>= loop
  where
    loop :: Links SectorIndex -> M (Links Addr)
    loop = \case
      Nil -> pure Nil
      Cons si morem -> do
        prefixLinks (entryAddressesAtSector si) $ do
          more <- morem -- ???
          loop more

dirSectors :: Dir -> M (Links SectorIndex)
dirSectors = \case
  Root -> rootSectors
  Sub cn -> expandClusterChainFrom cn

rootSectors :: M (Links SectorIndex)
rootSectors = do
  VolumeInfo{rootDirBegin,clusterBegin} <- volume
  let n = fromIntegral (clusterBegin - rootDirBegin)
  pure $ toLinks (take n [rootDirBegin .. ])

expandClusterChainFrom :: CN -> M (Links SectorIndex)
expandClusterChainFrom = loop
  where
    loop :: CN -> M (Links SectorIndex)
    loop cn = do
      si <- siFromClusterNumber cn
      stepFatChainL "expandClusterChainFrom" cn >>= \case
        Link_UnAllocated -> error "expandClusterChainFrom/UnAllocated"
        Link_EndOfChain -> do
          pure $ Cons si (pure Nil)
        Link_NextCluster cn' -> do
          pure $ Cons si $ do
            more <- loop cn' -- ???
            pure more

entryAddressesAtSector :: SectorIndex -> [Addr] -- # (512/32 = 16)
entryAddressesAtSector si = do
  let sizeDirSlot = 32
  let numEntrySlotsInSector = sectorSize `div` sizeDirSlot
  [ (si,32 * so) | so <- take numEntrySlotsInSector [0..] ]

decodeEntrySlot :: Addr -> M EntrySlot
decodeEntrySlot addr@(si,so) = do
  firstByte <- get8 si so
  attr <- get8 si (so+0xB)
  if
    | firstByte == 0x00 -> pure ES_OpenEnd
    | firstByte == 0xE5 -> pure ES_Open
    | (attr .&. 0xF) == 0xF -> do
        nameChunk <- getLongNameChunk addr -- TODO: Maybe delay this
        pure (ES_Long (LongSlot {addr,nameChunk}))
    | otherwise -> do
        --name <- map toChar <$> getBytes si so 11
        let name = undefined
        --item <- _itemAtAddr addr
        let item = undefined
        pure (ES_Short (ShortSlot {addr,name,item}))

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
  deriving Show

data LongSlot = LongSlot
  { addr :: Addr
  , nameChunk :: String
  }
  deriving Show
data ShortSlot = ShortSlot
  { addr :: Addr
  , name :: String -- TODO: remove?
  , item :: Item -- TODO: remove?
  }
  deriving Show

longAddr :: LongSlot -> Addr
longAddr LongSlot{addr} = addr

shortAddr :: ShortSlot -> Addr
shortAddr ShortSlot{addr} = addr

itemAtShortSlot :: ShortSlot -> M Item
itemAtShortSlot ShortSlot{addr} = itemAtAddr addr
--itemAtShortSlot ShortSlot{item} = pure item

itemAtAddr :: Addr -> M Item
itemAtAddr (si,so) = do
  attr <- get8 si (so+0xB)
  firstCluster <- CN <$> get16 si (so + 0x1A)
  let isDir = (attr .&. 0x10) /= 0
  if | isDir -> do
         pure $ IDir (if firstCluster == 0 then Root
                      else Sub { firstCluster })
     | otherwise -> do
         fileSize <- get32 si (so + 0x1C)
         pure $ IFile (File { fileSize, firstCluster })


shortNameAtShortSlot :: ShortSlot -> M String
shortNameAtShortSlot ShortSlot{addr=(si,so)} = map toChar <$> getBytes si so 11
--shortNameAtShortSlot ShortSlot{name} = pure name

----------------------------------------------------------------------
-- FAT

nextCluster :: CN -> M CN -- allocating if necessary
nextCluster cn = do
  stepFatChainL "nextCluster" cn >>= \case
    Link_UnAllocated -> undefined
    Link_NextCluster cn' -> pure cn'
    Link_EndOfChain -> do
      cn' <- allocteCluster
      setNextCluser cn (fi cn')
      pure cn'

getNextCandidateCN :: M CN
getNextCandidateCN = do
  lastAllocatedCN <- getLastAllocatedCN
  cn <- liftIO $ readIORef lastAllocatedCN
  pure (cn + 1)

setLastAllocatedCN :: CN -> M ()
setLastAllocatedCN cn = do
  lastAllocatedCN <- getLastAllocatedCN
  liftIO $ writeIORef lastAllocatedCN cn

allocteCluster :: M CN
allocteCluster = getNextCandidateCN >>= loop
  where
    loop :: CN -> M CN
    loop cn = do
      stepFatChainL "allocteCluster" cn >>= \case
        Link_EndOfChain -> loop (cn+1)
        Link_NextCluster{} -> loop (cn+1)
        Link_UnAllocated -> do
          setNextCluser cn 0xffff
          setLastAllocatedCN cn
          pure cn

clearFatChain :: CN -> M ()
clearFatChain = loop
  where
    loop :: CN -> M ()
    loop cn = do
      (si,so) <- cn2sio cn
      cn' <- CN <$> get16 si so
      set16 si so 0
      VolumeInfo{sectorsPerFat} <- volume
      set16 (si + fi sectorsPerFat) so 0 -- backup FAT
      if cn' == 0xffff then pure () else loop cn'

zeroBytesInClusterChain :: CN -> M ()
zeroBytesInClusterChain cn = loop cn
  where
    loop :: CN -> M ()
    loop cn = do
      si <- siFromClusterNumber cn
      let bytes = replicate 512 0x0
      writeSectorBytes si 0 bytes
      stepFatChainL "zeroBytesInClusterChain" cn >>= \case
        Link_UnAllocated -> error "zeroBytesInClusterChain/UnAllocated"
        Link_EndOfChain -> pure ()
        Link_NextCluster cn' -> loop cn'

data Link = Link_UnAllocated | Link_NextCluster CN | Link_EndOfChain

stepFatChainL :: String -> CN -> M Link
stepFatChainL _who cn = do
  --_see("stepFatChain",cn,"-->", _who)
  assert (cn /= 0) $ do
  assert (cn /= 0xffff) $ do
  (si,so) <- cn2sio cn
  cn' <- CN <$> get16 si so
  --_see("stepFatChain",cn,"-->",cn', _who)
  if
    | cn' == 0 -> pure Link_UnAllocated -- TODO: maybe this case is always an error?
    | cn' == 0xffff -> pure Link_EndOfChain
    | otherwise -> pure (Link_NextCluster cn')

-- This is the core computation...
siFromClusterNumber :: CN -> M SectorIndex
siFromClusterNumber cn = do
  VolumeInfo{sectorsPerCluster,clusterBegin} <- volume
  pure $ clusterBegin + (fi cn - 2) * fi sectorsPerCluster

setNextCluser :: CN -> W16 -> M ()
setNextCluser cn w16 = do
  --_see ("setNextCluser",cn,"=",w16)
  VolumeInfo{sectorsPerFat} <- volume
  (si,so) <- cn2sio cn
  set16 si so w16
  set16 (si + fi sectorsPerFat) so w16 -- backup FAT

type Addr = (SectorIndex,SectorOffset)

cn2sio :: CN -> M Addr
cn2sio cn = do
  VolumeInfo{fatBegin} <- volume
  let bytePerChainWord :: Int = 2
  let n = bytePerChainWord * fi cn
  let si = fi (n `div` sectorSize) + fatBegin
  let so = fi (n `mod` sectorSize)
  --see ("cn2sio",cn,"->",si,so)
  pure (si,so)

----------------------------------------------------------------------
-- Links: monad aware list

data Links a = Nil | Cons a (M (Links a))

prefixLinks :: [a] -> M (Links a) -> M (Links a)
prefixLinks xs l = foldr (\x xs -> pure (Cons x xs)) l xs

toLinks :: [a] -> Links a
toLinks = \case
  [] -> Nil
  x:xs -> Cons x (pure (toLinks xs))

fromLinks :: Links a -> M [a]
fromLinks = \case
  Nil -> pure []
  Cons x xsm -> do
    xs <- xsm
    list <- fromLinks xs
    pure (x : list)

traverseLinks :: (a -> M b) -> Links a -> M (Links b)
traverseLinks f = \case
  Nil -> pure Nil
  Cons x xsm -> do
    y <- f x
    pure $ Cons y $ do
      xs <- xsm
      ys <- traverseLinks f xs -- moving this here was an improvement
      pure ys
