
module Fat16
  ( Entry (..)
  , Item(..)
  , File(..)
  , Dir(..)
  , lookupItem
  , getDirEntries
  , getFileContents
  , echoRedirect
  , appendRedirect
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

lookupItem :: Dir -> String -> M (Maybe Item)
lookupItem dir sought = dirEntries dir >>= loop
  where
    loop :: Links Entry -> M (Maybe Item)
    loop = \case
      Nil{} -> pure Nothing
      Cons e esm -> do
        let Entry{longName,item} = e
        if longName == sought then pure (Just item) else
          esm >>= loop

getDirEntries :: Dir -> M [Entry]
getDirEntries dir = do
  dirEntries dir >>= fromLinks

dirEntries :: Dir -> M (Links Entry)
dirEntries dir = dirGroups False dir >>= loop
  where
    loop :: Links Group -> M (Links Entry)
    loop = \case
      Nil -> pure Nil
      Cons x xsm -> do
        entryOfGroup x >>= \case
          Nothing -> xsm >>= loop
          Just y -> do
            pure $ Cons y $ do
              xsm >>= loop

entryOfGroup :: Group -> M (Maybe Entry)
entryOfGroup = \case
  Group_Open{} -> pure Nothing
  Group_OpenEnd{} -> pure Nothing
  Group_Active (ActiveGroup longs short) -> do
    item <- itemAtShortSlot short
    shortName <- shortNameAtShortSlot short
    longName <- if
      | shortName == ".          " -> pure "."
      | shortName == "..         " -> pure ".."
      | otherwise -> longNameOfLongSlots longs
    pure $ Just $ Entry { longName, item }

touch :: (Date,Time) -> String -> Dir -> M ()
touch dt longName dir = do
  let entry = makeStandardEntry dt 0 (makeShort longName) (FileOfSize 0)
  writeDirEntry longName entry dir

echoRedirect :: (Date,Time) -> String -> String -> Dir -> M ()
echoRedirect dt contents longName dir = do
  let fileSize = fromIntegral (length contents)
  let entry = makeStandardEntry dt 0 (makeShort longName) (FileOfSize fileSize)
  -- to get byte match: allocteCluster; writeDirEntry
  writeDirEntry longName entry dir
  cn <- allocteCluster
  setStartingCluster longName cn dir
  writeBytesInClusterChain cn (bytesString contents)

appendRedirect :: (Date,Time) -> String -> String -> Dir -> M ()
appendRedirect dt extraContents longName dir = do
  findActiveGroup longName dir >>= \case
    Nothing -> echoRedirect dt extraContents longName dir
    Just (ActiveGroup _longs short) -> do
      let addr = shortAddr short
      cn <- startingClusterFromAddr addr
      fileSize <- fileSizeAtAddr addr
      setFileSizeAtAddr addr (fileSize + fromIntegral (length extraContents))
      -- TODO: avoid read and re-write of existing contents
      oldContentts <- getFileContents File { fileSize, firstCluster = cn }
      let contents = oldContentts ++ extraContents
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
setStartingCluster sought cn dir =
  findActiveGroup sought dir >>= \case
    Nothing -> undefined "impossible, I just added it"
    Just (ActiveGroup _ short) -> do
      let (si,so) = shortAddr short
      set16 si (so + 0x1A) (fromIntegral cn) -- Ox1A is offset of start-cluster in normal entry

----------------------------------------------------------------------
-- RM

removeDirEntry :: Bool -> String -> Dir -> M ()
removeDirEntry zeroingContents longName dir = do
  findActiveGroup longName dir >>= \case
    Nothing -> pure () -- nothing to remove
    Just (ActiveGroup longs short) -> do
      mapM_ markEntryAddrUnused (map longAddr longs)
      let addr = shortAddr short
      markEntryAddrUnused addr
      cn <- startingClusterFromAddr addr
      when zeroingContents $ do
        zeroOutClusterAndSizeInfo addr
        zeroBytesInClusterChain cn
      clearFatChain cn

startingClusterFromAddr :: Addr -> M CN
startingClusterFromAddr (si,so) = fromIntegral <$> get16 si (so + 0x1A)

markEntryAddrUnused :: Addr -> M ()
markEntryAddrUnused (si,so) = set8 si so 0xe5

zeroOutClusterAndSizeInfo :: Addr -> M ()
zeroOutClusterAndSizeInfo (si,so) = do
  set16 si (so + 0x1A) 0 -- zero the startingCluster
  set32 si (so + 0x1C) 0 -- zero the file size

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
        setNextCluser cn (fromIntegral cn')
        loop cn' rest

writeSectorBytes :: SectorIndex -> SectorOffset -> [W8] -> M ()
writeSectorBytes si so0 bytes = do
  assert (fromIntegral so0 + length bytes <= sectorSize) $ do
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
  let remainingSpaceInSector = sectorSize - fromIntegral so
  if (length bytes <= remainingSpaceInSector) then writeSectorBytes si so bytes else do
    let (front,rest) = splitAt remainingSpaceInSector bytes
    -- Can a long name entry cause a further sector boundary crossing?
    assert (length rest <= sectorSize) $ do -- assert not
    writeSectorBytes si so front
    writeSectorBytes (si+1) 0 rest

writeSubDirEntryBytesAddr :: [W8] -> CN -> Addr -> M ()
writeSubDirEntryBytesAddr bytes cn (si,so) = do
  let remainingSpaceInSector = sectorSize - fromIntegral so
  if (length bytes <= remainingSpaceInSector) then writeSectorBytes si so bytes else do
    let (front,rest) = splitAt remainingSpaceInSector bytes
    -- Can a long name entry cause a further sector boundary crossing?
    assert (length rest <= sectorSize) $ do -- assert not
    writeSectorBytes si so front
    cn' <- nextCluster cn
    si' <- siFromClusterNumber cn'
    writeSectorBytes si' 0 rest

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
  let headerBytes = ([fromIntegral (0x40 + n)] ++ reverse [ 1 .. fromIntegral (n - 1) ])
  let rotAdd acc c = acc `rotateR` 1 + fromIntegral (c2w c)
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
    ++ bytes16 (fromIntegral startingCluster)
    ++ bytes32 fileSize

trunOrPadSpace :: Int -> String -> String
trunOrPadSpace n s = take n (s ++ repeat ' ')

getFileContents :: File -> M String
getFileContents dr = do
  VolumeInfo{sectorsPerCluster} <- volume
  let File{fileSize,firstCluster} = dr
  let clusterSize = sectorSize * fromIntegral sectorsPerCluster
  let
    loop :: CN -> Int -> M String
    loop cn size = if
      | size <= clusterSize -> do
          si <- siFromClusterNumber cn
          map toChar <$> getBytes si 0 (fromIntegral size)
      | otherwise -> do
          si <- siFromClusterNumber cn
          start <- map toChar <$> getBytes si 0 clusterSize
          stepFatChainL cn >>= \case
            Link_UnAllocated -> error "getFileContents/UnAllocated"
            Link_EndOfChain -> error "getFileContents/EndOfChain"
            Link_NextCluster cn' -> do
              rest <- loop cn' (size - clusterSize)
              pure (start ++ rest)
  loop firstCluster (fromIntegral fileSize)

----------------------------------------------------------------------

findDirNextEntry :: Int -> Dir -> M (Dir,Addr)
findDirNextEntry need dir0 = dirGroups True dir0 >>= loop
  where
    loop :: Links Group -> M (Dir,Addr)
    loop = \case
      Nil{} -> error "findDirNextEntry/Nil" -- expect Group_OpenEnd
      Cons g gsm -> do
        case g of
          Group_Active{} -> gsm >>= loop
          Group_Open{size,dir,addr} -> do
            if size < need then gsm >>= loop else do
              pure (dir,addr)
          Group_OpenEnd dir addr -> do
            pure (dir,addr)

----------------------------------------------------------------------
-- new directory stuff

findActiveGroup :: String -> Dir -> M (Maybe ActiveGroup)
findActiveGroup sought dir = do
  dirGroups False dir >>= loop
  where
    loop :: Links Group -> M (Maybe ActiveGroup)
    loop = \case
      Nil -> error "findActiveGroup/[]"
      Cons g gsm -> case g of
        Group_OpenEnd{} -> pure Nothing
        Group_Open{} -> gsm >>= loop
        Group_Active (ag@(ActiveGroup longs _)) -> do
          longName <- longNameOfLongSlots longs
          if longName == sought then pure (Just ag) else gsm >>= loop


data Group
  = Group_Active ActiveGroup
  | Group_Open { size :: Int, dir :: Dir, addr :: Addr }
  | Group_OpenEnd Dir Addr

data ActiveGroup = ActiveGroup [LongSlot] ShortSlot

longNameOfLongSlots :: [LongSlot] -> M String -- TODO: no need to be in M
longNameOfLongSlots longs =
  pure $ concat [ nameChunk | LongSlot{nameChunk} <- reverse longs ]

dirGroups :: Bool -> Dir -> M (Links Group)
dirGroups openNew dir = do
  slots <- dirSlots openNew dir
  groupSlots slots

dirSlots :: Bool -> Dir -> M (Links EntrySlot)
dirSlots openNew dir0 = do dirSectors dir0 >>= loopSis dir0
  where
    loopSis :: Dir -> Links (Dir,SectorIndex) -> M (Links EntrySlot)
    loopSis dir = \case
      Nil -> if
        | openNew -> do
            case dir of
              Root -> do
                pure Nil
              Sub cn -> do
                cn' <- nextCluster cn
                si' <- siFromClusterNumber cn'
                let a'= (si',0)
                let g = ES_OpenEnd (Sub cn') a'
                pure (Cons g (pure Nil))
        | otherwise ->
          pure Nil
      Cons (dir,si) xsm -> loopAddrs (entryAddressesAtSector si)
        where
          loopAddrs :: [Addr] -> M (Links EntrySlot)
          loopAddrs = \case
            [] -> xsm >>= loopSis dir
            addr:addrs -> do
              slot <- decodeEntrySlot dir addr
              pure $ Cons slot $ do
                loopAddrs addrs

groupSlots :: Links EntrySlot -> M (Links Group)
groupSlots = loop Nothing []
  where
    loop :: Maybe (Int,Dir,Addr) -> [LongSlot] -> Links EntrySlot -> M (Links Group)
    loop gap longs = \case
      Nil -> pure Nil
      Cons slot slotsm -> do
        case slot of
          ES_Long long -> maybeConsAvailable <$> do
            slots <- slotsm
            loop Nothing (long:longs) slots
          ES_Short short -> maybeConsAvailable <$> do
            let group = Group_Active $ ActiveGroup (reverse longs) short
            pure $ Cons group $ do
              slots <- slotsm -- Moving this here is the fix!
              loop Nothing [] slots
          ES_Open dir addr -> do
            assert (case longs of [] -> True; _ -> False) $ do
            slots <- slotsm
            let an = case gap of Nothing -> (1,dir,addr); Just (n,d,a) -> (n+1,d,a)
            loop (Just an) [] slots
          ES_OpenEnd dir addr -> do
            assert (case longs of [] -> True; _ -> False) $ do
            let g = case gap of
                  Just (_,d,a) -> Group_OpenEnd d a
                  Nothing -> Group_OpenEnd dir addr
            pure (Cons g (pure Nil))
          where
            maybeConsAvailable :: Links Group -> Links Group
            maybeConsAvailable links =
              case gap of
                Nothing -> links
                Just (size,dir,addr) -> do
                  let g = Group_Open { size, dir, addr }
                  Cons g (pure links)

dirSectors :: Dir -> M (Links (Dir,SectorIndex))
dirSectors = \case
  Root -> rootSectors
  Sub cn -> expandClusterChainFrom cn

rootSectors :: M (Links (Dir,SectorIndex))
rootSectors = do
  VolumeInfo{rootDirBegin,clusterBegin} <- volume
  let n = fromIntegral (clusterBegin - rootDirBegin)
  pure $ toLinks [ (Root,si) | si <- take n [rootDirBegin .. ] ]

expandClusterChainFrom :: CN -> M (Links (Dir,SectorIndex))
expandClusterChainFrom = loop
  where
    loop :: CN -> M (Links (Dir,SectorIndex))
    loop cn = do
      si <- siFromClusterNumber cn
      pure $ Cons (Sub cn, si) $ do
        stepFatChainL cn >>= \case
          Link_UnAllocated -> error "expandClusterChainFrom/UnAllocated"
          Link_EndOfChain -> pure Nil
          Link_NextCluster cn' -> loop cn'

entryAddressesAtSector :: SectorIndex -> [Addr] -- # (512/32 = 16)
entryAddressesAtSector si = do
  let numEntrySlotsInSector = sectorSize `div` bytesPerDirEntry
  [ (si,32 * so) | so <- take numEntrySlotsInSector [0..] ]

decodeEntrySlot :: Dir -> Addr -> M EntrySlot
decodeEntrySlot dir addr@(si,so) = do
  firstByte <- get8 si so
  attr <- get8 si (so+0xB)
  if
    | firstByte == 0x00 -> pure (ES_OpenEnd dir addr)
    | firstByte == 0xE5 -> pure (ES_Open dir addr)
    | (attr .&. 0xF) == 0xF -> do
        nameChunk <- getLongNameChunk addr -- TODO: Maybe delay this
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
  | ES_Open Dir Addr
  | ES_OpenEnd Dir Addr
  deriving Show

data LongSlot = LongSlot
  { addr :: Addr
  , nameChunk :: String
  }
  deriving Show
data ShortSlot = ShortSlot
  { addr :: Addr
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
itemAtAddr addr@(si,so) = do
  attr <- get8 si (so+0xB)
  firstCluster <- CN <$> get16 si (so + 0x1A)
  let isDir = (attr .&. 0x10) /= 0
  if | isDir -> do
         pure $ IDir (if firstCluster == 0 then Root
                      else Sub { firstCluster })
     | otherwise -> do
         fileSize <- fileSizeAtAddr addr
         pure $ IFile (File { fileSize, firstCluster })

fileSizeAtAddr :: Addr -> M W32
fileSizeAtAddr (si,so) = get32 si (so + 0x1C)

setFileSizeAtAddr :: Addr -> W32 -> M ()
setFileSizeAtAddr (si,so) w = set32 si (so + 0x1C) w

shortNameAtShortSlot :: ShortSlot -> M String
shortNameAtShortSlot ShortSlot{addr=(si,so)} = map toChar <$> getBytes si so 11
--shortNameAtShortSlot ShortSlot{name} = pure name

----------------------------------------------------------------------
-- FAT

nextCluster :: CN -> M CN -- allocating if necessary
nextCluster cn = do
  stepFatChainL cn >>= \case
    Link_UnAllocated -> undefined
    Link_NextCluster cn' -> pure cn'
    Link_EndOfChain -> do
      cn' <- allocteCluster
      setNextCluser cn (fromIntegral cn')
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
      stepFatChainL cn >>= \case
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
      set16 (si + fromIntegral sectorsPerFat) so 0 -- backup FAT
      if cn' == 0xffff then pure () else loop cn'

zeroBytesInClusterChain :: CN -> M ()
zeroBytesInClusterChain cn = loop cn
  where
    loop :: CN -> M ()
    loop cn = do
      si <- siFromClusterNumber cn
      let bytes = replicate 512 0x0
      writeSectorBytes si 0 bytes
      stepFatChainL cn >>= \case
        Link_UnAllocated -> error "zeroBytesInClusterChain/UnAllocated"
        Link_EndOfChain -> pure ()
        Link_NextCluster cn' -> loop cn'

data Link = Link_UnAllocated | Link_NextCluster CN | Link_EndOfChain

stepFatChainL :: CN -> M Link
stepFatChainL cn = do
  assert (cn /= 0) $ do
  assert (cn /= 0xffff) $ do
  (si,so) <- cn2sio cn
  cn' <- CN <$> get16 si so
  if
    | cn' == 0 -> pure Link_UnAllocated -- TODO: maybe this case is always an error?
    | cn' == 0xffff -> pure Link_EndOfChain
    | otherwise -> pure (Link_NextCluster cn')

-- This is the core computation...
siFromClusterNumber :: CN -> M SectorIndex
siFromClusterNumber cn = do
  VolumeInfo{sectorsPerCluster,clusterBegin} <- volume
  pure $ clusterBegin + (fromIntegral cn - 2) * fromIntegral sectorsPerCluster

setNextCluser :: CN -> W16 -> M ()
setNextCluser cn w16 = do
  --_see ("setNextCluser",cn,"=",w16)
  VolumeInfo{sectorsPerFat} <- volume
  (si,so) <- cn2sio cn
  set16 si so w16
  set16 (si + fromIntegral sectorsPerFat) so w16 -- backup FAT

type Addr = (SectorIndex,SectorOffset)

cn2sio :: CN -> M Addr
cn2sio cn = do
  VolumeInfo{fatBegin} <- volume
  let bytePerChainWord :: Int = 2
  let n = bytePerChainWord * fromIntegral cn
  let si = fromIntegral (n `div` sectorSize) + fatBegin
  let so = fromIntegral (n `mod` sectorSize)
  --see ("cn2sio",cn,"->",si,so)
  pure (si,so)

----------------------------------------------------------------------
-- Links: monad aware list

data Links a = Nil | Cons a (M (Links a)) -- TODO: rename List

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
