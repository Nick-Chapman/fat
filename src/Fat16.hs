module Fat16 (decode,interact) where

import Control.Exception (assert)
import Control.Monad (ap,liftM)
import Data.Bits ((.&.))
import Data.ByteString qualified as B
import FileTree (Tree(..),Entry(..))
import Prelude hiding (interact)
import Text.Printf (printf)

import Types

interact :: FilePath -> IO ()
interact = exec $ do
  vol <- getVolumeInfo 0
  --PutStr (show vol ++ "\n")
  let
    loop :: State -> [Com] -> M ()
    loop state = \case
      [] -> pure ()
      com:coms -> do
        PutStr (printf "> %s\n" (showCom com))
        state <- execCom vol state com
        loop state coms
  let
    coms =
      [ Ls
      , Cd "quarter-forth"
      , Ls
      , Cat "z.list"
      , Cd ".."
      , Ls
{-      , Cd "wordle"
      , Ls
      , Cd "src"
      , Ls
      , Cat "Memo.hs"-}
      ]
  loop state0 coms

type Vol = VolumeInfo

data Com = Ls | Cat Name | Cd Name

showCom :: Com -> String
showCom = \case
  Ls -> "ls -l"
  Cat name -> "cat " ++ name
  Cd name -> "cd " ++ name

type Name = String

data State = State { cwd :: Dir }

state0 :: State
state0 = State { cwd = Root }

execCom :: Vol -> State -> Com -> M State
execCom vol state@State{cwd} = \case
  Ls -> do
    lsDirAction vol cwd
    pure state
  Cat name -> do
    lookupFile vol cwd name >>= \case
      Nothing -> pure state
      Just file -> do
        catFileAction vol file
        pure state
  Cd name -> do
    lookupSubDir vol cwd name >>= \case
      Nothing -> pure state
      Just dir -> pure state { cwd = dir }

data Dir = Root | Sub CN
data File = File DirRecord
data Item = IDir Dir | IFile File

lookupSubDir :: Vol -> Dir -> Name -> M (Maybe Dir)
lookupSubDir vol dir name =
  lookupItem vol dir name >>= \case
    Nothing -> pure Nothing
    Just (IDir dir) -> pure (Just dir)
    Just (IFile{}) -> do
      errM (printf "not a directory:"  ++ name)
      pure Nothing

lookupFile :: Vol -> Dir -> Name -> M (Maybe File)
lookupFile vol dir name =
  lookupItem vol dir name >>= \case
    Nothing -> pure Nothing
    Just (IFile file) -> pure (Just file)
    Just (IDir{}) -> do
      errM (printf "not a file: %s" name)
      pure Nothing

lsDirAction :: Vol -> Dir -> M ()
lsDirAction vol dir = do
  rs <- case dir of
    Root -> getRootDirInfo vol
    Sub cn -> getDirInfo vol cn
  let
    showRec :: DirRecord -> String
    showRec DirRecord{longName,fileSize,isDir} = do
      let i :: Int = fromIntegral fileSize
      let mode = (if isDir then "d" else "-")
      let slashMaybe = (if isDir then "/" else "")
      printf "%s %6i  %s%s\n" mode i longName slashMaybe
  mapM_ (PutStr . showRec) rs

lookupItem :: Vol -> Dir -> Name -> M (Maybe Item)
lookupItem vol dir sought = do
  rs <- case dir of -- TODO: be incremental?
    Root -> getRootDirInfo vol
    Sub cn -> getDirInfo vol cn
  case [ if isDir
         then IDir (if firstCluster == 0 then Root else Sub firstCluster)
         else IFile (File dr)
       | dr@DirRecord{longName,isDir,firstCluster} <- rs
       , longName == sought
       ] of
    item:_ -> pure (Just item)
    [] -> do
      errM (printf "no such item: " ++ sought )
      pure Nothing

errM :: String -> M ()
errM s = PutStr ("Error: " ++ s ++ "\n")

catFileAction :: Vol -> File -> M ()
catFileAction vol (File dr) = do
  getFileContents vol dr >>= PutStr

----------------------------------------------------------------------

fi :: (Integral a, Num b) => a -> b
fi x = fromIntegral x

bytesPerDirEntry :: Int
bytesPerDirEntry = 32

decode :: FilePath -> IO Tree
decode = exec $ do
  vol <- getVolumeInfo 0
  getTree vol

getTree :: VolumeInfo -> M Tree
getTree vol = do
  rs <- getRootDirInfo vol
  binds <- sequence [ do entry <- deepEntry r; pure (longName,entry)
                    | r@DirRecord{longName} <- rs
                    ]
  pure (Tree binds)
  where
    deepTree :: CN -> M Tree
    deepTree cn = do
      rs <- getDirInfo vol cn
      binds <- sequence [ do entry <- deepEntry r; pure (longName,entry)
                        | r@DirRecord{longName} <- rs
                        , longName /= "."
                        , longName /= ".."
                        ]
      pure (Tree binds)

    deepEntry :: DirRecord -> M Entry
    deepEntry r@DirRecord{isDir,firstCluster=cn} = if
      | isDir -> do
          tree <- deepTree cn
          pure (ETree tree)
      | otherwise -> do
          contents <- getFileContents vol r
          pure (EFile contents)

data VolumeInfo = VolumeInfo
  { sectorsPerCluster :: W8
--  , numberReservedSectors :: W16
--  , numberOfFats :: W8
--  , numberOfRootEntries :: W16
--  , sectorsPerFat :: W16
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
                  --, sectorsPerFat
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
  let VolumeInfo{fatBegin} = vol
  let bytePerChainWord :: Int = 2
  let n = bytePerChainWord * fi cn
  let si = fi (n `div` sectorSize) + fatBegin
  let so = fi (n `mod` sectorSize)
  cn' <- CN <$> get16 si so
  --Print ("stepFatChain",cn,"-->",cn')
  pure cn'

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

instance Functor M where fmap = liftM
instance Applicative M where pure = Pure; (<*>) = ap
instance Monad M where (>>=) = Bind

data M a where
  Pure :: a -> M a
  Bind :: M a -> (a -> M b) -> M b
  ReadSector :: SectorIndex -> M Sector
  Print :: Show a => a -> M ()
  PutStr :: String -> M ()

exec :: M a -> FilePath -> IO a
exec m0 path = do
  theBS <- B.readFile path
  let
    reallyReadSector :: DiskState -> SectorIndex -> IO (Sector,DiskState)
    reallyReadSector cache@DiskState{readCount} si = do
      --print ("ReadSector",n)
      let sec = mkSector theBS si
      pure (sec, cache { lastSI = si, lastSec = sec, readCount = 1 + readCount })

    go :: DiskState -> M a -> IO (a,DiskState)
    go c = \case
      Pure x -> pure (x,c)
      Bind m f -> do (a,c) <- go c m; go c (f a)
      Print x -> do print x; pure ((),c)
      PutStr x -> do putStr x; pure ((),c)
      ReadSector si -> do
        let DiskState {lastSI,lastSec,cacheHitCount} = c
        if si == lastSI
          then pure (lastSec,c { cacheHitCount = 1 + cacheHitCount })
          else reallyReadSector c si

  let si = 0 -- volume index
  let cu = state0
  (_sec,c0) <- reallyReadSector cu si
  (a,cFinal) <- go c0 m0
  let DiskState{readCount,cacheHitCount} = cFinal
  printf "[#reads=%d; #hits=%d]\n" readCount cacheHitCount
  pure a
    where
      state0 :: DiskState
      state0 = DiskState
        { readCount = 0
        , cacheHitCount = 0
        , lastSI = undefined
        , lastSec = undefined
        }


data DiskState = DiskState
  { readCount :: Int
  , cacheHitCount :: Int
  , lastSI:: SectorIndex
  , lastSec :: Sector
  }
