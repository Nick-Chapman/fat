module Browse (dump,interact) where

import Control.Monad.IO.Class (liftIO)
import FileTree (Tree(..),TreeEntry(..),printTree)
import Prelude hiding (interact)
import System.Console.ANSI qualified as AN
import System.Console.Haskeline qualified as HL
import System.IO qualified as IO (stderr)
import System.IO (hFlush,hPutStrLn)
import Text.Printf (printf)
import TheDisk (M,exec)

import Fat16

dump :: FilePath -> IO ()
dump path = exec path $ do
  vol <- getVolumeInfo 0
  tree <- getTree vol
  liftIO $ printTree tree

getTree :: VolumeInfo -> M Tree
getTree vol = do
  rs <- getDirInfo vol Root
  binds <- sequence [ do entry <- deepEntry r; pure (longName,entry)
                    | r@DirRecord{longName} <- rs
                    ]
  pure (Tree binds)
  where
    deepTree :: CN -> M Tree
    deepTree cn = do
      rs <- getDirInfo vol (Sub cn)
      binds <- sequence [ do entry <- deepEntry r; pure (longName,entry)
                        | r@DirRecord{longName} <- rs
                        , longName /= "."
                        , longName /= ".."
                        ]
      pure (Tree binds)

    deepEntry :: DirRecord -> M TreeEntry
    deepEntry r@DirRecord{isDir,firstCluster=cn} = if
      | isDir -> do
          tree <- deepTree cn
          pure (ETree tree)
      | otherwise -> do
          contents <- getFileContents vol r
          pure (EFile contents)

interact :: FilePath -> IO ()
interact path = do
  exec path $ do
    vol <- getVolumeInfo 0
    let
      loop :: State -> M ()
      loop state = do
        (liftIO $ runHL $ HL.getInputLine (col AN.Green $ "> ")) >>= \case
          Nothing -> pure ()
          Just line -> do
            case parseCom line of
              FailedToParse -> do
                errM (printf "syntax error: '%s'" line)
                loop state
              ExitCommand -> do
                pure ()
              Command com -> do
                state <- execCom vol state com
                loop state

    loop state0

data Com
  = Null
  | Ls
  | Cat Name
  | Cd Name
  | EchoRedirect String Name
  | Touch Name
  | CopyIn FilePath Name
  | Mkdir Name

data ParseRes = Command Com | FailedToParse | ExitCommand

parseCom :: String -> ParseRes
parseCom s =
  case words s of
    [] -> Command Null
    ('#':_):_ -> Command Null
    ["exit"] -> ExitCommand
    ["ls"] -> Command Ls
    ["cd",name] -> Command (Cd name)
    ["cat",name] -> Command (Cat name)
    ["echo","''",">",name] -> Command (EchoRedirect "" name)
    ["echo",s,">",name] -> Command (EchoRedirect s name)
    ["touch",name] -> Command (Touch name)
    ["cp",path@('/':_),name] -> Command (CopyIn path name)
    ["mkdir",name] -> Command (Mkdir name)
    _ -> FailedToParse

type Vol = VolumeInfo

type Name = String

data State = State { cwd :: Dir }

state0 :: State
state0 = State { cwd = Root }

execCom :: Vol -> State -> Com -> M State
execCom vol state@State{cwd} = \case
  Null -> do
    pure state
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
  EchoRedirect contents name -> do
    echoRedirect vol (contents++"\n") name cwd
    pure state
  Touch name -> do
    touch vol name cwd
    pure state
  CopyIn path name -> do
    contents <- liftIO $ readFile path
    echoRedirect vol contents name cwd
    pure state
  Mkdir name -> do
    mkDir vol name cwd
    pure state

data File = File DirRecord
data Item = IDir Dir | IFile File

catFileAction :: Vol -> File -> M ()
catFileAction vol (File dr) = do
  getFileContents vol dr >>= liftIO . putStr

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
  rs <- getDirInfo vol dir
  let
    showRec :: DirRecord -> IO ()
    showRec DirRecord{longName,fileSize,isDir} = do
      let i :: Int = fromIntegral fileSize
      let mode = (if isDir then "d" else "-")
      let slashMaybe = (if isDir then "/" else "")
      printf "%s %6i  %s%s\n" mode i longName slashMaybe
  mapM_ (liftIO . showRec) rs

lookupItem :: Vol -> Dir -> Name -> M (Maybe Item)
lookupItem vol dir sought = do
  rs <- getDirInfo vol dir -- TODO: be incremental?
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
errM s = liftIO $ putErr (col AN.Red "Error: " ++ s ++ "\n")

runHL :: HL.InputT IO a -> IO a
runHL = HL.runInputT HL.defaultSettings

col :: AN.Color -> String -> String
col c s =
  AN.setSGRCode [AN.SetColor AN.Foreground AN.Vivid c] <> s <>
  AN.setSGRCode [AN.SetColor AN.Foreground AN.Vivid AN.White]

putErr :: String -> IO ()
putErr s = do
  hPutStrLn IO.stderr s
  hFlush IO.stderr
  pure ()
