module Browse (dump,interact) where

import Control.Monad.IO.Class (liftIO)
import Fat16 (Entry(..),Item(..),File(..),Dir(..),getDirEntries,getFileContents,echoRedirect,mkDir,touch,rmCommand)
import FileTree (Tree(..),TreeEntry(..),printTree)
import Prelude hiding (interact)
import System.Console.ANSI qualified as AN
import System.Console.Haskeline qualified as HL
import System.IO (hFlush,hPutStrLn)
import System.IO qualified as IO (stderr)
import Text.Printf (printf)
import TheDisk (M,exec)
import Volume (Vol,getVolumeInfo)

dump :: FilePath -> IO ()
dump path = exec path $ do
  vol <- getVolumeInfo 0
  tree <- getTree vol
  liftIO $ printTree tree

getTree :: Vol -> M Tree
getTree vol = deepTree Root
  where
    deepTree :: Dir -> M Tree
    deepTree dir = do
      es <- getDirEntries vol dir
      binds <- sequence [ do treeEntry <- deepEntry item
                             pure (longName,treeEntry)
                        | Entry{longName,item} <- es
                        , longName /= "."
                        , longName /= ".."
                        ]
      pure (Tree binds)

    deepEntry :: Item -> M TreeEntry
    deepEntry = \case
      IDir dir -> do
        tree <- deepTree dir
        pure (ETree tree)
      IFile file -> do
        contents <- getFileContents vol file
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
  | Rm Name

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
    ["rm",name] -> Command (Rm name)
    _ -> FailedToParse

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
  Rm name -> do
    rmCommand vol name cwd
    pure state

catFileAction :: Vol -> File -> M ()
catFileAction vol file = do
  getFileContents vol file >>= liftIO . putStr

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
  let
    showEntry :: Entry -> IO ()
    showEntry Entry{longName,item} = case item of
      IDir{} -> printf "- %6i  %s/\n" (0::Int) longName -- TODO: dir size
      IFile (File {fileSize}) ->
        printf "- %6i  %s\n" (fromIntegral fileSize :: Int) longName

  es <- getDirEntries vol dir
  mapM_ (liftIO . showEntry) es

lookupItem :: Vol -> Dir -> Name -> M (Maybe Item)
lookupItem vol dir sought = do
  es <- getDirEntries vol dir -- TODO: be incremental?
  case [ item
       | Entry{longName,item} <- es
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
