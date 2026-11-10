module Browse (dump,interact) where

import CachedDisk (M,exec)
import Control.Monad.IO.Class (liftIO)
import DateTime (Date,Time)
import Fat16 (Entry(..),Item(..),File(..),Dir(..),getDirEntries,getFileContents,echoRedirect,mkDir,touch,removeDirEntry)
import FileTree (Tree(..),TreeEntry(..),printTree)
import Prelude hiding (interact)
import System.Console.ANSI qualified as AN
import System.Console.Haskeline qualified as HL
import System.IO (hFlush,hPutStrLn)
import System.IO qualified as IO (stderr)
import Text.Printf (printf)

dump :: FilePath -> IO ()
dump path = exec path $ do
  tree <- getTree
  liftIO $ printTree tree

getTree :: M Tree
getTree = deepTree Root
  where
    deepTree :: Dir -> M Tree
    deepTree dir = do
      es <- getDirEntries dir
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
        contents <- getFileContents file
        pure (EFile contents)

interact :: Bool -> (Date,Time) -> FilePath -> IO ()
interact showPrompt dt path = do
  let prompt = if showPrompt then (col AN.Green $ "> ") else ""
  exec path $ do
    let
      loop :: State -> M ()
      loop state = do
        (liftIO $ runHL $ HL.getInputLine prompt) >>= \case
          Nothing -> pure ()
          Just line -> do
            case parseCom line of
              FailedToParse -> do
                errM (printf "syntax error: '%s'" line)
                loop state
              ExitCommand -> do
                pure ()
              Command com -> do
                state <- execCom dt state com
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
  | Rmdir Name

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
    ["echo","-n","''",">",name] -> Command (Touch name)
    ["echo","-n",s,">",name] -> Command (EchoRedirect s name)
    ["echo","''",">",name] -> Command (EchoRedirect "\n" name)
    ["echo",s,">",name] -> Command (EchoRedirect (s++"\n") name)
    ["touch",name] -> Command (Touch name)
    ["cp",path@('/':_),name] -> Command (CopyIn path name)
    ["mkdir",name] -> Command (Mkdir name)
    ["rm",name] -> Command (Rm name)
    ["rmdir",name] -> Command (Rmdir name)
    _ -> FailedToParse

type Name = String

data State = State { cwd :: Dir }

state0 :: State
state0 = State { cwd = Root }

execCom :: (Date,Time) -> State -> Com -> M State
execCom dt state@State{cwd} = \case
  Null -> do
    pure state
  Ls -> do
    lsDirAction cwd
    pure state
  Cat name -> do
    lookupItem cwd name >>= \case
      Nothing -> do
        errM (printf "cat: %s: No such file or directory" name)
      Just (IDir{}) -> do
        errM (printf "cat: %s: Is a directory" name)
      Just (IFile file) -> do
        getFileContents file >>= liftIO . putStr
    pure state
  Cd name -> do
    lookupItem cwd name >>= \case
      Nothing -> do
        errM (printf "cd: %s: No such file or directory" name )
        pure state
      Just (IFile{}) -> do
        errM (printf "cd: %s: Not a directory" name)
        pure state
      Just (IDir dir) -> do
        pure state { cwd = dir }
  EchoRedirect contents name -> do
    let makeFile = echoRedirect dt contents name cwd
    lookupItem cwd name >>= \case
      Just IDir{} -> do
        errM (printf "%s: is a directory" name)
      Just IFile{} -> do
        removeDirEntry True name cwd
        makeFile
      Nothing -> makeFile
    pure state
  Touch name -> do
    touch dt name cwd
    pure state
  CopyIn path name -> do
    contents <- liftIO $ readFile path
    echoRedirect dt contents name cwd
    pure state
  Mkdir name -> do
    lookupItem cwd name >>= \case
      Just{} -> do
        errM (printf "mkdir: cannot create directory '%s': File exists" name)
      Nothing -> do
        mkDir dt name cwd
    pure state
  Rm name -> do
    lookupItem cwd name >>= \case
      Nothing -> do
        errM (printf "rm: cannot remove '%s': No such file or directory" name)
      Just IDir{} -> do
        errM (printf "rm: cannot remove '%s': Is a directory" name)
      Just IFile{} -> do
        removeDirEntry True name cwd
    pure state
  Rmdir name -> do
    lookupItem cwd name >>= \case
      Nothing -> do
        errM (printf "rmdir: failed to remove '%s': No such file or directory" name)
      Just IFile{} -> do
        errM (printf "rmdir: failed to remove '%s': Not a directory" name)
      Just IDir{} -> do
        removeDirEntry False name cwd
    pure state

lsDirAction :: Dir -> M ()
lsDirAction dir = do
  let
    showEntry :: Entry -> IO ()
    showEntry Entry{longName,item} = case item of
      IDir{} -> printf "- %6i  %s/\n" (0::Int) longName -- TODO: dir size
      IFile (File {fileSize}) ->
        printf "- %6i  %s\n" (fromIntegral fileSize :: Int) longName

  es <- getDirEntries dir
  mapM_ (liftIO . showEntry) es

lookupItem :: Dir -> Name -> M (Maybe Item)
lookupItem dir sought = do
  es <- getDirEntries dir -- TODO: be incremental?
  case [ item
       | Entry{longName,item} <- es
       , longName == sought
       ] of
    item:_ -> pure (Just item)
    [] -> do
      pure Nothing

errM :: String -> M ()
errM s = liftIO $ putErr (col AN.Red s ++ "\n")

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
