module Browse (dump,interact) where

import CachedDisk (M,exec)
import Control.Monad.IO.Class (liftIO)
import DateTime (Date,Time)
import Fat16 (Entry(..),Item(..),File(..),Dir(..),lookupItem,getDirEntries,getFileContents,echoRedirect,appendRedirect,mkDir,removeDirEntry,touchFile)
import FileTree (Tree(..),TreeEntry(..),printTree)
import Prelude hiding (interact)
import System.Console.ANSI qualified as AN
import System.Console.Haskeline qualified as HL
import System.IO (hFlush,hPutStrLn)
import System.IO qualified as IO (stderr)
import Text.Printf (printf)
import Control.Monad (when)

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
  | Ls_la (Maybe Name)
  | Cat Name
  | Cd Name
  | EchoRedirect String Name
  | AppendRedirect String Name
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
    ["ls","-la"] -> Command (Ls_la Nothing)
    ["ls","-la",name] -> Command (Ls_la (Just name))
    ["cd",name] -> Command (Cd name)
    ["cat",name] -> Command (Cat name)
    ["echo","-n","''",">",name] -> Command (Touch name)
    ["echo","-n",s,">",name] -> Command (EchoRedirect s name)
    ["echo","''",">",name] -> Command (EchoRedirect "\n" name)
    ["echo",s,">",name] -> Command (EchoRedirect (s++"\n") name)
    ["echo",s,">>",name] -> Command (AppendRedirect (s++"\n") name)
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
  Ls_la Nothing -> do
    lsDirAction (const True) cwd
    pure state
  Ls_la (Just path) -> do
    resolvePath cwd path >>= \case
      Left e -> errM e
      Right (dir,base) -> do
        lookupItem dir base >>= \case
          Nothing -> do
            errM (printf "ls: cannot access '%s': No such file or directory" path)
          Just (IFile{}) -> do
            lsDirAction (==base) dir
          Just (IDir dir') -> do
            lsDirAction (const True) dir'
    pure state
  Cat path -> do
    resolvePath cwd path >>= \case
      Left e -> errM e
      Right (dir,base) -> do
        lookupItem dir base >>= \case
          Nothing -> do
            errM (printf "cat: %s: No such file or directory" path)
          Just (IDir{}) -> do
            errM (printf "cat: %s: Is a directory" path)
          Just (IFile file) -> do
            getFileContents file >>= liftIO . putStr
    pure state
  Cd path -> do
    resolvePath cwd path >>= \case
      Left e -> do errM e; pure state
      Right (dir,base) -> do
        lookupItem dir base >>= \case
          Nothing -> do
            errM (printf "cd: %s: No such file or directory" path )
            pure state
          Just (IFile{}) -> do
            errM (printf "cd: %s: Not a directory" path)
            pure state
          Just (IDir dir) -> do
            pure state { cwd = dir }
  EchoRedirect contents path -> do
    resolvePath cwd path >>= \case
      Left e -> errM e
      Right (dir,base) -> do
        let makeFile = echoRedirect dt contents base dir
        lookupItem dir base >>= \case
          Just IDir{} -> do
            errM (printf "%s: is a directory" path)
          Just IFile{} -> do
            removeDirEntry True base dir
            makeFile
          Nothing -> makeFile
    pure state
  AppendRedirect contents path -> do
    resolvePath cwd path >>= \case
      Left e -> errM e
      Right (dir,base) -> do
        let appendFile = appendRedirect dt contents base dir
        lookupItem dir base >>= \case
          Just IDir{} -> do
            errM (printf "%s: is a directory" path)
          Just IFile{} -> do
            appendFile
          Nothing ->
            appendFile
    pure state
  Touch path -> do
    resolvePath cwd path >>= \case
      Left e -> errM e
      Right (dir,base) -> do
        lookupItem dir base >>= \case
          Just{} -> do
            pure () -- TODO: touch meta data
          Nothing ->
            touchFile dt base dir
    pure state
  CopyIn xpath path -> do
    contents <- liftIO $ readFile xpath
    resolvePath cwd path >>= \case
      Left e -> errM e
      Right (dir,base) -> do
        echoRedirect dt contents base dir
    pure state
  Mkdir path -> do
    resolvePath cwd path >>= \case
      Left e -> errM e
      Right (dir,base) -> do
        lookupItem dir base >>= \case
          Just{} -> do
            errM (printf "mkdir: cannot create directory '%s': File exists" path)
          Nothing -> do
            mkDir dt base dir
    pure state
  Rm path -> do
    resolvePath cwd path >>= \case
      Left e -> errM e
      Right (dir,base) -> do
        lookupItem dir base >>= \case
          Nothing -> do
            errM (printf "rm: cannot remove '%s': No such file or directory" path)
          Just IDir{} -> do
            errM (printf "rm: cannot remove '%s': Is a directory" path)
          Just IFile{} -> do
            removeDirEntry True base dir
    pure state
  Rmdir path -> do
    resolvePath cwd path >>= \case
      Left e -> errM e
      Right (dir,base) -> do
        lookupItem dir base >>= \case
          Nothing -> do
            errM (printf "rmdir: failed to remove '%s': No such file or directory" path)
          Just IFile{} -> do
            errM (printf "rmdir: failed to remove '%s': Not a directory" path)
          Just (IDir dir') -> do
            n <- length <$> getDirEntries dir'
            if (n == 2) -- "." and ".."
              then removeDirEntry False base dir
              else errM (printf "rmdir: failed to remove '%s': Directory not empty" path)
    pure state

resolvePath :: Dir -> String -> M (Either String (Dir,String))
resolvePath cwd path0 = loop cwd path0
  where
    loop dir path = do
      let (front,back) = break (=='/') path
      case back of
        [] -> pure (Right (dir,front))
        _slash:path' -> do
          lookupItem dir front >>= \case
            Nothing -> pure (Left (printf "%s: No such file or directory" path0))
            Just IFile{} -> pure (Left (printf "%s: Not a directory" path0))
            Just (IDir dir) -> loop dir path'

lsDirAction :: (String -> Bool) -> Dir -> M ()
lsDirAction pred dir = do
  let
    showEntry :: Entry -> IO ()
    showEntry Entry{longName,item} = do
      when (pred longName) $ case item of
        IDir{} -> printf "- %6i  %s/\n" (0::Int) longName -- TODO: dir size
        IFile (File {fileSize}) ->
          printf "- %6i  %s\n" (fromIntegral fileSize :: Int) longName

  es <- getDirEntries dir
  mapM_ (liftIO . showEntry) es

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
