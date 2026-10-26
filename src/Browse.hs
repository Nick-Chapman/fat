module Browse (dump,interact) where

import Control.Monad.IO.Class (liftIO)
import FileTree (Tree(..),Entry(..),printTree)
import Prelude hiding (interact)
import System.Console.ANSI qualified as AN
import System.Console.Haskeline qualified as HL
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
              Nothing -> do
                liftIO $ putStrLn (col AN.Red "syntax error")
                loop state
              Just com -> do
                state <- execCom vol state com
                loop state

    loop state0

runHL :: HL.InputT IO a -> IO a
runHL = HL.runInputT HL.defaultSettings

col :: AN.Color -> String -> String
col c s =
  AN.setSGRCode [AN.SetColor AN.Foreground AN.Vivid c] <> s <>
  AN.setSGRCode [AN.SetColor AN.Foreground AN.Vivid AN.White]

parseCom :: String -> Maybe Com
parseCom s =
  case words s of
    ["ls"] -> Just Ls
    ["cd",name] -> Just (Cd name)
    ["cat",name] -> Just (Cat name)
    ["echo","''",">",name] -> Just (EchoRedirect "" name)
    ["echo",s,">",name] -> Just (EchoRedirect s name)
    ["touch",name] -> Just (Touch name)
    _ -> Nothing

type Vol = VolumeInfo

data Com = Ls | Cat Name | Cd Name | EchoRedirect String Name | Touch Name

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
  EchoRedirect contents longName -> do
    echoRedirect vol (contents++"\n") longName
    pure state
  Touch longName -> do
    touch vol longName
    pure state

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
    showRec :: DirRecord -> IO ()
    showRec DirRecord{longName,fileSize,isDir} = do
      let i :: Int = fromIntegral fileSize
      let mode = (if isDir then "d" else "-")
      let slashMaybe = (if isDir then "/" else "")
      printf "%s %6i  %s%s\n" mode i longName slashMaybe
  mapM_ (liftIO . showRec) rs

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
errM s = liftIO $ putStrLn ("Error: " ++ s ++ "\n")

catFileAction :: Vol -> File -> M ()
catFileAction vol (File dr) = do
  getFileContents vol dr >>= liftIO . putStr
