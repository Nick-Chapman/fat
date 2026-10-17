
module FileTree
  ( Tree(..), Entry(..), File(..)
  , printTree
  ) where

import Text.Printf (printf)

data Tree = Tree [(String,Entry)] deriving Show
data Entry = EFile File | ETree Tree deriving Show
data File = File Int String deriving Show -- TODO: kill Int; kill type

printTree :: Tree -> IO ()
printTree root = walkTree "" root
  where
    trim = reverse . dropWhile (==' ') . reverse

    walkTree :: String-> Tree -> IO ()
    walkTree prefix (Tree es) = mapM_ (walkEntry prefix) es

    walkEntry :: String -> (String,Entry) -> IO ()
    walkEntry prefix (basename,entry) = do
      let fullname = prefix ++ "/" ++ trim basename
      case entry of
        EFile (File _ contents) ->
          putStrLn (printf "%s : %s" fullname (show contents))
        ETree t -> do
          walkTree fullname t
