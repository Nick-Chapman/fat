
module FileTree
  ( Tree(..), TreeEntry(..)
  , printTree
  ) where

import Text.Printf (printf)

data Tree = Tree [(String,TreeEntry)] deriving Show
data TreeEntry = EFile String | ETree Tree deriving Show

printTree :: Tree -> IO ()
printTree root = walkTree "" root
  where
    walkTree :: String-> Tree -> IO ()
    walkTree prefix (Tree es) = mapM_ (walkEntry prefix) es

    walkEntry :: String -> (String,TreeEntry) -> IO ()
    walkEntry prefix (basename,entry) = do
      let fullname = prefix ++ "/" ++ basename
      case entry of
        EFile contents ->
          putStrLn (printf "%s : %s" fullname (show contents))
        ETree t -> do
          walkTree fullname t
