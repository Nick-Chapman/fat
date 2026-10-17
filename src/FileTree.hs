
module FileTree
  ( Tree(..), Entry(..)
  , printTree
  ) where

import Text.Printf (printf)

data Tree = Tree [(String,Entry)] deriving Show
data Entry = EFile String | ETree Tree deriving Show

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
        EFile contents ->
          putStrLn (printf "%s : %s" fullname (show contents))
        ETree t -> do
          walkTree fullname t
