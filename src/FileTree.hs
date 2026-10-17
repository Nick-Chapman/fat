
module FileTree
  ( Tree(..), Entry(..), File(..)
  , printTree
  ) where

data Tree = Tree [(String,Entry)] deriving Show
data Entry = EFile File | ETree Tree deriving Show
data File = File Int String deriving Show

printTree :: Tree -> IO ()
printTree root = walkTree 0 root
  where
    tab :: Int -> String -> String
    tab i s = replicate i ' ' ++ s
    walkTree :: Int -> Tree -> IO ()
    walkTree i (Tree es) = do
      putStrLn (tab i ("#" ++ show (length es)))
      mapM_ (walkEntry (i+2)) es
    walkEntry :: Int -> (String,Entry) -> IO ()
    walkEntry i (name,entry) =
      case entry of
        EFile f -> putStrLn (tab i (show (name,f)))
        ETree t -> do
          putStrLn (tab i (show (name,"DIR")))
          walkTree (i+2) t

