module Top (main) where

import Fat32 qualified (decode)
import FileTree (printTree)
import System.Environment (getArgs)
import Text.Printf (printf)

main :: IO ()
main = do
  args <- getArgs
  let Config{imageFile} = parseConfig args
  tree <- Fat32.decode imageFile
  printTree tree

data Config = Config
  { imageFile :: FilePath
  }

parseConfig :: [String] -> Config
parseConfig = loop config0
  where
    config0 = Config
      { imageFile = "[some.image]"
      }
    loop :: Config -> [String] -> Config
    loop acc = \case
      [] -> acc
      flag@('-':_):_ -> error (printf "unknown flag: %s" flag)
      imageFile:rest -> loop acc { imageFile } rest
