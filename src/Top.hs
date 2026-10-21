module Top (main) where

import Fat16 qualified (decode,interact)
import FileTree (printTree)
import System.Environment (getArgs)
import Text.Printf (printf)

main :: IO ()
main = do
  args <- getArgs
  let Config{imageFile,mode} = parseConfig args
  case mode of
    Dump -> do
      tree <- Fat16.decode imageFile
      printTree tree
    Interact16 -> do
      Fat16.interact imageFile

data Config = Config
  { imageFile :: FilePath
  , mode :: Mode
  }

data Mode = Dump | Interact16

parseConfig :: [String] -> Config
parseConfig = loop config0
  where
    config0 = Config
      { imageFile = "[some.image]"
      , mode = Dump
      }
    loop :: Config -> [String] -> Config
    loop acc = \case
      [] -> acc
      "-i":rest -> loop acc { mode = Interact16 } rest
      flag@('-':_):_ -> error (printf "unknown flag: %s" flag)
      imageFile:rest -> loop acc { imageFile } rest
