module Top (main) where

import Fat16 qualified (decode)
import Fat32 qualified (decode)
import FileTree (printTree)
import System.Environment (getArgs)
import Text.Printf (printf)

main :: IO ()
main = do
  args <- getArgs
  let Config{format,imageFile} = parseConfig args
  tree <- case format of
    NoFormat -> error "NoFormat"
    F16 -> Fat16.decode imageFile
    F32 -> Fat32.decode imageFile
  printTree tree

data Config = Config
  { imageFile :: FilePath
  , format :: Format
  }

data Format = NoFormat | F16 | F32

parseConfig :: [String] -> Config
parseConfig = loop config0
  where
    config0 = Config
      { imageFile = "[some.image]"
      , format = NoFormat
      }
    loop :: Config -> [String] -> Config
    loop acc = \case
      [] -> acc
      "-16":rest -> loop acc { format = F16 } rest
      "-32":rest -> loop acc { format = F32 } rest
      flag@('-':_):_ -> error (printf "unknown flag: %s" flag)
      imageFile:rest -> loop acc { imageFile } rest
