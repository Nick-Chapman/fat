module Top (main) where

import Browse qualified (dump,interact,devMut)
import System.Environment (getArgs)
import Text.Printf (printf)

main :: IO ()
main = do
  args <- getArgs
  let Config{imageFile,mode} = parseConfig args
  case mode of
    Dump -> Browse.dump imageFile
    Interact -> Browse.interact imageFile
    DevMut contents longName -> Browse.devMut imageFile contents longName

data Config = Config
  { imageFile :: FilePath
  , mode :: Mode
  }

data Mode = Dump | Interact | DevMut String String

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
      "-i":rest -> loop acc { mode = Interact } rest
      "-m":contents:longName:rest -> loop acc { mode = DevMut contents longName } rest
      flag@('-':_):_ -> error (printf "unknown flag: %s" flag)
      imageFile:rest -> loop acc { imageFile } rest
