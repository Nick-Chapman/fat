module Top (main) where

import Browse qualified (dump,interact)
import System.Environment (getArgs)
import Text.Printf (printf)
import DateTime (wellKnownDate,wellKnownTime,parseDate,parseTime)

main :: IO ()
main = do
  args <- getArgs
  let Config{imageFile,mode} = parseConfig args
  case mode of
    Dump -> Browse.dump imageFile

    -- TODO: use actual current date/time when not in batch mode
    Interact -> Browse.interact True (wellKnownDate, wellKnownTime) imageFile

    Batch{dateString,timeString} -> do
      --print ("Batch",dateString,timeString)
      let date = parseDate dateString
      let time = parseTime timeString
      Browse.interact False (date,time) imageFile

data Config = Config
  { imageFile :: FilePath
  , mode :: Mode
  }

data Mode = Dump | Interact | Batch { dateString :: String, timeString :: String }

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
      "-b":d:t:rest -> loop acc { mode = Batch d t } rest
      flag@('-':_):_ -> error (printf "unknown flag: %s" flag)
      imageFile:rest -> loop acc { imageFile } rest
