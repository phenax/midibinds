module Command where

import Control.Monad (void)
import Data.List (find)
import Sound.PortMidi.Simple (ChannelMessage (..), key)
import qualified System.Process as Proc

data MidiEvent
  = KeyDown Int
  | KeyUp Int

type Handler = (MidiEvent, Command)

data Command
  = Shell String
  | Execute ()
  deriving (Show)

getCommand :: [Handler] -> ChannelMessage -> Maybe Command
getCommand handlers = \case
  NoteOn {key} -> snd <$> find ((\case (KeyDown n) -> key == n; _ -> False) . fst) handlers
  NoteOff {key} -> snd <$> find ((\case (KeyUp n) -> key == n; _ -> False) . fst) handlers
  _ -> Nothing

runCommand :: Command -> IO ()
runCommand = \case
  Shell cmd -> void . Proc.spawnCommand $ cmd
  _ -> pure ()
