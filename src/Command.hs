module Command where

import Control.Monad (void)
import Data.List (find)
import Sound.PortMidi.Simple (ChannelMessage (..), key)
import qualified System.Process as Proc

data MidiEvent
  = KeyDown Int
  | KeyUp Int

type Handler = (MidiEvent, Action)

data Action
  = Shell String
  | Action (IO ())

getCommand :: [Handler] -> ChannelMessage -> Maybe Action
getCommand handlers = \case
  NoteOn {key} -> snd <$> find ((\case (KeyDown n) -> key == n; _ -> False) . fst) handlers
  NoteOff {key} -> snd <$> find ((\case (KeyUp n) -> key == n; _ -> False) . fst) handlers
  _ -> Nothing

runCommand :: Action -> IO ()
runCommand = \case
  Shell cmd -> void . Proc.createProcess . setsid . Proc.shell $ cmd
    where
      setsid p = p {Proc.new_session = True}
  Action io -> io
