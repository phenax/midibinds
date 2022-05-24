module Action where

import Control.Monad (filterM, void)
import qualified Sound.PortMidi.Simple as Midi
import qualified System.Process as Proc

data MidiEvent
  = KeyDown Int (Action ())
  | KeyUp Int (Action ())
  | ControlChange Int (Action Int)
  | PitchWheel (Action Int)

type Handler = MidiEvent

data Action a
  = Shell (a -> String)
  | Action (a -> IO ())

runHandler :: [Handler] -> Midi.ChannelMessage -> IO ()
runHandler handlers = \case
  Midi.NoteOn {Midi.key} -> findAction_ $ \case
    (KeyDown n action) | key == n -> True <$ runAction () action
    _ -> pure False
  Midi.NoteOff {Midi.key} -> findAction_ $ \case
    (KeyUp n action) | key == n -> True <$ runAction () action
    _ -> pure False
  Midi.PitchWheel {Midi.pitchWheel} -> findAction_ $ \case
    (PitchWheel action) -> True <$ runAction pitchWheel action
    _ -> pure False
  Midi.ControlChange {Midi.controllerValue, Midi.controllerNumber} -> findAction_ $ \case
    (ControlChange num action) | controllerNumber == num -> True <$ runAction controllerValue action
    _ -> pure False
  _ -> pure ()
  where
    findAction_ fn = void $ filterM fn handlers

runAction :: a -> Action a -> IO ()
runAction val = \case
  Shell cmd -> void . Proc.createProcess . setsid . Proc.shell . cmd $ val
    where
      setsid p = p {Proc.new_session = True}
  Action io -> io val
