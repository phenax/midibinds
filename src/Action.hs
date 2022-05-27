module Action where

import Control.Monad (forM_, void)
import Control.Monad.IO.Class (MonadIO (liftIO))
import MiMonad (MiMonad)
import qualified Sound.PortMidi.Simple as Midi
import qualified System.Process as Proc

data MidiEvent
  = KeyDown Int (Action ())
  | KeyUp Int (Action ())
  | ControlChange Int (Action Int)
  | PitchWheel (Action Int)

-- KeyChord [Action () -> MidiEvent] (Action ())

type Handler = MidiEvent

data Action a
  = Shell (a -> String)
  | Action (a -> MiMonad ())

runHandler :: [Handler] -> Midi.ChannelMessage -> MiMonad ()
runHandler handlers = \case
  Midi.NoteOn {Midi.key} -> mapHandlers $ \case
    (KeyDown n action) | key == n -> runAction () action
    _ -> pure ()
  Midi.NoteOff {Midi.key} -> mapHandlers $ \case
    (KeyUp n action) | key == n -> runAction () action
    _ -> pure ()
  Midi.PitchWheel {Midi.pitchWheel} -> mapHandlers $ \case
    (PitchWheel action) -> runAction pitchWheel action
    _ -> pure ()
  Midi.ControlChange {Midi.controllerValue, Midi.controllerNumber} -> mapHandlers $ \case
    (ControlChange num action) | controllerNumber == num -> runAction controllerValue action
    _ -> pure ()
  _ -> pure ()
  where
    mapHandlers = forM_ handlers

runAction :: a -> Action a -> MiMonad ()
runAction val = \case
  Shell cmd -> liftIO $ void . Proc.createProcess . setsid . Proc.shell . cmd $ val
    where
      setsid p = p {Proc.new_session = True}
  Action act -> act val
