module Action where

import Control.Monad (forM_, void)
import Control.Monad.IO.Class (MonadIO (liftIO))
import qualified Control.Monad.State as State
import qualified Data.IntSet as Set
import MiMonad (MiMonad, MiState (..))
import qualified Sound.PortMidi.Simple as Midi
import qualified System.Process as Proc

data MidiEvent
  = KeyDown Int (Action ())
  | KeyUp Int (Action ())
  | ControlChange Int (Action Int)
  | PitchWheel (Action Int)
  | KeyChord [Int] Int (Action ())

type Handler = MidiEvent

data Action a
  = Shell (a -> String)
  | Action (a -> MiMonad ())

runHandler :: [Handler] -> Midi.ChannelMessage -> MiMonad ()
runHandler handlers = \case
  Midi.NoteOn {Midi.key} -> do
    MiState {activeKeys} <- State.get
    updateKeys (key :)
    mapHandlers $ \case
      (KeyChord modifiers activator action)
        | activator == key && Set.fromList activeKeys == Set.fromList modifiers ->
          runAction () action
      (KeyDown n action) | key == n -> runAction () action
      _ -> pure ()
  Midi.NoteOff {Midi.key} -> do
    updateKeys $ filter (/= key)
    MiState {activeKeys} <- State.get
    if null activeKeys
      then mapHandlers $ \case
        (KeyUp n action) | key == n -> runAction () action
        _ -> pure ()
      else pure ()
  Midi.PitchWheel {Midi.pitchWheel} -> mapHandlers $ \case
    (PitchWheel action) -> runAction pitchWheel action
    _ -> pure ()
  Midi.ControlChange {Midi.controllerValue, Midi.controllerNumber} -> mapHandlers $ \case
    (ControlChange num action) | controllerNumber == num -> runAction controllerValue action
    _ -> pure ()
  _ -> pure ()
  where
    mapHandlers = forM_ handlers
    updateKeys fn = State.modify $ \(MiState {activeKeys}) -> MiState {activeKeys = fn activeKeys}

runAction :: a -> Action a -> MiMonad ()
runAction val = \case
  Shell cmd -> liftIO $ void . Proc.createProcess . setsid . Proc.shell . cmd $ val
    where
      setsid p = p {Proc.new_session = True}
  Action act -> act val
