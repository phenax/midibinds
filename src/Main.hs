module Main where

-- import Control.Concurrent (threadDelay)

import Control.Concurrent (threadDelay)
import Control.Monad (forever, void)
import Data.List (find)
import Sound.PortMidi.Simple (ChannelMessage (..), key)
import qualified Sound.PortMidi.Simple as Midi
import qualified System.Process as Proc

data MidiEvent
  = KeyDown Int
  | KeyUp Int

data Command
  = Shell String
  | Execute ()
  deriving (Show)

shell :: String -> IO ()
shell _ = pure ()

handlers :: [(MidiEvent, Command)]
handlers =
  [ (KeyDown 44, Shell "dmenu_run"),
    (KeyDown 51, Shell "echo \"1\n2\n3\n4\n5\" | dmenu -p 'LAMBDA > '"),
    (KeyDown 48, Shell "st")
  ]

getCommand :: Midi.ChannelMessage -> Maybe Command
getCommand = \case
  NoteOn {key} -> snd <$> find ((\case (KeyDown n) -> key == n; _ -> False) . fst) handlers
  NoteOff {key} -> snd <$> find ((\case (KeyUp n) -> key == n; _ -> False) . fst) handlers
  _ -> Nothing

runCommand :: Command -> IO ()
runCommand = \case
  Shell cmd -> void . Proc.spawnCommand $ cmd
  _ -> pure ()

onMessage :: Midi.Message -> IO ()
onMessage = \case
  Midi.Channel _ msg -> do
    -- print msg
    case getCommand msg of
      Just cmd -> runCommand cmd
      Nothing -> pure ()
  _ -> pure ()

main :: IO ()
main = Midi.withMidi $ do
  devices <- Midi.getDevices
  print devices
  -- TODO: Show devices and pick from

  Just device <- Midi.findInputNamed "MPK Mini Mk II MIDI 1"
  -- TODO: Show device not found error

  Midi.withInput device $ \stream ->
    Midi.withReadMessages stream 256 $ \readMessages ->
      forever $ do
        readMessages >>= mapM_ (onMessage . snd)
        threadDelay 1000
