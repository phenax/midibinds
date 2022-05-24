module Main where

-- import Control.Concurrent (threadDelay)

import Control.Concurrent (threadDelay)
import Control.Monad (forever)
import qualified Sound.PortMidi.Simple as Midi

onMessage :: Midi.Message -> IO ()
onMessage = print

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
