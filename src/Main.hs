module Main where

import Action
import Config
import Control.Concurrent (threadDelay)
import Control.Monad (forM_, forever, (<=<))
import Control.Monad.IO.Class (liftIO)
import Data.Version (showVersion)
import MiMonad
import Midi
import qualified Options.Applicative.Simple as Opt
import Paths_midi_keybindings (version)
import qualified Sound.PortMidi as PM
import qualified Sound.PortMidi.Simple as PM

onMessage :: PM.Message -> MiMonad ()
onMessage (PM.Channel _ msg) = do
  liftIO $ putStrLn $ "[DEBUG/msg]: " ++ show msg
  runHandler handlers msg
onMessage _ = pure ()

connect :: String -> MiMonad ()
connect name = do
  deviceId <- findDeviceByName name
  stream <- liftIO $ PM.midiOpenInput deviceId

  forever $ do
    readMessages stream >>= mapM_ (onMessage . snd)
    liftIO $ threadDelay 1000

-- TODO: Close stream on exception

listDevices :: MiMonad ()
listDevices = do
  devices <- getInputDevices
  liftIO $
    forM_ devices $ \(devId, PM.DeviceInfo {PM.name}) ->
      putStrLn $ "[" ++ show devId ++ "]: " ++ name

main :: IO ()
main = do
  let description = "Use your midi controller to runWithMidi commands on your machine"
  let header = "midibind - Midi Keybindings"

  let runCmd = snd <=< Opt.simpleOptions (showVersion version) header description (pure ())

  runCmd $ do
    Opt.addCommand
      "devices"
      "List all connected input devices"
      (runWithMidi $ const listDevices)
      (pure ())

    Opt.addCommand
      "connect"
      "Connect to an input device by name"
      (runWithMidi connect)
      (Opt.strArgument $ Opt.metavar "<device-name>")

---
