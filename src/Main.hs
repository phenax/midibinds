module Main where

import Action
import Control.Concurrent (threadDelay)
import Control.Monad (forever)
import Data.List (find)
import qualified Sound.PortMidi as PM (DeviceInfo (..))
import qualified Sound.PortMidi.Simple as Midi
import qualified Sound.PortMidi.Simple as PM
import qualified System.Environment as Env

handlers :: [Handler]
handlers =
  [ KeyDown 44 $ Shell $ const "dmenu_run",
    KeyDown 51 $ Shell $ const "echo \"1\n2\n3\n4\n5\" | dmenu -p 'LAMBDA > '",
    KeyDown 48 $ Shell $ const "st",
    KeyDown 45 $ Action $ \_ -> print "foobarity",
    PitchWheel $ Action $ \n -> print n
  ]

onMessage :: Midi.Message -> IO ()
onMessage = \case
  Midi.Channel _ msg -> do
    print msg
    runHandler handlers msg
  _ -> pure ()

main :: IO ()
main = Midi.withMidi $ do
  -- TODO: Arg parsing
  args <- Env.getArgs

  inputDevices <- filter ((== True) . PM.input . snd) <$> Midi.getDevices
  print inputDevices

  let maybeName = if null args then Nothing else Just $ head args
  let findDevice n = find ((== n) . PM.name . snd) inputDevices

  case maybeName >>= findDevice of
    Just (deviceId, PM.DeviceInfo {PM.name}) -> do
      putStrLn $ "Searching for device: " ++ name
      Just (device, _) <- PM.findDevice ((== deviceId) . fst)

      Midi.withInput device $ \stream ->
        Midi.withReadMessages stream 256 $ \readMessages ->
          forever $ do
            readMessages >>= mapM_ (onMessage . snd)
            threadDelay 1000
    Nothing -> putStrLn "Error: Input device not found"
