module Main where

import Action
import Control.Concurrent (threadDelay)
import Control.Monad (forever)
import Data.List (find, intercalate)
import qualified Sound.PortMidi as PM (DeviceInfo (..))
import qualified Sound.PortMidi.Simple as Midi
import qualified Sound.PortMidi.Simple as PM
import qualified System.Environment as Env
import Utils

alsaSet :: String -> Int -> String
alsaSet name perc =
  "amixer sset '" ++ name ++ "' '" ++ show perc ++ "%'"

setVolume :: Int -> String
setVolume vol = alsaSet "Master" $ toPerc vol

setBrightness :: Int -> String
setBrightness brightness =
  "brightnessctl s '" ++ show (toPerc brightness) ++ "%'"

setMicVolume :: Int -> String
setMicVolume vol =
  intercalate
    " && "
    [ alsaSet "Mic Boost" $ toPerc vol,
      alsaSet "Internal Mic Boost" $ toPerc vol
    ]

handlers :: [Handler]
handlers =
  [ KeyUp 44 $ Shell $ const "xdotool key super+1",
    KeyUp 48 $ Shell $ const "xdotool key super+6",
    KeyUp 51 $ Shell $ const "dmenu_run",
    KeyUp 45 $ Action $ \_ -> print "foobarity",
    ControlChange 2 $ Shell setVolume,
    ControlChange 3 $ Shell setBrightness,
    ControlChange 4 $ Shell setMicVolume
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

  let findDevice n = find ((== n) . PM.name . snd) inputDevices

  case head' args >>= findDevice of
    Just (deviceId, PM.DeviceInfo {PM.name}) -> do
      putStrLn $ "Searching for device: " ++ name
      Just (device, _) <- PM.findDevice ((== deviceId) . fst)

      Midi.withInput device $ \stream ->
        Midi.withReadMessages stream 256 $ \readMessages ->
          forever $ do
            readMessages >>= mapM_ (onMessage . snd)
            threadDelay 1000
    Nothing -> putStrLn "Error: Input device not found"
