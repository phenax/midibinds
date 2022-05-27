module Main where

import Action
import Config
import Control.Concurrent (threadDelay)
import Control.Monad (forever)
import Control.Monad.IO.Class (liftIO)
import Data.List (find)
import MiMonad
import qualified Sound.PortMidi as PM
import qualified Sound.PortMidi.Simple as Midi
import qualified System.Environment as Env

onMessage :: Midi.Message -> MiMonad ()
onMessage (Midi.Channel _ msg) = do
  liftIO $ putStrLn $ "[DEBUG/msg]: " ++ show msg
  runHandler handlers msg
onMessage _ = pure ()

findDeviceByName :: String -> MiMonad PM.DeviceID
findDeviceByName name = do
  inputDevices <- filter ((== True) . PM.input . snd) <$> liftIO Midi.getDevices
  liftIO $ print inputDevices

  let findDevice n = find ((== n) . PM.name . snd) inputDevices
  case findDevice name of
    Just (deviceId, _) -> pure deviceId
    Nothing -> throw DeviceNotFound

start :: MiMonad ()
start = do
  -- TODO: Arg parsing
  args <- liftIO Env.getArgs
  deviceId <- findDeviceByName (head args)

  liftIO $
    Midi.withInput deviceId $ \stream ->
      Midi.withReadMessages stream 256 $ \readMessages ->
        forever $ do
          readMessages
            >>= (runMi . mapM_ (onMessage . snd))
            >>= either displayError pure
          threadDelay 1000

main :: IO ()
main = (Midi.withMidi . runMi $ start) >>= either displayError pure
