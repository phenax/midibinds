module Main where

import Action
import Config
import Control.Concurrent (threadDelay)
import Control.Monad (forever, void)
import Control.Monad.IO.Class (liftIO)
import Data.List (find)
import MiMonad
import qualified Sound.PortMidi as PM
import qualified Sound.PortMidi.Simple as PM
import qualified System.Environment as Env

findDeviceByName :: String -> MiMonad PM.DeviceID
findDeviceByName name = do
  inputDevices <- filter ((== True) . PM.input . snd) <$> liftIO PM.getDevices
  liftIO $ print inputDevices

  let findDevice n = find ((== n) . PM.name . snd) inputDevices
  case findDevice name of
    Just (deviceId, _) -> pure deviceId
    Nothing -> throw DeviceNotFound

readMessages :: PM.PMStream -> MiMonad [(PM.Timestamp, PM.Message)]
readMessages stream = do
  evs <-
    liftIO (PM.readEvents stream)
      >>= either (\_ -> throw ReadEventError) pure
  liftIO $ PM.mkReadMessages (pure evs)

onMessage :: PM.Message -> MiMonad ()
onMessage (PM.Channel _ msg) = do
  liftIO $ putStrLn $ "[DEBUG/msg]: " ++ show msg
  runHandler handlers msg
onMessage _ = pure ()

start :: MiMonad ()
start = do
  -- TODO: Arg parsing
  args <- liftIO Env.getArgs
  deviceId <- findDeviceByName (head args)

  stream <- liftIO $ PM.midiOpenInput deviceId

  forever $ do
    readMessages stream >>= mapM_ (onMessage . snd)
    liftIO $ threadDelay 1000

-- TODO: Close stream on exception

main :: IO ()
main = (PM.withMidi . runMi $ start) >>= either displayError pure . void
