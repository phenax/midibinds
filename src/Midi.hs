module Midi where

import Control.Monad (void)
import Control.Monad.IO.Class (liftIO)
import Data.List (find)
import MiMonad
import qualified Sound.PortMidi as PM
import qualified Sound.PortMidi.Simple as PM

runWithMidi :: (a -> MiMonad ()) -> a -> IO ()
runWithMidi m a = (PM.withMidi . runMi $ m a) >>= either displayError pure . void

getInputDevices :: MiMonad [(PM.DeviceID, PM.DeviceInfo)]
getInputDevices = filter ((== True) . PM.input . snd) <$> liftIO PM.getDevices

findDeviceByName :: String -> MiMonad PM.DeviceID
findDeviceByName name = do
  inputDevices <- filter ((== True) . PM.input . snd) <$> liftIO PM.getDevices

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
