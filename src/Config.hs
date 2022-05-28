module Config where

import Action
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.List (intercalate)
import Utils (toPerc)

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

-- Toggle mute and mic mute
handlers :: [Handler]
handlers =
  [ KeyUp 32 $ Shell $ const "xdotool key super+1",
    KeyUp 36 $ Shell $ const "xdotool key super+6",
    KeyUp 39 $ Shell $ const "dmenu_run",
    KeyUp 37 $ Action $ \_ -> liftIO $ print "foobarity",
    ControlChange 2 $ Shell setVolume,
    ControlChange 3 $ Shell setBrightness,
    ControlChange 4 $ Shell setMicVolume,
    KeyChord [48, 52] 55 $ Shell $ const "notify-send choredpress"
  ]
