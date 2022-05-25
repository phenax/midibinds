module Config where

import Action
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
  [ KeyUp 44 $ Shell $ const "xdotool key super+1",
    KeyUp 48 $ Shell $ const "xdotool key super+6",
    KeyUp 51 $ Shell $ const "dmenu_run",
    KeyUp 45 $ Action $ \_ -> print "foobarity",
    ControlChange 2 $ Shell setVolume,
    ControlChange 3 $ Shell setBrightness,
    ControlChange 4 $ Shell setMicVolume
  ]
