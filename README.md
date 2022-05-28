# Midi Keybindings
An experiment to use my midi keyboard as a secondary keyboard for executing commands and macros

Configuration is inside `src/Config.hs`

Config example -

```haskell
setVolume :: Int -> String
setVolume vol =
  "amixer sset Master '" ++ toPerc vol "%'"

setBrightness :: Int -> String
setBrightness brightness =
  "brightnessctl s '" ++ show (toPerc brightness) ++ "%'"

handlers =
  [
    -- Run a command when a key is released
    KeyUp 32 $ Shell $ const "spotify",
    KeyUp 36 $ Shell $ const "xdotool key super+6 && qutebrowser",
    KeyUp 39 $ Shell $ const "dmenu_run",
    
    -- Run a command immedietely on press instead of on release
    KeyDown 32 $ Shell $ const "",
    
    -- Run arbitrary haskell code
    KeyUp 37 $ Action $ \_ -> liftIO $ print "foobarity",
    
    -- Use controllers to manage volume and screen brightness
    ControlChange 2 $ Shell setVolume,
    ControlChange 3 $ Shell setBrightness,
    
    -- Bind keychords to commands
    -- Eg - Hold 48, 52 and then press 55 to run command
    KeyChord [48, 52] 55 $ Shell $ const "notify-send choredpress"
  ]
```

