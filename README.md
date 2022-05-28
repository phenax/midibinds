# midibinds
An experiment to use my midi keyboard as a secondary keyboard for executing commands and macros

Configuration is inside `src/Config.hs`

Config example -

```haskell
setVolume :: Int -> String
setVolume vol =
  "amixer sset Master '" ++ show (toPerc vol) "%'"

setBrightness :: Int -> String
setBrightness brightness =
  "brightnessctl s '" ++ show (toPerc brightness) ++ "%'"

handlers =
  [
    -- Run a command when a key is released
    KeyUp 32 $ Shell $ const "spotify",
    KeyUp 36 $ Shell $ const "xdotool key super+enter", -- Mock key press
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

### Install
- Clone the repo
- Edit `src/Config.hs`
- Run `make install` to install (Required cabal)
- This should install it to your cabal install directory (Default: `~/.cabal/bin/midibinds`). Make sure that it's in your PATH


### Run
- `midibinds devices` - See list of connected devices
- `midibinds connect "Device name"` - Connect to a device with the name "Device name" and execute bindings
- `midibinds debug "Device name"` - Connect to a device and log out all midi messages. Useful for getting key values
- `midibinds --help` - See help menu

