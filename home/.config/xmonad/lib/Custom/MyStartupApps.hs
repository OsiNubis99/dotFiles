module Custom.MyStartupApps where

import           XMonad
import           XMonad.Util.SpawnOnce

myWallpaperPath :: String
myWallpaperPath = "~/wallpapers/kanagawa_custom.png"

myStartupHook :: X ()
myStartupHook = do
  let wallpaperCmd   = "feh --bg-scale " ++ myWallpaperPath
      lauchPolybar   = "~/.config/polybar/launch.sh"
      blurCmd        = "~/scripts/feh-blur.sh -s; ~/scripts/feh-blur.sh -d"
      picomCmd       = "killall -9 picom; sleep 2 && picom -b &"
      easyeffectsCmd = "easyeffects --gapplication-service &"
      ewwCmd         = "~/.config/eww/scripts/startup.sh"
  sequence_
    [ spawn wallpaperCmd
    , spawn blurCmd
    , spawn picomCmd
    , spawnOnce easyeffectsCmd
    , spawnOnce lauchPolybar
    , spawn ewwCmd
    ]
