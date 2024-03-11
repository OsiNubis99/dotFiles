module Custom.MyStartupApps where

import           XMonad
import           XMonad.Util.SpawnOnce

myStartupHook :: X ()
myStartupHook = do
  spawnOnce "nm-applet &"
  spawnOnce "~/scripts/setWallpaper.sh &"
  spawnOnce "xfce4-session &"
  spawnOnce "dunst &"
  spawnOnce "killall -9 picom; sleep 2 && picom -b &"
  spawnOnce "eww daemon &"
  spawnOnce "greenclip daemon &"
  spawnOnce "motrix &"
  spawnOnce "/usr/bin/emacs --daemon &"
  spawnOnce "/usr/lib/polkit-gnome/polkit-gnome-authentication-agent-1 &"
  spawnOnce "~/.config/polybar/launch.sh"
  spawnOnce "~/.config/eww/scripts/startup.sh"
      -- easyeffectsCmd = "easyeffects --gapplication-service &"
