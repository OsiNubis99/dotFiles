import qualified Data.Map as M
import Data.Maybe (fromJust)
import Data.Monoid
import System.IO (hPutStrLn)
import XMonad
import XMonad.Actions.CopyWindow (kill1)
import XMonad.Actions.CycleWS (WSType (..), moveTo)
import XMonad.Actions.Promote
import XMonad.Actions.RotSlaves (rotSlavesUp)
import XMonad.Actions.WithAll (killAll, sinkAll)
import XMonad.Hooks.DynamicLog (PP (..), dynamicLogWithPP, wrap, xmobarColor, xmobarPP)
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.FadeInactive
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers (doFullFloat, isFullscreen)
import XMonad.Layout.Accordion
import XMonad.Layout.Drawer
import XMonad.Layout.LimitWindows (limitWindows)
import XMonad.Layout.MultiToggle (EOT (EOT), mkToggle, (??))
import qualified XMonad.Layout.MultiToggle as MT (Toggle (..))
import XMonad.Layout.MultiToggle.Instances (StdTransformers (NBFULL, NOBORDERS))
import XMonad.Layout.PerWorkspace
import XMonad.Layout.Reflect
import XMonad.Layout.Renamed
import XMonad.Layout.ResizableTile
import XMonad.Layout.ShowWName
import XMonad.Layout.Spacing
import XMonad.Layout.Tabbed
import qualified XMonad.StackSet as W
import XMonad.Util.EZConfig (additionalKeysP)
import XMonad.Util.Run (spawnPipe)
import XMonad.Util.SpawnOnce (spawnOnce)

myFont :: String
myFont = "xft:Arimo Nerd Font:weight=bold:pixelsize=11:antialias=true:hinting=true"

myBigFont :: String
myBigFont = "xft:Arimo Nerd Font:regular:size=73:antialias=true:hinting=true"

myModMask :: KeyMask
myModMask = mod4Mask

myTerminal :: String
myTerminal = "alacritty"

myBrowser :: String
myBrowser = "firefox"

myEditor :: String
myEditor = "emacsclient -c -a 'emacs' "

myBorderWidth :: Dimension
myBorderWidth = 2

myNormColor :: String
myNormColor = "#152429"

myFocusColor :: String
myFocusColor = "#ffaa00"

windowCount :: X (Maybe String)
windowCount = gets $ Just . show . length . W.integrate' . W.stack . W.workspace . W.current . windowset

myStartupHook :: X ()
myStartupHook = do
  spawnOnce "picom -f &"
  spawnOnce "xfce4-session &"
  spawnOnce "qlipper &"
  spawnOnce "/usr/bin/emacs --daemon &"
  spawnOnce "connman-gtk --tray &"
  spawnOnce "dunst -config ~/.config/dunst/dunstrc &"
  spawnOnce "~/dotFiles/scripts/setWallpaper.sh '20' &"
  spawnOnce "conky &"

myTabTheme :: Theme
myTabTheme =
  def
    { fontName = myFont,
      activeColor = myFocusColor,
      inactiveColor = myNormColor,
      activeBorderColor = myFocusColor,
      inactiveBorderColor = myNormColor,
      activeTextColor = myNormColor,
      inactiveTextColor = myFocusColor
    }

-- Border U D R L
accordion =
  spacingRaw False (Border 8 8 0 0) True (Border 0 0 0 0) True $
    Mirror (reflectVert Accordion)

tabs =
  renamed [Replace "T"] $
    spacingRaw False (Border 38 8 8 8) True (Border 0 0 0 0) True $
      tabbed shrinkText myTabTheme

tall =
  renamed [Replace "G"] $
    limitWindows 12 $
      spacingRaw False (Border 34 4 4 4) True (Border 4 4 4 4) True $
        ResizableTall 1 (1 / 100) (1 / 2) []

gridTall =
  renamed [Replace "g"] $
    drawer 0.04 0.65 (ClassName "Alacritty") accordion `onBottom` tall

gridTabs =
  renamed [Replace "t"] $
    drawer 0.04 0.65 (ClassName "Alacritty") accordion `onBottom` tabs

defaultLayout = tall ||| gridTabs ||| gridTall ||| tabs

gridLayout = gridTabs ||| gridTall ||| tabs ||| tall

tabLayout = tabs ||| tall ||| gridTabs ||| gridTall

myLayoutHook =
  mkToggle (NBFULL ?? NOBORDERS ?? EOT) $
    onWorkspace (head myWorkspaces) gridLayout $
      onWorkspaces [myWorkspaces !! 1] tabLayout defaultLayout

myWorkspaces :: [String]
myWorkspaces =
  [ "Editor",
    "Web",
    "Chat",
    "File",
    "Tool",
    "Media"
  ]

myWorkspaceIcons :: M.Map String [Char]
myWorkspaceIcons =
  M.fromList $
    zip
      myWorkspaces
      [ "\61595",
        "\62057",
        "\57879",
        "\61563",
        "\57871",
        "\57969"
      ]

myWorkspaceIndices :: M.Map String Integer
myWorkspaceIndices = M.fromList $ zip myWorkspaces [1 ..]

clickable :: String -> String
clickable wsName = "<action=xdotool key super+" ++ show i ++ "><fc=#ffaa00><fn=1>" ++ wsIcon ++ "</fn> </fc>" ++ wsName ++ "</action>"
  where
    i = fromJust $ M.lookup wsName myWorkspaceIndices
    wsIcon = fromJust $ M.lookup wsName myWorkspaceIcons

myShowWNameTheme :: SWNConfig
myShowWNameTheme =
  def
    { swn_font = myBigFont,
      swn_fade = 1,
      swn_bgcolor = "#152429",
      swn_color = "#ffaa00"
    }

myManageHook :: XMonad.Query (Data.Monoid.Endo WindowSet)
myManageHook =
  composeAll
    [ className =? "Code" --> doShift (head myWorkspaces),
      className =? "Emacs" --> doShift (head myWorkspaces),
      className =? "libreoffice" --> doShift (head myWorkspaces),
      className =? "firefox" --> doShift (myWorkspaces !! 1),
      className =? "Opera" --> doShift (myWorkspaces !! 1),
      className =? "discord" --> doShift (myWorkspaces !! 2),
      className =? "TelegramDesktop" --> doShift (myWorkspaces !! 2),
      className =? "Thunar" --> doShift (myWorkspaces !! 3),
      className =? "zoom" --> doFloat <+> doShift (myWorkspaces !! 4),
      className =? "Free Download Manager" --> doShift (myWorkspaces !! 4),
      className =? "vlc" --> doShift (myWorkspaces !! 5),
      className =? "Steam" --> doShift (myWorkspaces !! 5),
      className =? "dota2" --> doFullFloat <+> doShift (myWorkspaces !! 5),
      className =? "Nm-connection-editor" --> doFloat,
      title =? "Oracle VM VirtualBox Manager" --> doFloat,
      title =? "File Operation Progress" --> doFloat,
      resource =? "Dialog" --> doFloat,
      isFullscreen --> doFullFloat
    ]

myLogHook :: X ()
myLogHook = fadeInactiveLogHook fadeAmount
  where
    fadeAmount = 1.0

myKeys :: [(String, X ())]
myKeys =
  [ ("M-c", kill1),
    ("M-q", spawn "lxsession-logout"),
    ("M-S-c", killAll),
    ("M-S-r", spawn "xmonad --restart"),
    -- Programs
    ("M-<Return>", spawn myTerminal),
    ("M-<Space>", spawn "~/dotFiles/scripts/spawnRofi.sh"),
    ("M-f", spawn "firefox --browser"),
    ("M-t", spawn "telegram-desktop"),
    ("M-x", spawn "~/dotFiles/scripts/spawnTrayer.sh"),
    ("M-z", spawn myTerminal),
    -- Workspaces
    ("M-<Left>", moveTo Prev NonEmptyWS),
    ("M-<Right>", moveTo Next NonEmptyWS),
    ("M-S-<Left>", moveTo Prev EmptyWS),
    ("M-S-<Right>", moveTo Next EmptyWS),
    -- Layouts
    ("M-<Up>", windows W.focusUp),
    ("M-<Down>", windows W.focusDown),
    ("M-/", sendMessage NextLayout),
    ("M-,", sendMessage (IncMasterN 1)),
    ("M-.", sendMessage (IncMasterN (-1))),
    ("M-a", sinkAll),
    ("M-b", sendMessage (MT.Toggle NBFULL) >> sendMessage ToggleStruts),
    ("M-d", decWindowSpacing 2),
    ("M-i", incWindowSpacing 2),
    ("M-m", promote),
    ("M-C-<Left>", sendMessage Shrink),
    ("M-C-<Right>", sendMessage Expand),
    ("M-C-<Up>", sendMessage MirrorExpand),
    ("M-C-<Down>", sendMessage MirrorShrink),
    ("M-S-<Up>", windows W.swapUp),
    ("M-S-<Down>", windows W.swapDown),
    ("M-S-/", rotSlavesUp),
    ("M-S-i", incScreenSpacing 2),
    ("M-S-d", decScreenSpacing 2),
    -- Wallpapers
    ("M-w o", spawn "nitrogen"),
    ("M-w a", spawn "~/dotFiles/scripts/setWallpaper.sh '20'"),
    ("M-w f", spawn "nitrogen --restore"),
    ("M-w l", spawn "~/dotFiles/scripts/listWallpaper.sh"),
    ("M-w 1", spawn "~/dotFiles/scripts/setWallpaper.sh '0'"),
    ("M-w 2", spawn "~/dotFiles/scripts/setWallpaper.sh '1'"),
    ("M-w 3", spawn "~/dotFiles/scripts/setWallpaper.sh '2'"),
    ("M-w 4", spawn "~/dotFiles/scripts/setWallpaper.sh '3'"),
    ("M-w 5", spawn "~/dotFiles/scripts/setWallpaper.sh '4'"),
    ("M-w 6", spawn "~/dotFiles/scripts/setWallpaper.sh '5'"),
    ("M-w 7", spawn "~/dotFiles/scripts/setWallpaper.sh '6'"),
    ("M-w 8", spawn "~/dotFiles/scripts/setWallpaper.sh '7'"),
    ("M-w 9", spawn "~/dotFiles/scripts/setWallpaper.sh '8'"),
    ("M-w 0", spawn "~/dotFiles/scripts/setWallpaper.sh '9'"),
    -- Editor
    ("M-e o", spawn myEditor),
    ("M-e e", spawn (myEditor ++ "--eval '(dashboard-refresh-buffer)'")),
    ("M-e 0", spawn (myEditor ++ "~/Repos/OsiNubis99")),
    ("M-e 1", spawn (myEditor ++ "~/dotFiles")),
    ("M-e 2", spawn (myEditor ++ "~/Repos/Bots/CaidaVZLABot")),
    ("M-e 3", spawn (myEditor ++ "~/Repos/Web/Ofimania")),
    ("M-e 4", spawn (myEditor ++ "~/Repos")),
    ("M-e 5", spawn (myEditor ++ "~/Repos")),
    ("M-e 6", spawn (myEditor ++ "~/Repos")),
    ("M-e 7", spawn (myEditor ++ "~/Repos")),
    ("M-e 8", spawn (myEditor ++ "~/Repos")),
    ("M-e 9", spawn (myEditor ++ "~/Repos")),
    -- Notifications
    ("M-o", spawn "dunstctl history-pop"),
    ("M-S-o", spawn "dunstctl close"),
    -- Multimedia Keys
    ("<XF86AudioLowerVolume>", spawn "~/dotFiles/scripts/volume.sh down 5"),
    ("<XF86AudioRaiseVolume>", spawn "~/dotFiles/scripts/volume.sh up 5"),
    ("<XF86AudioMute>", spawn "~/dotFiles/scripts/volume.sh mute"),
    ("<XF86MonBrightnessUp>", spawn "~/dotFiles/scripts/backlight.sh up 10"),
    ("<XF86MonBrightnessDown>", spawn "~/dotFiles/scripts/backlight.sh down 10"),
    ("<XF86HomePage>", spawn myBrowser),
    ("<Print>", spawn "flameshot full -c"),
    ("M-<Print>", spawn "flameshot gui")
  ]

main :: IO ()
main = do
  xmproc <- spawnPipe "xmobar"
  xmonad $
    ewmh
      def
        { manageHook = manageDocks <+> myManageHook,
          handleEventHook = docksEventHook <+> fullscreenEventHook,
          modMask = myModMask,
          terminal = myTerminal,
          startupHook = myStartupHook,
          layoutHook = showWName' myShowWNameTheme myLayoutHook,
          workspaces = myWorkspaces,
          borderWidth = myBorderWidth,
          normalBorderColor = myNormColor,
          focusedBorderColor = myFocusColor,
          logHook =
            myLogHook
              <+> dynamicLogWithPP
                xmobarPP
                  { ppOutput = hPutStrLn xmproc,
                    ppCurrent = xmobarColor "#00d1ff" "" . clickable,
                    ppVisible = xmobarColor "#c792ea" "" . clickable,
                    ppHidden = xmobarColor "#3fd12e" "" . clickable,
                    ppHiddenNoWindows = clickable,
                    ppUrgent = xmobarColor "#C45500" "" . wrap "!" "!" . clickable,
                    ppSep = "<fc=#ffffff> <fn=1>|</fn> </fc>",
                    ppWsSep = " ",
                    ppExtras = [windowCount],
                    ppOrder = \(ws : l : t : ex) -> [l, ws] ++ ex
                  }
        }
      `additionalKeysP` myKeys
