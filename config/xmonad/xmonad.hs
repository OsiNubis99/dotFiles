import qualified Data.Map as M
import Data.Maybe (fromJust)
import Data.Monoid (Endo)
import System.IO (hPutStrLn)
import XMonad
  ( ChangeLayout (NextLayout),
    Default (def),
    Dimension,
    Full (Full),
    IncMasterN (IncMasterN),
    KeyMask,
    Mirror (Mirror),
    Query,
    Resize (Expand, Shrink),
    WindowSet,
    X,
    XConfig
      ( borderWidth,
        focusedBorderColor,
        handleEventHook,
        layoutHook,
        logHook,
        manageHook,
        modMask,
        normalBorderColor,
        startupHook,
        terminal,
        workspaces
      ),
    XState (windowset),
    className,
    composeAll,
    doFloat,
    doShift,
    gets,
    mod4Mask,
    resource,
    sendMessage,
    spawn,
    title,
    windows,
    xmonad,
    (-->),
    (<+>),
    (=?),
    (|||),
  )
import qualified XMonad as XMonad.Layout
import XMonad.Actions.CopyWindow (kill1)
import XMonad.Actions.CycleWS
import XMonad.Actions.Promote (promote)
import XMonad.Actions.RotSlaves (rotSlavesUp)
import XMonad.Actions.WithAll (killAll, sinkAll)
import XMonad.Hooks.DynamicLog (PP (..), dynamicLogWithPP, wrap, xmobarColor, xmobarPP)
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.FadeInactive (fadeInactiveLogHook)
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers (doFullFloat, isFullscreen)
import XMonad.Layout.Accordion (Accordion (Accordion))
import XMonad.Layout.Drawer
  ( Property (ClassName),
    drawer,
    onBottom,
  )
import XMonad.Layout.LimitWindows (limitWindows)
import XMonad.Layout.MultiToggle (EOT (EOT), mkToggle, (??))
import qualified XMonad.Layout.MultiToggle as MT (Toggle (..))
import XMonad.Layout.MultiToggle.Instances (StdTransformers (NBFULL, NOBORDERS))
import XMonad.Layout.NoBorders (noBorders)
import XMonad.Layout.PerWorkspace (onWorkspace, onWorkspaces)
import XMonad.Layout.Reflect (reflectVert)
import XMonad.Layout.Renamed (Rename (Replace), renamed)
import XMonad.Layout.ResizableTile
  ( MirrorResize (MirrorExpand, MirrorShrink),
    ResizableTall (ResizableTall),
  )
import XMonad.Layout.ShowWName
  ( SWNConfig (swn_bgcolor, swn_color, swn_fade, swn_font),
    def,
    showWName',
  )
import XMonad.Layout.Spacing
  ( Border (Border),
    decScreenSpacing,
    decWindowSpacing,
    incScreenSpacing,
    incWindowSpacing,
    spacingRaw,
  )
import XMonad.Layout.Tabbed
  ( Theme
      ( activeBorderColor,
        activeColor,
        activeTextColor,
        fontName,
        inactiveBorderColor,
        inactiveColor,
        inactiveTextColor
      ),
    shrinkText,
    tabbed,
  )
import qualified XMonad.StackSet as W
import XMonad.Util.EZConfig (additionalKeysP)
import XMonad.Util.Run (spawnPipe)
import XMonad.Util.SpawnOnce (spawnOnce)

myFont :: String
myFont = "xft:Arimo Nerd Font:weight=bold:pixelsize=11:antialias=true:hinting=true"

myBigFont :: String
myBigFont = "xft:Arimo Nerd Font:regular:size=35:antialias=true:hinting=true"

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
  spawnOnce "~/dotFiles/scripts/setWallpaper.sh '20' &"
  spawnOnce "xfce4-session &"
  spawnOnce "picom -f &"
  spawnOnce "/usr/bin/emacs --daemon &"
  spawnOnce "nm-applet &"
  spawnOnce "qlipper &"
  spawnOnce "dunst -config ~/.config/dunst/dunstrc &"
  spawnOnce "conky &"
  spawnOnce "conky -c .config/conky/conky2.conf &"
  spawnOnce "blugon &"

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

split =
  renamed [Replace "S"] $
    limitWindows 12 $
      spacingRaw False (Border 34 4 4 4) True (Border 4 4 4 4) True $
        ResizableTall 1 (1 / 100) (1 / 2) []

editor =
  renamed [Replace "F"] $
    spacingRaw False (Border 30 0 0 0) True (Border 0 0 0 0) True $
      noBorders Full

defaultLayout = split ||| tabs ||| editor

editorDefault = tabs ||| editor ||| split

tabsDefault = tabs ||| editor ||| split

myLayoutHook =
  mkToggle (NBFULL ?? NOBORDERS ?? EOT) $
    onWorkspace (head myWorkspaces) editorDefault $
      onWorkspaces [myWorkspaces !! 1, myWorkspaces !! 5] tabsDefault defaultLayout

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
      swn_color = myFocusColor
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
  [ -- Control
    ("M-c", kill1),
    ("M-<Backspace>", kill1),
    ("M-q", spawn "lxsession-logout"),
    ("M-S-c", killAll),
    ("M-S-<Backspace>", killAll),
    ("M-S-r", spawn "xmonad --restart"),
    -- Programs
    ("M-<Return>", spawn myTerminal),
    ("M-<Space>", spawn "~/dotFiles/scripts/spawnRofi.sh"),
    ("M-<Tab>", spawn "~/dotFiles/scripts/spawnRofi.sh"),
    ("M-o", spawn "~/dotFiles/scripts/spawnRofi.sh"),
    ("M-x", spawn "~/dotFiles/scripts/spawnTrayer.sh"),
    ("M-z", spawn myTerminal),
    -- Workspaces
--    ("M-h", moveTo Prev not emptyWS),
--    ("M-l", moveTo Next not emptyWS),
--    ("M-<Left>", moveTo Prev not emptyWS),
--    ("M-<Right>", moveTo Next not emptyWS),
    ("M-S-<Left>", moveTo Prev emptyWS),
    ("M-S-<Right>", moveTo Next emptyWS),
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
    ("M-e c", spawn (myEditor ++ "--eval '(doom/restart)'")),
    ("M-e k", spawn (myEditor ++ "--eval '(kill-emacs)'")),
    ("M-e r", spawn (myEditor ++ "--eval '(doom/reload)'")),
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
    ("M-n", spawn "dunstctl history-pop"),
    ("M-S-n", spawn "dunstctl close"),
    -- Multimedia Keys
    ("<XF86AudioLowerVolume>", spawn "~/dotFiles/scripts/volume.sh down 5"),
    ("<XF86AudioRaiseVolume>", spawn "~/dotFiles/scripts/volume.sh up 5"),
    ("<XF86AudioMute>", spawn "~/dotFiles/scripts/volume.sh mute"),
    ("<XF86MonBrightnessUp>", spawn "~/dotFiles/scripts/backlight.sh up 10"),
    ("<XF86MonBrightnessDown>", spawn "~/dotFiles/scripts/backlight.sh down 10"),
    ("C-<XF86MonBrightnessUp>", spawn "blugon --setcurrent='+600'"),
    ("C-<XF86MonBrightnessDown>", spawn "blugon --setcurrent='-600'"),
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
--          handleEventHook = docks <+> ewmhFullscreen,
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
