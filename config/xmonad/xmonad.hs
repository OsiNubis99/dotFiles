-- Base
import XMonad
import System.IO (hPutStrLn)
import System.Exit (exitSuccess)
import qualified XMonad.StackSet as W
-- Actions
import XMonad.Actions.CopyWindow (kill1)
import XMonad.Actions.CycleWS (moveTo, shiftTo, WSType(..))
import XMonad.Actions.WithAll (sinkAll, killAll)
-- Data
import Data.Monoid
-- Hooks
import XMonad.Hooks.DynamicLog ( dynamicLogWithPP  , wrap, xmobarPP , xmobarColor , PP(..))
import XMonad.Hooks.FadeInactive
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers (isFullscreen, doFullFloat)
-- Layouts
import XMonad.Layout.Accordion
import XMonad.Layout.Drawer
import XMonad.Layout.ResizableTile
import XMonad.Layout.Tabbed
-- Layouts modifiers
import XMonad.Layout.LimitWindows (limitWindows)
import XMonad.Layout.PerWorkspace
import XMonad.Layout.Renamed
import XMonad.Layout.Spacing
import qualified XMonad.Layout.ToggleLayouts as T (toggleLayouts, ToggleLayout(Toggle))
import qualified XMonad.Layout.MultiToggle as MT (Toggle(..))
-- Prompt
import XMonad.Prompt
-- Utilities
import XMonad.Util.EZConfig (additionalKeysP)
import XMonad.Util.Run (spawnPipe)
import XMonad.Util.SpawnOnce (spawnOnce)

myFont :: String
myFont = "xft:Hack Nerd Font:weight=bold:pixelsize=11:antialias=true:hinting=true"

myModMask :: KeyMask
myModMask = mod4Mask

myTerminal :: String
myTerminal = "alacritty"

myBrowser :: String
myBrowser = "firefox"

myBorderWidth :: Dimension
myBorderWidth = 2

myNormColor :: String
myNormColor  = "#152429"

myFocusColor :: String
myFocusColor  = "#ffaa00"

windowCount :: X (Maybe String)
windowCount = gets $ Just . show . length . W.integrate' . W.stack . W.workspace . W.current . windowset

myStartupHook :: X ()
myStartupHook = do
  spawnOnce "lxsession -r"
  spawnOnce "~/dotFiles/scripts/setWallpaper.sh '10'"
  spawnOnce "dunst -config ~/.config/dunst/dunstrc"
  spawnOnce "qlipper"
  spawnOnce "picom -f"
  spawnOnce "nm-applet"
  spawnOnce "volumeicon"

myTabTheme = def
  { fontName      = myFont
  , activeColor    = myFocusColor
  , inactiveColor    = myNormColor
  , activeBorderColor  = myFocusColor
  , inactiveBorderColor = myNormColor
  , activeTextColor  = myNormColor
  , inactiveTextColor  = myFocusColor
  }
-- Border U D R L
accordion = spacingRaw False (Border 8 8 0 0) True (Border 0 0 0 0 ) True
  $ Mirror Accordion
-- floats = limitWindows 20 simplestFloat
tabs = renamed [Replace "T"]
  $ spacingRaw False (Border 38 8 8 8) True (Border 0 0 0 0 ) True
  $ tabbed shrinkText myTabTheme
tall = renamed [Replace "G"]
  $ limitWindows 12
  $ spacingRaw False (Border 34 4 4 4) True (Border 4 4 4 4 ) True
  $ ResizableTall 1 (1/100) (1/2) []
gridTall = renamed [Replace "A"]
  $ drawer 0.1 0.65 (ClassName "Alacritty") accordion `onBottom` tall
gridTabs = renamed [Replace "A"]
  $ drawer 0.05 0.65 (ClassName "Alacritty") accordion `onBottom` tabs

defaultLayout = tall ||| gridTabs ||| gridTall ||| tabs
editorLayout = gridTabs ||| gridTall ||| tabs ||| tall
webLayout = tabs ||| tall ||| gridTabs ||| gridTall

myLayoutHook = onWorkspace ( head myWorkspaces ) editorLayout
  $ onWorkspace ( myWorkspaces !! 1 ) webLayout defaultLayout

myWorkspaces :: [String]
myWorkspaces =
  clickable
  [ "<fc=#ffaa00><fn=1>\61595 </fn></fc>Editor"
  , "<fc=#ffaa00><fn=1>\62057 </fn></fc>Web"
  , "<fc=#ffaa00><fn=1>\57879 </fn></fc>Chat"
  , "<fc=#ffaa00><fn=1>\61563 </fn></fc>File"
  , "<fc=#ffaa00><fn=1>\57871 </fn></fc>Tool"
  , "<fc=#ffaa00><fn=1>\57969 </fn></fc>Media"]
    where
      clickable l = [ "<action=xdotool key super+" ++ show n ++ ">" ++ ws ++ "</action>" |
        (i,ws) <- zip [1..9] l,
        let n = i]

myManageHook :: XMonad.Query (Data.Monoid.Endo WindowSet)
myManageHook = composeAll
  [ className =? "Code" --> doShift ( head myWorkspaces )
  , className =? "firefox" --> doShift ( myWorkspaces !! 1 )
  , className =? "Opera" --> doShift ( myWorkspaces !! 1 )
  , className =? "discord" --> doShift (  myWorkspaces !! 2 )
  , className =? "TelegramDesktop" --> doShift (  myWorkspaces !! 2 )
  , className =? "Thunar" --> doShift (  myWorkspaces !! 3 )
  , className =? "Free Download Manager" --> doShift  ( myWorkspaces !! 4 )
  , className =? "vlc" --> doShift ( myWorkspaces !! 5 )
  , className =? "Nm-connection-editorq"  --> doFloat
  , title =? "Oracle VM VirtualBox Manager"  --> doFloat
  , title =? "File Operation Progress"  --> doFloat
  , resource =? "Dialog" --> doFloat
  , isFullscreen --> doFullFloat
  ]

myLogHook :: X ()
myLogHook = fadeInactiveLogHook fadeAmount
  where fadeAmount = 1.0

myKeys :: [(String, X ())]
myKeys =
    [ ("M-c", kill1)
    , ("M-q", spawn "lxsession-logout")
    , ("M-S-c", killAll)
    , ("M-S-r", spawn "xmonad --restart")
    , ("M-C-r", spawn "xmonad --recompile")
      -- Programs
    , ("M-<Return>", spawn myTerminal)
    , ("M-<Space>", spawn "~/dotFiles/scripts/spawnRofi.sh")
    , ("M-x", spawn "~/dotFiles/scripts/spawnTrayer.sh")
    , ("M-z", spawn myTerminal)
      -- Workspaces
    , ("M-<Right>", moveTo Next NonEmptyWS)
    , ("M-<Left>", moveTo Prev NonEmptyWS)
    , ("M-S-<Right>", moveTo Next EmptyWS)
    , ("M-S-<Left>", moveTo Prev EmptyWS)
      -- Layouts
    , ("M-<Up>", windows W.focusDown)
    , ("M-<Down>", windows W.focusUp)
    -- , ("M-b", sendMessage Toggle) -- TODO Toggle full screen
    , ("M-t", withFocused $ windows . W.sink)  -- Push floating window back to tile
    , ("M-a", sinkAll)
    , ("M-m", windows W.swapMaster)
    , ("M-/", sendMessage NextLayout)
    , ("M-,", sendMessage (IncMasterN 1))
    , ("M-.", sendMessage (IncMasterN (-1)))
    , ("M-d", decWindowSpacing 2)
    , ("M-i", incWindowSpacing 2)
    , ("M-S-d", decScreenSpacing 2)
    , ("M-S-i", incScreenSpacing 2)
    , ("M-C-<Left>", sendMessage Shrink)
    , ("M-C-<Right>", sendMessage Expand)
    , ("M-C-<Up>", sendMessage MirrorExpand)
    , ("M-C-<Down>", sendMessage MirrorShrink)
      -- Wallpapers
    , ("M-w o", spawn "nitrogen")
    , ("M-w a", spawn "~/dotFiles/scripts/setWallpaper.sh '10'")
    , ("M-w f", spawn "nitrogen --restore")
    , ("M-w l", spawn "~/dotFiles/scripts/listWallpaper.sh")
    , ("M-w 1", spawn "~/dotFiles/scripts/setWallpaper.sh '0'")
    , ("M-w 2", spawn "~/dotFiles/scripts/setWallpaper.sh '1'")
    , ("M-w 3", spawn "~/dotFiles/scripts/setWallpaper.sh '2'")
    , ("M-w 4", spawn "~/dotFiles/scripts/setWallpaper.sh '3'")
    , ("M-w 5", spawn "~/dotFiles/scripts/setWallpaper.sh '4'")
    , ("M-w 6", spawn "~/dotFiles/scripts/setWallpaper.sh '5'")
    , ("M-w 7", spawn "~/dotFiles/scripts/setWallpaper.sh '6'")
    , ("M-w 8", spawn "~/dotFiles/scripts/setWallpaper.sh '7'")
    , ("M-w 9", spawn "~/dotFiles/scripts/setWallpaper.sh '8'")
    , ("M-w 0", spawn "~/dotFiles/scripts/setWallpaper.sh '9'")
      -- Editor
    , ("M-e 0", spawn "code ~/Repos/OsiNubis99")
    , ("M-e 1", spawn "code ~/dotFiles")
    , ("M-e 2", spawn "code ~/Repos/Bots/CaidaVZLABot")
    , ("M-e 3", spawn "code ~/Repos/Web/Ofimania")
    , ("M-e 4", spawn "code ~/Repos")
    , ("M-e 5", spawn "code ~/Repos")
    , ("M-e 6", spawn "code ~/Repos")
    , ("M-e 7", spawn "code ~/Repos")
    , ("M-e 8", spawn "code ~/Repos")
    , ("M-e 9", spawn "code ~/Repos")
      -- Multimedia Keys
    , ("<XF86AudioLowerVolume>", spawn "amixer set Master 5%- unmute")
    , ("<XF86AudioRaiseVolume>", spawn "amixer set Master 5%+ unmute")
    , ("<XF86AudioMute>",  spawn "amixer set Master toggle")
    , ("<XF86MonBrightnessUp>", spawn "xbacklight -inc 20")
    , ("<XF86MonBrightnessDown>", spawn "xbacklight -dec 20")
    , ("<XF86HomePage>", spawn myBrowser)
    , ("<Print>", spawn "flameshot gui")
    ]

main :: IO ()
main = do
  xmproc <- spawnPipe "xmobar"
  xmonad $ def
    { manageHook = manageDocks <+> myManageHook
    , modMask = myModMask
    , terminal = myTerminal
    , startupHook = myStartupHook
    , layoutHook = myLayoutHook
    , workspaces = myWorkspaces
    , borderWidth = myBorderWidth
    , normalBorderColor = myNormColor
    , focusedBorderColor = myFocusColor
    , logHook = myLogHook <+> dynamicLogWithPP xmobarPP
        { ppOutput = hPutStrLn xmproc
        , ppCurrent = xmobarColor "#ffaa00" "" -- Current workspace in xmobar
        , ppHidden = xmobarColor "#3fd12e" "" -- Hidden workspaces in xmobar
        , ppHiddenNoWindows = id            -- Hidden workspaces (no windows)
        , ppSep =  "<fc=#ffffff><fn=1> | </fn></fc>"    -- Separators in xmobar
        , ppWsSep = " "                    -- Separators
        , ppUrgent = xmobarColor "#C45500" "" . wrap "!" "!" -- Urgent workspace
        , ppExtras  = [windowCount]              -- # of windows current workspace
        , ppOrder  = \(ws:l:t:ex) -> [l,ws]++ex
        }
    } `additionalKeysP` myKeys
