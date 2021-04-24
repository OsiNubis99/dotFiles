-- Base
import XMonad
import System.IO (hPutStrLn)
import System.Exit (exitSuccess)
import qualified XMonad.StackSet as W
-- Actions
import XMonad.Actions.CopyWindow (kill1)
import XMonad.Actions.CycleWS (moveTo, shiftTo, WSType(..), nextScreen, prevScreen)
import XMonad.Actions.GridSelect
import XMonad.Actions.MouseResize
import XMonad.Actions.Promote
import XMonad.Actions.RotSlaves (rotSlavesDown, rotAllDown)
import XMonad.Actions.WindowGo (runOrRaise)
import XMonad.Actions.WithAll (sinkAll, killAll)
-- Data
import Data.Monoid
-- Hooks
import XMonad.Hooks.DynamicLog ( dynamicLogWithPP  , wrap, xmobarPP , xmobarColor , PP(..))
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.FadeInactive
import XMonad.Hooks.ManageDocks (avoidStruts, docksEventHook, manageDocks, ToggleStruts(..))
import XMonad.Hooks.ManageHelpers (isFullscreen, doFullFloat)
import XMonad.Hooks.ServerMode
import XMonad.Hooks.SetWMName
import XMonad.Hooks.WorkspaceHistory
-- Layouts
import XMonad.Layout.SimplestFloat
import XMonad.Layout.ResizableTile
import XMonad.Layout.Tabbed
-- Layouts modifiers
import XMonad.Layout.LayoutModifier
import XMonad.Layout.LimitWindows (limitWindows, increaseLimit, decreaseLimit)
import XMonad.Layout.Magnifier
import XMonad.Layout.MultiToggle (mkToggle, single, EOT(EOT), (??))
import XMonad.Layout.MultiToggle.Instances (StdTransformers(NBFULL, MIRROR))
import XMonad.Layout.NoBorders
import XMonad.Layout.PerWorkspace
import XMonad.Layout.Renamed
-- import XMonad.Layout.ShowWName
import XMonad.Layout.Simplest
import XMonad.Layout.Spacing
import XMonad.Layout.SubLayouts
import XMonad.Layout.WindowNavigation
import XMonad.Layout.WindowArranger (windowArrange)
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

myBigFont :: String
myBigFont = "xft:Hack Nerd Font:regular:size=73:antialias=true:hinting=true"

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

altMask :: KeyMask
altMask = mod1Mask

windowCount :: X (Maybe String)
windowCount = gets $ Just . show . length . W.integrate' . W.stack . W.workspace . W.current . windowset

myStartupHook :: X ()
myStartupHook = do
        spawnOnce "~/dotFiles/scripts/setWallpaper.sh '10'"
        spawnOnce "qlipper"
        spawnOnce "picom -f"
        spawnOnce "nm-applet"
        spawnOnce "volumeicon"
        spawnOnce "ulauncher --no-window-shadow --hide-window"
        spawnOnce "/usr/lib/polkit-gnome/polkit-gnome-authentication-agent-1"

myTabTheme = def
        { fontName      = myFont
        , activeColor    = "#ffaa00"
        , inactiveColor    = "#152429"
        , activeBorderColor  = "#ffaa00"
        , inactiveBorderColor = "#152429"
        , activeTextColor  = "#152429"
        , inactiveTextColor  = "#ffaa00"
        }

defaultSpacing :: Integer -> l a -> XMonad.Layout.LayoutModifier.ModifiedLayout Spacing l a
defaultSpacing i = spacingRaw False (Border i i i i) True (Border i i i i) True

otherSpacing :: Integer -> l a -> XMonad.Layout.LayoutModifier.ModifiedLayout Spacing l a
otherSpacing i = spacingRaw False (Border i i i i) True (Border i i i i) False

tall  = renamed [Replace "G"]
    $ windowNavigation
    $ addTabs shrinkText myTabTheme
    $ subLayout [] (smartBorders $ tabbed shrinkText myTabTheme)
    $ magnifierOff
    $ limitWindows 12
    $ defaultSpacing 4
    $ ResizableTall 1 (3/100) (1/2) []
floats  = renamed [Replace "F"]
    $ windowNavigation
    $ addTabs shrinkText myTabTheme
    $ subLayout [] (smartBorders Simplest)
    $ limitWindows 20 simplestFloat
tabs  = renamed [Replace "T"]
    $ otherSpacing 8
    $ tabbed shrinkText myTabTheme

firstTall= tall ||| tabs

firstTabs= tabs ||| tall

myLayoutHook = avoidStruts
				-- $ mouseResize
								-- $ windowArrange
								$ T.toggleLayouts floats
        $ mkToggle (NBFULL ?? EOT) myDefaultLayout
      where
        myDefaultLayout = onWorkspaces [( myWorkspaces !! 1 )] firstTabs firstTall


xmobarEscape :: String -> String
xmobarEscape = concatMap doubleLts
  where
    doubleLts '<' = "<"
    doubleLts x  = [x]

myWorkspaces :: [String]
myWorkspaces =
  clickable . map xmobarEscape $
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
  [ className =? "Code" --> doShift ( myWorkspaces !! 0 )
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
  , (resource =? "Dialog") --> doFloat  -- Float All Dialogs
  , isFullscreen --> doFullFloat
  ]

myLogHook :: X ()
myLogHook = fadeInactiveLogHook fadeAmount
  where fadeAmount = 1.0

myKeys :: [(String, X ())]
myKeys =
    [ ("M-c", kill1)
    , ("M-S-c", killAll)
    , ("M-S-q", io exitSuccess)
    , ("M-S-r", spawn "xmonad --restart")
    , ("M-C-r", spawn "xmonad --recompile")
      -- Programs
			    , ("M-<Return>", spawn (myTerminal))
    , ("M-<Space>", spawn "ulauncher-toggle")
    , ("M-x", spawn "~/dotFiles/scripts/spawnTrayer.sh")
    , ("M-z", spawn (myTerminal))
      -- Workspaces
    , ("M-<Right>", moveTo Next NonEmptyWS)
    , ("M-<Left>", moveTo Prev NonEmptyWS)
    , ("M-S-<Right>", moveTo Next EmptyWS)
    , ("M-S-<Left>", moveTo Prev EmptyWS)
      -- Layouts
    , ("M-<Up>", sendMessage (MT.Toggle NBFULL) >> sendMessage ToggleStruts)
    , ("M-<Down>", sendMessage NextLayout)
    , ("M-b", sendMessage Toggle)
    , ("M-t", withFocused $ windows . W.sink)  -- Push floating window back to tile
    , ("M-a", sinkAll)
    , ("M-m", windows W.swapMaster)
    , ("M-/", windows W.focusDown)
    , ("M-,", sendMessage (IncMasterN 1))
    , ("M-.", sendMessage (IncMasterN (-1)))
    , ("M-d", decWindowSpacing 2)
    , ("M-i", incWindowSpacing 2)
    , ("M-C-d", decScreenSpacing 2)
    , ("M-C-i", incScreenSpacing 2)
    , ("M-S-<Up>", sendMessage MagnifyMore)
    , ("M-S-<Down>", sendMessage MagnifyLess)
    , ("M-S-<Space>", sendMessage ToggleStruts)
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
      -- Sub layouts
    , ("M-s ;", sendMessage $ pullGroup L)
    , ("M-s ]", sendMessage $ pullGroup R)
    , ("M-s [", sendMessage $ pullGroup U)
    , ("M-s '", sendMessage $ pullGroup D)
    , ("M-s m", withFocused (sendMessage . MergeAll))
    , ("M-s u", withFocused (sendMessage . UnMerge))
    , ("M-s a", withFocused (sendMessage . UnMergeAll))
      -- Multimedia Keys
    , ("<XF86AudioLowerVolume>", spawn "amixer set Master 5%- unmute")
    , ("<XF86AudioRaiseVolume>", spawn "amixer set Master 5%+ unmute")
    , ("<XF86AudioMute>",  spawn "amixer set Master toggle")
    , ("<XF86MonBrightnessUp>", spawn "xbacklight -inc 20")
    , ("<XF86MonBrightnessDown>", spawn "xbacklight -dec 20")
    , ("<XF86HomePage>", spawn (myBrowser))
    , ("<Print>", spawn "flameshot gui")

      -- Windows navigation
    , ("M-k", windows W.focusUp)    -- Move focus to the prev window
    , ("M-S-j", windows W.swapDown)  -- Swap focused window with next window
    , ("M-S-k", windows W.swapUp)  -- Swap focused window with prev window
    , ("M-S-<Backspace>", promote)    -- Moves focused window to master, others maintain order
    , ("M-S-<Tab>", rotSlavesDown)  -- Rotate all windows except master and keep focus in place
    , ("M-C-<Tab>", rotAllDown)    -- Rotate all the windows in the current stack
    ]

main :: IO ()
main = do
  xmproc <- spawnPipe "xmobar"
  xmonad $ ewmh def
    { manageHook = ( isFullscreen --> doFullFloat ) <+> myManageHook <+> manageDocks
    , handleEventHook = serverModeEventHookCmd
        <+> serverModeEventHook
        <+> serverModeEventHookF "XMONAD_PRINT" (io . putStrLn)
        <+> docksEventHook
    , modMask = myModMask
    , terminal = myTerminal
    , startupHook = myStartupHook
    , layoutHook = myLayoutHook
    , workspaces = myWorkspaces
    , borderWidth = myBorderWidth
    , normalBorderColor = myNormColor
    , focusedBorderColor = myFocusColor
    , logHook = workspaceHistoryHook <+> myLogHook <+> dynamicLogWithPP xmobarPP
        { ppOutput = \x -> hPutStrLn xmproc x
        , ppCurrent = xmobarColor "#ffaa00" "" -- Current workspace in xmobar
        , ppHidden = xmobarColor "#3fd12e" "" -- Hidden workspaces in xmobar
        , ppHiddenNoWindows = \ws -> ws            -- Hidden workspaces (no windows)
        , ppSep =  "<fc=#ffffff><fn=1> | </fn></fc>"    -- Separators in xmobar
        , ppWsSep = " "                    -- Separators
        , ppUrgent = xmobarColor "#C45500" "" . wrap "!" "!" -- Urgent workspace
        , ppExtras  = [windowCount]              -- # of windows current workspace
        , ppOrder  = \(ws:l:t:ex) -> [l,ws]++ex
        }
    } `additionalKeysP` myKeys
