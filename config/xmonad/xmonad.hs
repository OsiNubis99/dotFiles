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
import XMonad.Hooks.EwmhDesktops  -- for some fullscreen events, also for xcomposite in obs.
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
import XMonad.Layout.ShowWName
import XMonad.Layout.Simplest
import XMonad.Layout.Spacing
import XMonad.Layout.SubLayouts
import XMonad.Layout.WindowNavigation
import XMonad.Layout.WindowArranger (windowArrange, WindowArrangerMsg(..))
import qualified XMonad.Layout.ToggleLayouts as T (toggleLayouts, ToggleLayout(Toggle))
import qualified XMonad.Layout.MultiToggle as MT (Toggle(..))

-- Prompt
import XMonad.Prompt
import XMonad.Prompt.Input
import XMonad.Prompt.FuzzyMatch
import XMonad.Prompt.Man
import XMonad.Prompt.Pass
import XMonad.Prompt.Shell
import XMonad.Prompt.Ssh
import XMonad.Prompt.Unicode
import XMonad.Prompt.XMonad
import Control.Arrow (first)

-- Utilities
import XMonad.Util.EZConfig (additionalKeysP)
import XMonad.Util.NamedScratchpad
import XMonad.Util.Run (runProcessWithInput, safeSpawn, spawnPipe)
import XMonad.Util.SpawnOnce

myFont :: String
myFont = "xft:Hack Nerd Font:weight=bold:pixelsize=11:antialias=true:hinting=true"

myBigFont :: String
myBigFont = "xft:Hack Nerd Font:regular:size=73:antialias=true:hinting=true"

myModMask :: KeyMask
myModMask = mod4Mask    -- Sets modkey to super/windows key

myTerminal :: String
myTerminal = "alacritty"  -- Sets default terminal

myBrowser :: String
myBrowser = "firefox"        -- Sets qutebrowser as browser for tree select

myBorderWidth :: Dimension
myBorderWidth = 2      -- Sets border width for windows

myNormColor :: String
myNormColor  = "#152429"  -- Border color of normal windows

myFocusColor :: String
myFocusColor  = "#ffaa00"  -- Border color of focused windows

altMask :: KeyMask
altMask = mod1Mask    -- Setting this for use in xprompts

windowCount :: X (Maybe String)
windowCount = gets $ Just . show . length . W.integrate' . W.stack . W.workspace . W.current . windowset

myStartupHook :: X ()
myStartupHook = do
        spawnOnce "nitrogen --random q--set-zoom-fill"
        spawnOnce "qlipper"
        spawnOnce "picom -f"
        spawnOnce "nm-applet"
        spawnOnce "volumeicon"
        spawnOnce "~/dotFiles/scripts/spawnOnceFDM.sh"
        spawnOnce "~/dotFiles/scripts/spawnOnceTelegram.sh"
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
    $ subLayout [] (smartBorders Simplest)
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

myLayoutHook = avoidStruts $ mouseResize $ windowArrange $ T.toggleLayouts floats
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
-- , (className =? "firefox" <&&> resource =? "Dialog") --> doFloat  -- Float Firefox Dialog
  , isFullscreen --> doFullFloat
  ]

myLogHook :: X ()
myLogHook = fadeInactiveLogHook fadeAmount
  where fadeAmount = 1.0

myKeys :: [(String, X ())]
myKeys =
    [ ("M-<Space>", spawn "ulauncher-toggle") -- ulaunch
    , ("M-c", kill1) -- Kill the currently focused client
    , ("M-S-c", killAll) -- Kill all windows on current workspace
    , ("M-S-q", io exitSuccess) -- Quits xmonad
    , ("M-S-r", spawn "xmonad --restart") -- Restarts xmonad
    , ("M-C-r", spawn "xmonad --recompile") -- Recompiles xmonad
-- Useful programs to have a keybinding for launch
    , ("M-<Return>", spawn (myTerminal)) -- Launch a terminal
    , ("M-z", spawn (myTerminal)) -- Launch a terminal
    , ("M-x", spawn "~/dotFiles/scripts/spawnTrayer.sh") -- Launch the trayer
    , ("M-q", spawn "~/dotFiles/scripts/killTrayers.sh") -- Kill all trayers
-- Workspaces
    , ("M-<Right>", moveTo Next NonEmptyWS) -- Shifts focused window to next ws
    , ("M-<Left>", moveTo Prev NonEmptyWS) -- Shifts focused window to prev ws
    , ("M-S-<Right>", moveTo Next EmptyWS) -- Shifts focused window to next ws
    , ("M-S-<Left>", moveTo Prev EmptyWS) -- Shifts focused window to prev ws
-- Layouts
    , ("M-<Up>", sendMessage (MT.Toggle NBFULL) >> sendMessage ToggleStruts) -- Toggles noborder/full
    , ("M-<Down>", sendMessage NextLayout) -- Change to the next layaout
    , ("M-b", sendMessage Toggle) -- Expand the focus windows (Magnify)
    , ("M-t", withFocused $ windows . W.sink)  -- Push floating window back to tile
    , ("M-a", sinkAll) -- Push ALL floating windows to tile
    , ("M-m", windows W.swapMaster) -- Move focus to the master window
    , ("M-/", windows W.focusDown) -- Move focus to the next window
    , ("M-,", sendMessage (IncMasterN 1)) -- Increase number of clients in master pane
    , ("M-.", sendMessage (IncMasterN (-1))) -- Decrease number of clients in master pane
    , ("M-d", decWindowSpacing 2) -- Decrease window spacing
    , ("M-i", incWindowSpacing 2) -- Increase window spacing
    , ("M-C-d", decScreenSpacing 2) -- Decrease screen spacing
    , ("M-C-i", incScreenSpacing 2) -- Increase screen spacing
    , ("M-S-<Up>", sendMessage MagnifyMore) -- Increase magnify window
    , ("M-S-<Down>", sendMessage MagnifyLess) -- Decrease magnify window
    , ("M-S-<Space>", sendMessage ToggleStruts) -- Hide bar
    , ("M-C-<Left>", sendMessage Shrink) -- Shrink horiz window
    , ("M-C-<Right>", sendMessage Expand) -- Expand horiz window
    , ("M-C-<Up>", sendMessage MirrorExpand) -- Expand vert window
    , ("M-C-<Down>", sendMessage MirrorShrink) -- Shrink vert window
-- Wallpapers
    , ("M-w o", spawn "nitrogen") -- Set a random wallpaper
    , ("M-w a", spawn "nitrogen --random --set-zoom-fill") -- Set a random wallpaper
    , ("M-w f", spawn "nitrogen --restore") -- Set a default wallpaper
    , ("M-w 1", spawn "~/dotFiles/scripts/setWallAC.sh") -- Set a random wallpaper from AC folder
    , ("M-w 2", spawn "~/dotFiles/scripts/setWallCOD.sh") -- Set a random wallpaper from COD folder
    , ("M-w 3", spawn "~/dotFiles/scripts/setWallRM.sh") -- Set a random wallpaper from R&M folder
-- Sub layouts
    , ("M-S-;", sendMessage $ pullGroup L)
    , ("M-S-]", sendMessage $ pullGroup R)
    , ("M-S-[", sendMessage $ pullGroup U)
    , ("M-S-'", sendMessage $ pullGroup D)
    , ("M-S-m", withFocused (sendMessage . MergeAll))
    , ("M-S-u", withFocused (sendMessage . UnMerge))
    , ("M-S-/", withFocused (sendMessage . UnMergeAll))

-- Windows navigation
    , ("M-k", windows W.focusUp)    -- Move focus to the prev window
    , ("M-S-j", windows W.swapDown)  -- Swap focused window with next window
    , ("M-S-k", windows W.swapUp)  -- Swap focused window with prev window
    , ("M-S-<Backspace>", promote)    -- Moves focused window to master, others maintain order
    , ("M-S-<Tab>", rotSlavesDown)  -- Rotate all windows except master and keep focus in place
    , ("M-C-<Tab>", rotAllDown)    -- Rotate all the windows in the current stack

-- Multimedia Keys
    -- , ("<XF86AudioPlay>", spawn (myTerminal ++ "mocp --play"))
    -- , ("<XF86AudioPrev>", spawn (myTerminal ++ "mocp --previous"))
    -- , ("<XF86AudioNext>", spawn (myTerminal ++ "mocp --next"))
    , ("<XF86AudioMute>",  spawn "amixer set Master toggle")
    , ("<XF86AudioLowerVolume>", spawn "amixer set Master 5%- unmute")
    , ("<XF86AudioRaiseVolume>", spawn "amixer set Master 5%+ unmute")
    , ("<XF86MonBrightnessUp>", spawn "xbacklight -inc 20")
    , ("<XF86MonBrightnessDown>", spawn "xbacklight -dec 20")
    , ("<XF86HomePage>", spawn (myBrowser))
    -- , ("<XF86Search>", safeSpawn (myBrowser) ["https://www.duckduckgo.com/"])
    -- , ("<XF86Mail>", runOrRaise "thunderbird" (resource =? "thunderbird"))
    -- , ("<XF86Calculator>", runOrRaise "qalculate-gtk" (resource =? "qalculate-gtk"))
    -- , ("<XF86Eject>", spawn "toggleeject")
    , ("<Print>", spawn "flameshot gui")
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
