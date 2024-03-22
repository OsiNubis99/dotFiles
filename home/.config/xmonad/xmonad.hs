import Custom.MyDecorations
import Custom.MyLayouts
import Custom.MyManagement
import Custom.MyMouse
-- import           Custom.MyKeys
-- import Custom.MyPolybar
import Custom.MyScratchpads
import Custom.MyScreen
import Custom.MyStartupApps
import Custom.MyTheme
import Custom.MyWorkspaces
import Data.Char
import Data.List
import qualified Data.Map as M
import Data.Maybe
import Data.Monoid
import System.IO
import System.Exit
import XMonad
import XMonad.Actions.CopyWindow
import XMonad.Actions.CycleWS
import XMonad.Actions.GridSelect
import XMonad.Actions.RotSlaves
import qualified XMonad.Actions.Search as S
import XMonad.Actions.SwapWorkspaces
import XMonad.Actions.WithAll
import XMonad.Config.Desktop
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.FadeInactive
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.RefocusLast
import XMonad.Layout.Decoration
import XMonad.Layout.LayoutModifier
import XMonad.Layout.LimitWindows
import XMonad.Layout.MultiToggle
import qualified XMonad.Layout.MultiToggle as MT
import XMonad.Layout.MultiToggle.Instances
import XMonad.Layout.NoBorders
import XMonad.Layout.PerScreen
import XMonad.Layout.PerWorkspace
import XMonad.Layout.Renamed
import XMonad.Layout.ResizableTile
import XMonad.Layout.ShowWName
import XMonad.Layout.Simplest
import XMonad.Layout.Spacing
import XMonad.Layout.Tabbed
import qualified XMonad.Layout.ToggleLayouts as T
import XMonad.Layout.TwoPanePersistent
import XMonad.Prompt.ConfirmPrompt
import XMonad.Prompt.Man
import XMonad.Prompt.XMonad
import qualified XMonad.StackSet as W
import XMonad.Util.EZConfig
import XMonad.Util.NamedActions
import XMonad.Util.NamedScratchpad
import XMonad.Util.Run
import XMonad.Util.SpawnOnce

myModMask :: KeyMask
myModMask = mod4Mask

myTerminal :: String
myTerminal = "alacritty"

myBrowser :: String
myBrowser = "firefox"

myEditor :: String
myEditor = "emacsclient -c -a 'emacs' "

windowCount :: X (Maybe String)
windowCount =
  gets $
    Just
      . show
      . length
      . W.integrate'
      . W.stack
      . W.workspace
      . W.current
      . windowset

showKeybindings :: [((KeyMask, KeySym), NamedAction)] -> NamedAction
showKeybindings x = addName "Show Keybindings" $ io $ do
  h <-
    spawnPipe $
      unwords
        [ "yad",
          "--text-info",
          "--geometry=120x1080",
          "--formatted",
          "--wrap",
          "--justify=fill",
          "--fontname= 'Hack Nerd Font Mono 12'",
          "--fore=",
          fgWhite,
          "back=",
          bgDark,
          "--center",
          "--title 'XMonad keybindings'"
        ]
  hPutStr h (unlines $ showKmSimple x)
  hClose h
  return ()

myKeys :: XConfig l0 -> [((KeyMask, KeySym), NamedAction)]
myKeys c =
  subKeys "Xmonad Essentials" essentialsKeys
    ^++^ subKeys "Workspace" workspaceKeys
    ^++^ subKeys "Window" windowKeys
    ^++^ subKeys "Programs" programsKeys
    ^++^ subKeys "Other" otherKeys
  where
    subKeys str namedAction = subtitle str : mkNamedKeymap c namedAction
    essentialsKeys =
      [ ("M-q", addName "Restart XMonad" $ spawn "xmonad --restart && xmonad --restart"),
        ("M-C-q", addName "Quit Xmonad" (confirmPrompt myPromptConfig "exit" $ io exitSuccess)),
        ("M-S-q", addName "Open Power Menu" $ spawn "~/.config/eww/scripts/customtoggle.sh powermenu"),
        ("M-<Backspace>", addName "Kill focused window" kill1),
        ("M-S-<Backspace>", addName "Kill all windows on WS" killAll)
      ]
    workspaceKeys =
      [ ("M-<Left>", addName "Move to prev WS" $ moveTo Prev nonNSP),
        ("M-<Right>", addName "Move to next WS" $ moveTo Next nonNSP),
        ( "M-S-<Left>",
          addName
            "Move window to prev WS"
            (shiftTo Prev nonNSP >> moveTo Prev nonNSP)
        ),
        ( "M-S-<Right>",
          addName
            "Move window to next WS"
            (shiftTo Next nonNSP >> moveTo Next nonNSP)
        )
      ]
    windowKeys =
      [ ("M-<Up>", addName "Move focus to next window" $ windows W.focusUp),
        ("M-k", addName "Move focus to prev window" $ windows W.focusUp),
        ("M-<Down>", addName "Move focus to prev window" $ windows W.focusDown),
        ("M-j", addName "Move focus to next window" $ windows W.focusDown),
        ("M-S-<Up>", addName "Move window up" $ windows W.swapDown),
        ("M-S-j", addName "Move window up" $ windows W.swapDown),
        ("M-S-<Down>", addName "Move window down" $ windows W.swapUp),
        ("M-S-k", addName "Move window down" $ windows W.swapUp),
        ("M-m", addName "Move to master window" $ windows W.focusMaster),
        ("M-S-m", addName "Promote window to master" $ windows W.swapMaster),
        ("M-S-,", addName "Rotate all windows except master" rotSlavesDown),
        ("M-S-.", addName "Rotate all windows current stack" rotAllDown),
        ("M-S-b", addName "Toggle bar show/hide" $ sendMessage ToggleStruts),
        ("M-/", noName $ sendMessage NextLayout),
        ("M-,", noName $ sendMessage (IncMasterN 1)),
        ("M-.", noName $ sendMessage (IncMasterN (-1))),
        ("M-a", noName sinkAll),
        ( "M-b",
          noName $ sendMessage (MT.Toggle NBFULL) >> sendMessage ToggleStruts
        ),
        ("M-d", noName $ decWindowSpacing 2),
        ("M-i", noName $ incWindowSpacing 2),
        ("M-C-<Left>", noName $ sendMessage Shrink),
        ("M-C-<Right>", noName $ sendMessage Expand),
        ("M-C-<Up>", noName $ sendMessage MirrorExpand),
        ("M-C-<Down>", noName $ sendMessage MirrorShrink),
        ("M-S-<Up>", noName $ windows W.swapUp),
        ("M-S-<Down>", noName $ windows W.swapDown),
        ("M-S-/", noName rotSlavesUp),
        ("M-S-i", noName $ incScreenSpacing 2),
        ("M-S-d", noName $ decScreenSpacing 2)
      ]
    programsKeys =
      [ ( "M-<Return>",
          addName "Open floating terminal" $
            namedScratchpadAction myScratchpads "terminal"
        ),
        ( "M-t",
          addName "Open floating terminal" $
            namedScratchpadAction myScratchpads "telegram"
        ),
        ("M-z", addName "Open terminal" $ spawn myTerminal),
        ("M-<Space>", addName "Open Rofi" $ spawn "~/scripts/spawnRofi.sh"),
        ("M-x", addName "Show trayer" $ spawn "~/scripts/spawnTrayer.sh"),
        ("M-e", addName "Spawn Emacs" $ spawn myEditor)
      ]
    otherKeys =
      [ ("M-<Tab>", noName $ spawn "rofi -show drun"),
        ("M-s", noName $ spawn "rofi -show window"),
        ("M-y", noName $ spawn "rofi -modi 'clipboard:greenclip print' -show clipboard -run-command '{cmd}'"),
        -- Wallpapers
        ("M-w o", noName $ spawn "nitrogen"),
        ("M-w f", noName $ spawn "nitrogen --restore"),
        ("M-w l", noName $ spawn "~/scripts/listWallpaper.sh"),
        ("M-w a", noName $ spawn "~/scripts/setWallpaper.sh"),
        ("M-w 1", noName $ spawn "~/scripts/setWallpaper.sh '0'"),
        ("M-w 2", noName $ spawn "~/scripts/setWallpaper.sh '1'"),
        ("M-w 3", noName $ spawn "~/scripts/setWallpaper.sh '2'"),
        ("M-w 4", noName $ spawn "~/scripts/setWallpaper.sh '3'"),
        ("M-w 5", noName $ spawn "~/scripts/setWallpaper.sh '4'"),
        ("M-w 6", noName $ spawn "~/scripts/setWallpaper.sh '5'"),
        ("M-w 7", noName $ spawn "~/scripts/setWallpaper.sh '6'"),
        ("M-w 8", noName $ spawn "~/scripts/setWallpaper.sh '7'"),
        ("M-w 9", noName $ spawn "~/scripts/setWallpaper.sh '8'"),
        ("M-w 0", noName $ spawn "~/scripts/setWallpaper.sh '9'"),
        -- Notifications
        ("M-n", noName $ spawn "dunstctl history-pop"),
        ("M-S-n", noName $ spawn "dunstctl close"),
        -- Multimedia Keys
        ("<XF86AudioLowerVolume>", noName $ spawn "~/scripts/volume.sh down 5"),
        ("<XF86AudioRaiseVolume>", noName $ spawn "~/scripts/volume.sh up 5"),
        ("<XF86AudioMute>", noName $ spawn "~/scripts/volume.sh mute"),
        ("<XF86MonBrightnessUp>", noName $ spawn "~/scripts/backlight.sh up 10"),
        ( "<XF86MonBrightnessDown>",
          noName $ spawn "~/scripts/backlight.sh down 10"
        ),
        ("C-<XF86MonBrightnessUp>", noName $ spawn "blugon --setcurrent='+600'"),
        ("C-<XF86MonBrightnessDown>", noName $ spawn "blugon --setcurrent='-600'"),
        ("<XF86HomePage>", noName $ spawn myBrowser),
        ("<Print>", noName $ spawn "flameshot full -c"),
        ("M-<Print>", noName $ spawn "flameshot gui")
      ]
    nonNSP = WSIs (return (\ws -> W.tag ws /= scratchpadWorkspaceTag))
    nonEmptyNonNSP =
      WSIs (return (\ws -> isJust (W.stack ws) && W.tag ws /= "NSP"))

main :: IO ()
main = do
  xmproc0 <-
    spawnPipe
      "xmobar -x 0 ~/.config/xmobar/xmobarrc -p 'Static { xpos = 0 , ypos = 0, width = 1920, height = 45 }'"
  xmproc1 <-
    spawnPipe
      "xmobar -x 0 ~/.config/xmobar/xmobarrc -p 'Static { xpos = 1920 , ypos = 0, width = 1920, height = 45 }'"
  xmonad $
    addDescrKeys ((mod4Mask, xK_F1), showKeybindings) myKeys $
      ewmh $
        docks $
          desktopConfig 
           { manageHook = myManageHook <+> manageDocks,
              modMask = myModMask,
              mouseBindings = myMouseBindings,
              terminal = myTerminal,
              startupHook = myStartupHook,
              layoutHook = showWName' myShowWNameConfig myLayoutHook,
              workspaces = myWorkspaces,
              borderWidth = myBorderWidth,
              normalBorderColor = myNormalBorderColor,
              focusedBorderColor = myFocusedBorderColor,
              logHook =
                myLogHook
                  <+> dynamicLogWithPP
                    ( filterOutWsPP [scratchpadWorkspaceTag] $
                        xmobarPP
                          { ppOutput = \x -> hPutStrLn xmproc0 x >> hPutStrLn xmproc1 x,
                            ppCurrent = xmobarColor "#00d1ff" "" . clickable,
                            ppVisible = xmobarColor "#c792ea" "" . clickable,
                            ppHidden = xmobarColor "#3fd12e" "" . clickable,
                            ppHiddenNoWindows = xmobarColor "#ffaa00" "" . clickable,
                            ppUrgent = xmobarColor "#C45500" "" . clickable,
                            ppSep = "<fc=#ffffff> <fn=1>|</fn> </fc>",
                            ppWsSep = " ",
                            ppExtras = [windowCount],
                            ppOrder = \(ws : l : t : ex) -> [l, ws] ++ ex
                          }
                    )
            }
