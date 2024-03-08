import           Data.Char
import           Data.List
import qualified Data.Map                      as M
import           Data.Maybe
import           Data.Monoid
import           System.IO
import           XMonad
import           XMonad.Actions.CopyWindow
import           XMonad.Actions.CycleWS
import           XMonad.Actions.GridSelect
import           XMonad.Actions.RotSlaves
import qualified XMonad.Actions.Search         as S
import           XMonad.Actions.SwapWorkspaces
import           XMonad.Actions.WithAll
import           XMonad.Hooks.DynamicLog
import           XMonad.Hooks.EwmhDesktops
import           XMonad.Hooks.FadeInactive
import           XMonad.Hooks.ManageDocks
import           XMonad.Hooks.ManageHelpers
import           XMonad.Hooks.RefocusLast
import           XMonad.Layout.Decoration
import           XMonad.Layout.LayoutModifier
import           XMonad.Layout.LimitWindows
import           XMonad.Layout.MultiToggle
import qualified XMonad.Layout.MultiToggle     as MT
import           XMonad.Layout.MultiToggle.Instances
import           XMonad.Layout.NoBorders
import           XMonad.Layout.PerScreen
import           XMonad.Layout.PerWorkspace
import           XMonad.Layout.Renamed
import           XMonad.Layout.ResizableTile
import           XMonad.Layout.ShowWName
import           XMonad.Layout.Simplest
import           XMonad.Layout.Spacing
import           XMonad.Layout.Tabbed
import qualified XMonad.Layout.ToggleLayouts   as T
import           XMonad.Layout.TwoPanePersistent
import qualified XMonad.StackSet               as W
import           XMonad.Util.EZConfig
import           XMonad.Util.NamedActions
import           XMonad.Util.NamedScratchpad
import           XMonad.Util.Run
import           XMonad.Util.SpawnOnce

myFont :: String
myFont =
  "xft:Arimo Nerd Font:weight=bold:pixelsize=11:antialias=true:hinting=true"

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
windowCount =
  gets
    $ Just
    . show
    . length
    . W.integrate'
    . W.stack
    . W.workspace
    . W.current
    . windowset

myStartupHook :: X ()
myStartupHook = do
  spawnOnce "nm-applet &"
  spawnOnce "~/scripts/setWallpaper.sh &"
  spawnOnce "xfce4-session &"
  spawnOnce "dunst &"
  spawnOnce "picom -f &"
  spawnOnce "eww daemon &"
  spawnOnce "greenclip daemon &"
  spawnOnce "/usr/bin/emacs --daemon &"
  spawnOnce "/usr/lib/polkit-gnome/polkit-gnome-authentication-agent-1 &"

myNavigation :: TwoD a (Maybe a)
myNavigation = makeXEventhandler $ shadowWithKeymap navKeyMap navDefaultHandler
 where
  navKeyMap = M.fromList
    [ ((0, xK_Escape), cancel)
    , ((0, xK_Return), select)
    , ((0, xK_slash) , substringSearch myNavigation)
    , ((0, xK_Left)  , move (-1, 0) >> myNavigation)
    , ((0, xK_h)     , move (-1, 0) >> myNavigation)
    , ((0, xK_Right) , move (1, 0) >> myNavigation)
    , ((0, xK_l)     , move (1, 0) >> myNavigation)
    , ((0, xK_Down)  , move (0, 1) >> myNavigation)
    , ((0, xK_j)     , move (0, 1) >> myNavigation)
    , ((0, xK_Up)    , move (0, -1) >> myNavigation)
    , ((0, xK_k)     , move (0, -1) >> myNavigation)
    , ((0, xK_space) , setPos (0, 0) >> myNavigation)
    ]
  navDefaultHandler = const myNavigation

myColorizer :: Window -> Bool -> X (String, String)
myColorizer = colorRangeFromClassName (0x28, 0x2c, 0x34) -- lowest inactive bg
                                      (0x28, 0x2c, 0x34) -- highest inactive bg
                                      (0xc7, 0x92, 0xea) -- active bg
                                      (0xc0, 0xa7, 0x9a) -- inactive fg
                                      (0x28, 0x2c, 0x34) -- active fg

-- gridSelect menu layout
mygridConfig :: p -> GSConfig Window
mygridConfig colorizer = (buildDefaultGSConfig myColorizer)
  { gs_cellheight   = 40
  , gs_cellwidth    = 180
  , gs_cellpadding  = 6
  , gs_navigate     = myNavigation
  , gs_originFractX = 0.5
  , gs_originFractY = 0.5
  , gs_font         = myFont
  }

spawnSelected' :: [(String, String)] -> X ()
spawnSelected' lst = gridselect conf lst >>= flip whenJust spawn
 where
  conf = def { gs_cellheight   = 40
             , gs_cellwidth    = 180
             , gs_cellpadding  = 6
             , gs_originFractX = 0.5
             , gs_originFractY = 0.5
             , gs_font         = myFont
             }

runSelectedAction' :: GSConfig (X ()) -> [(String, X ())] -> X ()
runSelectedAction' conf actions = do
  selectedActionM <- gridselect conf actions
  case selectedActionM of
    Just selectedAction -> selectedAction
    Nothing             -> return ()

gsApps :: [(String, String)]
gsApps =
  [ ("Firefox"   , "firefox")
  , ("Tick Tick" , "ticktick")
  , ("Discord"   , "discord")
  , ("VS Code"   , "code")
  , ("Steam"     , "steam")
  , ("0 A.D."    , "0ad")
  , ("Settings"  , "xfce4-settings-manager")
  , ("Files"     , "thunar")
  , ("Postman"   , "postman")
  , ("VirtualBox", "virtualbox")
  , ("Beekeeper" , "beekeeper-studio")
  ]

myTabTheme :: Theme
myTabTheme = def { fontName            = myFont
                 , activeColor         = myFocusColor
                 , inactiveColor       = myNormColor
                 , activeBorderColor   = myFocusColor
                 , inactiveBorderColor = myNormColor
                 , activeTextColor     = myNormColor
                 , inactiveTextColor   = myFocusColor
                 }

type MyFull
  = ModifiedLayout
      Rename
      (ModifiedLayout Spacing (ModifiedLayout WithBorder Full))

type MySplit
  = ModifiedLayout
      Rename
      (ModifiedLayout LimitWindows (ModifiedLayout Spacing ResizableTall))

type MyTabs
  = ModifiedLayout
      Rename
      ( ModifiedLayout
          Spacing
          ( ModifiedLayout
              (Decoration TabbedDecoration DefaultShrinker)
              Simplest
          )
      )


myLayoutHook
  :: MultiToggle
       (HCons StdTransformers (HCons StdTransformers EOT))
       ( PerWorkspace
           (Choose MyFull (Choose MySplit MyTabs))
           ( PerWorkspace
               (Choose MyTabs (Choose MyFull MySplit))
               (Choose MySplit (Choose MyTabs MyFull))
           )
       )
       Window
myLayoutHook =
  mkToggle (NBFULL ?? NOBORDERS ?? EOT)
    $ onWorkspace (myWorkspaces !! 2) fullDefault
    $ onWorkspaces [head myWorkspaces, myWorkspaces !! 1]
                   tabsDefault
                   splitDefault
 where
  fullDefault  = full ||| split ||| tabs
  tabsDefault  = tabs ||| full ||| split
  splitDefault = split ||| tabs ||| full
  tabs =
    renamed [Replace "T"]
      $ spacingRaw False (Border (45 + 8) 8 8 8) True (Border 0 0 0 0) True
      $ tabbed shrinkText myTabTheme
  split =
    renamed [Replace "S"]
      $ limitWindows 12
      $ spacingRaw False (Border (45 + 4) 4 4 4) True (Border 4 4 4 4) True
      $ ResizableTall 1 (1 / 100) (1 / 2) []
  full =
    renamed [Replace "F"]
      $ spacingRaw False (Border 45 0 0 0) True (Border 0 0 0 0) True
      $ noBorders Full

myWorkspaces :: [String]
myWorkspaces = ["Editor", "Web", "Game", "File", "NSP"]

myWorkspaceIcons :: M.Map String String
myWorkspaceIcons = M.fromList $ zip
  myWorkspaces
  ["\61595", "\62057", "\57879", "\61563", "\57871", "\57969", ""]

myWorkspaceIndices :: M.Map String Integer
myWorkspaceIndices = M.fromList $ zip myWorkspaces [1 ..]

clickable :: String -> String
clickable wsName =
  "<action=xdotool key super+"
    ++ show i
    ++ "><fc=#ffaa00><fn=1>"
    ++ wsIcon
    ++ "</fn> </fc>"
    ++ wsName
    ++ "</action>"
 where
  i      = fromJust $ M.lookup wsName myWorkspaceIndices
  wsIcon = fromJust $ M.lookup wsName myWorkspaceIcons

myShowWNameTheme :: SWNConfig
myShowWNameTheme = def { swn_font    = myBigFont
                       , swn_fade    = 1
                       , swn_bgcolor = "#152429"
                       , swn_color   = myFocusColor
                       }

myManageHook :: XMonad.Query (Data.Monoid.Endo WindowSet)
myManageHook =
  composeAll
    . concat
    $ [ [ className =? c --> doFloat | c <- myFloats ]
      , [ title =? t --> doFloat | t <- myOtherFloats ]
      , [ className =? c --> doShift (head myWorkspaces) | c <- apps0 ]
      , [ className =? c --> doShift (myWorkspaces !! 1) | c <- apps1 ]
      , [ className =? c --> doShift (myWorkspaces !! 2) | c <- apps2 ]
      , [ className =? c --> doShift (myWorkspaces !! 3) | c <- apps3 ]
      , [ stringProperty "WM_CLASS" =? "steam_app" --> doShift
          (myWorkspaces !! 3)
        , resource =? "Dialog" --> doFloat
        , isFullscreen --> doFullFloat
        , namedScratchpadManageHook myScratchpads
        ]
      ]
 where
  apps0 = ["Code", "Emacs"]
  apps1 = ["firefox", "Opera", "ticktick"]
  apps2 = ["steam", "discord"]
  apps3 = ["Thunar", "libreoffice"]
  myFloats =
    [ "confirm"
    , "file_progress"
    , "dialog"
    , "download"
    , "error"
    , "Gimp"
    , "notification"
    , "pinentry-gtk-2"
    , "splash"
    , "toolbar"
    , "Nm-connection-editor"
    , "Yad"
    ]
  myOtherFloats = ["alsamixer"]

myLogHook :: X ()
myLogHook =
  fadeInactiveLogHook fadeAmount
    <+> refocusLastLogHook
    >>  nsHideOnFocusLoss myScratchpads
  where fadeAmount = 1.0

myScratchpads :: [NamedScratchpad]
myScratchpads =
  [ NS "terminal"
       "warp-terminal"
       (className =? "dev.warp.Warp")
       (customFloating $ W.RationalRect 0 (1 / 24) 1 (7 / 10))
  , NS "telegram"
       "telegram-desktop"
       (className =? "TelegramDesktop")
       (customFloating rationalFloating)
  ]
 where
  rationalFloating = W.RationalRect locationLeft locationTop width height
  locationTop      = (1 - height) / 2
  locationLeft     = (1 - width) / 2
  height           = 5 / 6
  width            = 5 / 6

subtitle' :: String -> ((KeyMask, KeySym), NamedAction)
subtitle' x =
  ((0, 0), NamedAction $ map toUpper $ sep ++ "\n-- " ++ x ++ " --\n" ++ sep)
  where sep = replicate (6 + length x) '-'

showKeybindings :: [((KeyMask, KeySym), NamedAction)] -> NamedAction
showKeybindings x = addName "Show Keybindings" $ io $ do
  h <- spawnPipe $ unwords
    [ "yad"
    , "--text-info"
    , "--geometry=120x1080"
    , "--formatted"
    , "--wrap"
    , "--justify=fill"
    , "--fontname= 'Hack Nerd Font Mono 12'"
    , "--fore="
    , myFocusColor
    , "back="
    , myNormColor
    , "--center"
    , "--title 'XMonad keybindings'"
    ]
  hPutStr h (unlines $ showKmSimple x)
  hClose h
  return ()

myKeys :: XConfig l0 -> [((KeyMask, KeySym), NamedAction)]
myKeys c =
  subKeys "Xmonad Essentials" essentialsKeys
    ^++^ subKeys "Workspace" workspaceKeys
    ^++^ subKeys "Window"    windowKeys
    ^++^ subKeys "Programs"  programsKeys
    ^++^ subKeys "Other"     otherKeys
 where
  subKeys str namedAction = subtitle str : mkNamedKeymap c namedAction
  essentialsKeys =
    [ ("M-q"            , addName "Restart XMonad" $ spawn "xmonad --restart")
    , ("M-c"            , addName "Kill focused window" kill1)
    , ("M-<Backspace>"  , addName "Kill focused window" kill1)
    , ("M-S-c"          , addName "Kill all windows on WS" killAll)
    , ("M-S-<Backspace>", addName "Kill all windows on WS" killAll)
    , ("M-<Tab>"        , addName "GridSelect" $ spawnSelected' gsApps)
    ]
  workspaceKeys =
    [ ("M-<Left>" , addName "Move to prev WS" $ moveTo Prev nonNSP)
    , ("M-<Right>", addName "Move to next WS" $ moveTo Next nonNSP)
    , ( "M-S-<Left>"
      , addName "Move window to prev WS"
                (shiftTo Prev nonNSP >> moveTo Prev nonNSP)
      )
    , ( "M-S-<Right>"
      , addName "Move window to next WS"
                (shiftTo Next nonNSP >> moveTo Next nonNSP)
      )
    ]
  windowKeys =
    [ ("M-<Up>", addName "Move focus to next window" $ windows W.focusUp)
    , ("M-k", addName "Move focus to prev window" $ windows W.focusUp)
    , ("M-<Down>", addName "Move focus to prev window" $ windows W.focusDown)
    , ("M-j", addName "Move focus to next window" $ windows W.focusDown)
    , ("M-S-<Up>", addName "Move window up" $ windows W.swapDown)
    , ("M-S-j", addName "Move window up" $ windows W.swapDown)
    , ("M-S-<Down>", addName "Move window down" $ windows W.swapUp)
    , ("M-S-k", addName "Move window down" $ windows W.swapUp)
    , ("M-m", addName "Move to master window" $ windows W.focusMaster)
    , ("M-S-m", addName "Promote window to master" $ windows W.swapMaster)
    , ("M-S-,", addName "Rotate all windows except master" rotSlavesDown)
    , ("M-S-.", addName "Rotate all windows current stack" rotAllDown)
    , ("M-S-b", addName "Toggle bar show/hide" $ sendMessage ToggleStruts)
    , ("M-/"       , noName $ sendMessage NextLayout)
    , ("M-,"       , noName $ sendMessage (IncMasterN 1))
    , ("M-."       , noName $ sendMessage (IncMasterN (-1)))
    , ("M-a"       , noName sinkAll)
    , ( "M-b"
      , noName $ sendMessage (MT.Toggle NBFULL) >> sendMessage ToggleStruts
      )
    , ("M-d"        , noName $ decWindowSpacing 2)
    , ("M-i"        , noName $ incWindowSpacing 2)
    , ("M-C-<Left>" , noName $ sendMessage Shrink)
    , ("M-C-<Right>", noName $ sendMessage Expand)
    , ("M-C-<Up>"   , noName $ sendMessage MirrorExpand)
    , ("M-C-<Down>" , noName $ sendMessage MirrorShrink)
    , ("M-S-<Up>"   , noName $ windows W.swapUp)
    , ("M-S-<Down>" , noName $ windows W.swapDown)
    , ("M-S-/"      , noName rotSlavesUp)
    , ("M-S-i"      , noName $ incScreenSpacing 2)
    , ("M-S-d"      , noName $ decScreenSpacing 2)
    ]
  programsKeys =
    [ ( "M-<Return>"
      , addName "Open floating terminal"
        $ namedScratchpadAction myScratchpads "terminal"
      )
    , ( "M-t"
      , addName "Open floating terminal"
        $ namedScratchpadAction myScratchpads "telegram"
      )
    , ("M-z"      , addName "Open terminal" $ spawn myTerminal)
    , ("M-<Space>", addName "Open Rofi" $ spawn "~/scripts/spawnRofi.sh")
    , ("M-x"      , addName "Show trayer" $ spawn "~/scripts/spawnTrayer.sh")
    , ("M-e"      , addName "Spawn Emacs" $ spawn myEditor)
    ]
  otherKeys =
    [ ("M-<Tab>"               , noName $ spawnSelected' gsApps)
    , ("M-s", noName $ goToSelected $ mygridConfig myColorizer)
    , ("M-S-s", noName $ bringSelected $ mygridConfig myColorizer)
    -- Wallpapers
    , ("M-w o"                 , noName $ spawn "nitrogen")
    , ("M-w f"                 , noName $ spawn "nitrogen --restore")
    , ("M-w l"                 , noName $ spawn "~/scripts/listWallpaper.sh")
    , ("M-w a"                 , noName $ spawn "~/scripts/setWallpaper.sh")
    , ("M-w 1"                 , noName $ spawn "~/scripts/setWallpaper.sh '0'")
    , ("M-w 2"                 , noName $ spawn "~/scripts/setWallpaper.sh '1'")
    , ("M-w 3"                 , noName $ spawn "~/scripts/setWallpaper.sh '2'")
    , ("M-w 4"                 , noName $ spawn "~/scripts/setWallpaper.sh '3'")
    , ("M-w 5"                 , noName $ spawn "~/scripts/setWallpaper.sh '4'")
    , ("M-w 6"                 , noName $ spawn "~/scripts/setWallpaper.sh '5'")
    , ("M-w 7"                 , noName $ spawn "~/scripts/setWallpaper.sh '6'")
    , ("M-w 8"                 , noName $ spawn "~/scripts/setWallpaper.sh '7'")
    , ("M-w 9"                 , noName $ spawn "~/scripts/setWallpaper.sh '8'")
    , ("M-w 0"                 , noName $ spawn "~/scripts/setWallpaper.sh '9'")
    -- Notifications
    , ("M-n"                   , noName $ spawn "dunstctl history-pop")
    , ("M-S-n"                 , noName $ spawn "dunstctl close")
    -- Multimedia Keys
    , ("<XF86AudioLowerVolume>", noName $ spawn "~/scripts/volume.sh down 5")
    , ("<XF86AudioRaiseVolume>", noName $ spawn "~/scripts/volume.sh up 5")
    , ("<XF86AudioMute>"       , noName $ spawn "~/scripts/volume.sh mute")
    , ("<XF86MonBrightnessUp>" , noName $ spawn "~/scripts/backlight.sh up 10")
    , ( "<XF86MonBrightnessDown>"
      , noName $ spawn "~/scripts/backlight.sh down 10"
      )
    , ("C-<XF86MonBrightnessUp>"  , noName $ spawn "blugon --setcurrent='+600'")
    , ("C-<XF86MonBrightnessDown>", noName $ spawn "blugon --setcurrent='-600'")
    , ("<XF86HomePage>"           , noName $ spawn myBrowser)
    , ("<Print>"                  , noName $ spawn "flameshot full -c")
    , ("M-<Print>"                , noName $ spawn "flameshot gui")
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
  xmonad
    $ addDescrKeys ((mod4Mask, xK_F1), showKeybindings) myKeys
    $ ewmh
    $ docks
    $ def
        { manageHook         = myManageHook <+> manageDocks
        , modMask            = myModMask
        , terminal           = myTerminal
        , startupHook        = myStartupHook
        , layoutHook         = showWName' myShowWNameTheme myLayoutHook
        , workspaces         = myWorkspaces
        , borderWidth        = myBorderWidth
        , normalBorderColor  = myNormColor
        , focusedBorderColor = myFocusColor
        , logHook            = myLogHook <+> dynamicLogWithPP
          (filterOutWsPP [scratchpadWorkspaceTag] $ xmobarPP
            { ppOutput = \x -> hPutStrLn xmproc0 x >> hPutStrLn xmproc1 x
            , ppCurrent         = xmobarColor "#00d1ff" "" . clickable
            , ppVisible         = xmobarColor "#c792ea" "" . clickable
            , ppHidden          = xmobarColor "#3fd12e" "" . clickable
            , ppHiddenNoWindows = xmobarColor "#ffaa00" "" . clickable
            , ppUrgent          = xmobarColor "#C45500" "" . clickable
            , ppSep             = "<fc=#ffffff> <fn=1>|</fn> </fc>"
            , ppWsSep           = " "
            , ppExtras          = [windowCount]
            , ppOrder           = \(ws : l : t : ex) -> [l, ws] ++ ex
            }
          )
        }
