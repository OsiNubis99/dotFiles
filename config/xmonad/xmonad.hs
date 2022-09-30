import Data.Char
import qualified Data.Map as M
import Data.Maybe
import Data.Monoid
import Data.Tree
import System.Directory
import System.Exit
import System.IO
import XMonad
import XMonad.Actions.CopyWindow
import XMonad.Actions.CycleWS
import XMonad.Actions.GridSelect
import XMonad.Actions.MouseResize
import XMonad.Actions.Promote
import XMonad.Actions.RotSlaves
import qualified XMonad.Actions.Search as S
import XMonad.Actions.WindowGo
import XMonad.Actions.WithAll
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.FadeInactive
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.ServerMode
import XMonad.Hooks.SetWMName
import XMonad.Hooks.StatusBar
import XMonad.Hooks.StatusBar.PP
import XMonad.Hooks.WindowSwallowing
import XMonad.Hooks.WorkspaceHistory
import XMonad.Layout.Accordion
import XMonad.Layout.GridVariants
import XMonad.Layout.LayoutModifier
import XMonad.Layout.LimitWindows
import XMonad.Layout.MultiToggle
import qualified XMonad.Layout.MultiToggle as MT
import XMonad.Layout.MultiToggle.Instances
import XMonad.Layout.NoBorders
import XMonad.Layout.PerWorkspace
import XMonad.Layout.Reflect
import XMonad.Layout.Renamed
import XMonad.Layout.ResizableTile
import XMonad.Layout.ShowWName
import XMonad.Layout.Simplest
import XMonad.Layout.SimplestFloat
import XMonad.Layout.Spacing
import XMonad.Layout.Spiral
import XMonad.Layout.SubLayouts
import XMonad.Layout.Tabbed
import XMonad.Layout.ThreeColumns
import qualified XMonad.Layout.ToggleLayouts as T
import XMonad.Layout.WindowArranger
import XMonad.Layout.WindowNavigation
import qualified XMonad.StackSet as W
import XMonad.Util.Dmenu
import XMonad.Util.EZConfig
import XMonad.Util.NamedActions
import XMonad.Util.NamedScratchpad
import XMonad.Util.Run (spawnPipe)
import XMonad.Util.SpawnOnce

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
  spawnOnce "cmst -m &"
  spawnOnce "qlipper &"
  spawnOnce "dunst -config ~/.config/dunst/dunstrc &"
  spawnOnce "conky &"
  spawnOnce "conky -c .config/conky/conky2.conf &"
  spawnOnce "blugon &"

myNavigation :: TwoD a (Maybe a)
myNavigation = makeXEventhandler $ shadowWithKeymap navKeyMap navDefaultHandler
  where
    navKeyMap =
      M.fromList
        [ ((0, xK_Escape), cancel),
          ((0, xK_Return), select),
          ((0, xK_slash), substringSearch myNavigation),
          ((0, xK_Left), move (-1, 0) >> myNavigation),
          ((0, xK_h), move (-1, 0) >> myNavigation),
          ((0, xK_Right), move (1, 0) >> myNavigation),
          ((0, xK_l), move (1, 0) >> myNavigation),
          ((0, xK_Down), move (0, 1) >> myNavigation),
          ((0, xK_j), move (0, 1) >> myNavigation),
          ((0, xK_Up), move (0, -1) >> myNavigation),
          ((0, xK_k), move (0, -1) >> myNavigation),
          ((0, xK_space), setPos (0, 0) >> myNavigation)
        ]
    navDefaultHandler = const myNavigation

myColorizer :: Window -> Bool -> X (String, String)
myColorizer =
  colorRangeFromClassName
    (0x28, 0x2c, 0x34) -- lowest inactive bg
    (0x28, 0x2c, 0x34) -- highest inactive bg
    (0xc7, 0x92, 0xea) -- active bg
    (0xc0, 0xa7, 0x9a) -- inactive fg
    (0x28, 0x2c, 0x34) -- active fg

-- gridSelect menu layout
mygridConfig :: p -> GSConfig Window
mygridConfig colorizer =
  (buildDefaultGSConfig myColorizer)
    { gs_cellheight = 40,
      gs_cellwidth = 200,
      gs_cellpadding = 6,
      gs_navigate = myNavigation,
      gs_originFractX = 0.5,
      gs_originFractY = 0.5,
      gs_font = myFont
    }

spawnSelected' :: [(String, String)] -> X ()
spawnSelected' lst = gridselect conf lst >>= flip whenJust spawn
  where
    conf =
      def
        { gs_cellheight = 40,
          gs_cellwidth = 180,
          gs_cellpadding = 6,
          gs_originFractX = 0.5,
          gs_originFractY = 0.5,
          gs_font = myFont
        }

runSelectedAction' :: GSConfig (X ()) -> [(String, X ())] -> X ()
runSelectedAction' conf actions = do
  selectedActionM <- gridselect conf actions
  case selectedActionM of
    Just selectedAction -> selectedAction
    Nothing -> return ()

gsApps =
  [ ("0 A.D.", "0ad"),
    ("Steam", "steam"),
    ("Firefox", "firefox"),
    ("Telegram", "telegram-desktop"),
    ("Files", "thunar"),
    ("VLC", "vlc"),
    ("Discord", "discord"),
    ("Htop", myTerminal ++ " -e htop"),
    ("Postman", "postman"),
    ("VirtualBox", "virtualbox"),
    ("Settings", "xfce4-settings-manager"),
    ("Nitrogen", "nitrogen"),
    ("Beekeeper", "beekeeper-studio")
  ]

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
      className =? "steam_app_239140" --> doCenterFloat <+> doShift (myWorkspaces !! 5),
      resource =? "Dialog" --> doFloat,
      title =? "Oracle VM VirtualBox Manager" --> doFloat,
      title =? "File Operation Progress" --> doFloat,
      className =? "confirm" --> doFloat,
      className =? "file_progress" --> doFloat,
      className =? "dialog" --> doFloat,
      className =? "download" --> doFloat,
      className =? "error" --> doFloat,
      className =? "Gimp" --> doFloat,
      className =? "notification" --> doFloat,
      className =? "pinentry-gtk-2" --> doFloat,
      className =? "splash" --> doFloat,
      className =? "toolbar" --> doFloat,
      className =? "Yad" --> doCenterFloat,
      className =? "Nm-connection-editor" --> doFloat,
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
    ("M-o", spawn "~/dotFiles/scripts/spawnRofi.sh"),
    ("M-x", spawn "~/dotFiles/scripts/spawnTrayer.sh"),
    ("M-z", spawn myTerminal),
    -- Workspaces
    ("M-h", prevScreen),
    ("M-l", nextScreen),
    ("M-<Left>", prevScreen),
    ("M-<Right>", nextScreen),
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
    -- Grid Select
    ("M-<Tab>", spawnSelected' $ gsApps),
    ("M-t", goToSelected $ mygridConfig myColorizer),
    ("M-g", bringSelected $ mygridConfig myColorizer),
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
  xmproc0 <- spawnPipe "xmobar -x 0 ~/.config/xmobar/xmobarrc -p 'Static { xpos = 0 , ypos = 0, width = 1920, height = 30 }'"
  xmproc1 <- spawnPipe "xmobar -x 1 ~/.config/xmobar/xmobarrc -p 'Static { xpos = 1920 , ypos = 0, width = 1920, height = 30 }'"
  xmonad $
    ewmh $
      docks $
        def
          { manageHook = myManageHook <+> manageDocks,
            handleEventHook = swallowEventHook (className =? "Alacritty") (return True),
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
                    { ppOutput = \x ->
                        hPutStrLn xmproc0 x
                          >> hPutStrLn xmproc1 x,
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
