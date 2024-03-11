module Custom.MyManagement where

import Custom.MyScratchpads
import Custom.MyWorkspaces
import Data.Char
import Data.List
import qualified Data.Map as M
import Data.Maybe
import Data.Monoid
import System.IO
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
import qualified XMonad.StackSet as W
import XMonad.Util.EZConfig
import XMonad.Util.NamedActions
import XMonad.Util.NamedScratchpad
import XMonad.Util.Run
import XMonad.Util.SpawnOnce

myManageHook :: XMonad.Query (Data.Monoid.Endo WindowSet)
myManageHook =
  composeAll
    . concat
    $ [ [className =? c --> doFloat | c <- myFloats],
        [title =? t --> doFloat | t <- myOtherFloats],
        [className =? c --> doShift (head myWorkspaces) | c <- apps0],
        [className =? c --> doShift (myWorkspaces !! 1) | c <- apps1],
        [className =? c --> doShift (myWorkspaces !! 2) | c <- apps2],
        [className =? c --> doShift (myWorkspaces !! 3) | c <- apps3],
        [ stringProperty "WM_CLASS"
            =? "steam_app"
            --> doShift
              (myWorkspaces !! 3),
          resource =? "Dialog" --> doFloat,
          isFullscreen --> doFullFloat,
          namedScratchpadManageHook myScratchpads
        ]
      ]
  where
    apps0 = ["Code", "Emacs"]
    apps1 = ["firefox", "Opera", "ticktick"]
    apps2 = ["steam", "discord"]
    apps3 = ["Thunar", "libreoffice"]
    myFloats =
      [ "confirm",
        "file_progress",
        "dialog",
        "download",
        "error",
        "Gimp",
        "notification",
        "pinentry-gtk-2",
        "splash",
        "toolbar",
        "Nm-connection-editor",
        "Yad"
      ]
    myOtherFloats = ["alsamixer"]

myLogHook :: X ()
myLogHook =
  fadeInactiveLogHook fadeAmount
    <+> refocusLastLogHook
    >> nsHideOnFocusLoss myScratchpads
  where
    fadeAmount = 1.0
