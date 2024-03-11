module Custom.MyLayouts where

import Custom.MyDecorations
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

type MyFull =
  ModifiedLayout
    Rename
    (ModifiedLayout Spacing (ModifiedLayout WithBorder Full))

type MySplit =
  ModifiedLayout
    Rename
    (ModifiedLayout LimitWindows (ModifiedLayout Spacing ResizableTall))

type MyTabs =
  ModifiedLayout
    Rename
    ( ModifiedLayout
        Spacing
        ( ModifiedLayout
            (Decoration TabbedDecoration DefaultShrinker)
            Simplest
        )
    )

myLayoutHook ::
  MultiToggle
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
  mkToggle (NBFULL ?? NOBORDERS ?? EOT) $
    onWorkspace (myWorkspaces !! 2) fullDefault $
      onWorkspaces
        [head myWorkspaces, myWorkspaces !! 1]
        tabsDefault
        splitDefault
  where
    fullDefault = full ||| split ||| tabs
    tabsDefault = tabs ||| full ||| split
    splitDefault = split ||| tabs ||| full
    tabs =
      renamed [Replace "T"] $
        spacingRaw False (Border (45 + 8) 8 8 8) True (Border 0 0 0 0) True $
          tabbed shrinkText myTabConfig
    split =
      renamed [Replace "S"] $
        limitWindows 12 $
          spacingRaw False (Border (45 + 4) 4 4 4) True (Border 4 4 4 4) True $
            ResizableTall 1 (1 / 100) (1 / 2) []
    full =
      renamed [Replace "F"] $
        spacingRaw False (Border 45 0 0 0) True (Border 0 0 0 0) True $
          noBorders Full
