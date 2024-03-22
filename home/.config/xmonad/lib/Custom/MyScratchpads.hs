module Custom.MyScratchpads where

import XMonad
import XMonad.ManageHook ((=?))
import qualified XMonad.StackSet as W
import XMonad.Util.NamedScratchpad

myScratchpads :: [NamedScratchpad]
myScratchpads =
  [ NS
      "terminal"
      "warp-terminal"
      (className =? "dev.warp.Warp")
      (customFloating cascadeFloating),
    NS
      "telegram"
      "telegram-desktop"
      (className =? "TelegramDesktop")
      (customFloating bigCenterFloating)
  ]
  where
    cascadeFloating = W.RationalRect fromLeft fromTop width height
      where
        fromTop = 1 / 24
        fromLeft = 0
        width = 1
        height = 7 / 10
    bigCenterFloating = W.RationalRect fromLeft fromTop width height
      where
        fromTop = (1 - height) / 2
        fromLeft = (1 - width) / 2
        height = 5 / 6
        width = 5 / 6
