module Custom.MyDecorations where

import Custom.MyTheme
import XMonad
import XMonad.Actions.EasyMotion
import XMonad.Layout.ShowWName
import XMonad.Layout.Tabbed
import XMonad.Prompt

myBorderWidth :: XMonad.Dimension
myBorderWidth = 2

myNormalBorderColor :: String
myNormalBorderColor = bgDark

myFocusedBorderColor :: String
myFocusedBorderColor = focus1

myPromptConfig :: XPConfig
myPromptConfig =
  def
    { bgColor = bgDark,
      fgColor = fgWhite,
      bgHLight = bgWhite,
      fgHLight = fgDark,
      historySize = 0,
      position = Top,
      borderColor = bgDark,
      promptBorderWidth = 0,
      defaultText = "",
      alwaysHighlight = True,
      height = 60,
      font = myFont,
      autoComplete = Nothing,
      showCompletionOnTab = False
    }

myShowWNameConfig :: SWNConfig
myShowWNameConfig =
  def
    { swn_font = myBigFont,
      swn_fade = 0.8,
      swn_bgcolor = bgDark,
      swn_color = focus1
    }

myTabConfig :: Theme
myTabConfig =
  def
    { activeColor = focus1,
      inactiveColor = bgDark,
      urgentColor = peachRed,
      activeBorderColor = focus1,
      inactiveBorderColor = bgDark,
      urgentBorderColor = peachRed,
      activeTextColor = fgDark,
      inactiveTextColor = fgWhite,
      urgentTextColor = fgDark,
      fontName = myFont
    }

emConf :: EasyMotionConfig
emConf =
  def
    { txtCol = fgWhite,
      bgCol = bgDark,
      borderCol = sumiInk0,
      cancelKey = xK_Escape,
      emFont = myBigFont,
      overlayF = textSize,
      borderPx = 30
    }
