import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.ManageDocks
import XMonad.Layout.NoBorders

main = xmonad =<< xmobar myConfig

myConfig = defaultConfig
    { terminal           = myTerminal
    , modMask            = mod4Mask
    , normalBorderColor  = myNormalBorderColor
    , focusedBorderColor = myFocusedBorderColor
    , handleEventHook    = fullscreenEventHook
    , manageHook         = myManageHook
    , layoutHook         = myLayoutHook
    }

myTerminal = "urxvt"
myNormalBorderColor = "#d0d0d0"
myFocusedBorderColor = "#ffa500"
myLayoutHook = smartBorders $ layoutHook defaultConfig
myManageHook = (isFullscreen --> doFullFloat) <+> manageHook defaultConfig <+> manageDocks
