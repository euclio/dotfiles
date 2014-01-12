import XMonad
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageHelpers

main = do
    xmonad $ defaultConfig
        { terminal           = myTerminal
        , modMask            = mod4Mask
        , normalBorderColor  = myNormalBorderColor
        , focusedBorderColor = myFocusedBorderColor
        , handleEventHook    = fullscreenEventHook
        , manageHook         = myManageHook
        }

myTerminal = "urxvt -sl 0 -e bash -c \"tmux new-session\""
myNormalBorderColor = "#d0d0d0"
myFocusedBorderColor = "#ffa500"

myManageHook = isFullscreen --> doFullFloat

