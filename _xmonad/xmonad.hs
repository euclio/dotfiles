import XMonad

main = do
    xmonad $ defaultConfig
        { terminal           = myTerminal
        , modMask            = mod4Mask
        , normalBorderColor  = myNormalBorderColor
        , focusedBorderColor = myFocusedBorderColor
        }

myTerminal = "urxvt -sl 0 -e bash -c \"tmux new-session\""
myNormalBorderColor = "#d0d0d0"
myFocusedBorderColor = "#ffa500"
