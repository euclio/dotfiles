import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.UrgencyHook
import XMonad.Layout.NoBorders
import XMonad.Util.Run

myTerminal              = "urxvt -e fish"
myNormalBorderColor     = "#d0d0d0"
myFocusedBorderColor    = "#ffa500"

myLayoutHook            = smartBorders $ avoidStruts $
                          layoutHook defaultConfig

myManageHook = (isFullscreen --> doFullFloat) <+> manageHook defaultConfig <+> manageDocks

myLogHook h = dynamicLogWithPP $ xmobarPP
    {   ppOutput = hPutStrLn h
    ,   ppOrder = \(ws:_:t:_) -> [ws,t]
    ,   ppUrgent = xmobarColor "yellow" "red"
    }

myUrgentConfig = UrgencyConfig
    {   suppressWhen = OnScreen
    }

main :: IO()
main = do
    h <- spawnPipe "xmobar ~/.xmonad/xmobar.hs"
    xmonad $ withUrgencyHookC NoUrgencyHook myUrgentConfig
        defaultConfig
        { terminal           = myTerminal
        , modMask            = mod4Mask
        , normalBorderColor  = myNormalBorderColor
        , focusedBorderColor = myFocusedBorderColor
        , handleEventHook    = fullscreenEventHook
        , manageHook         = myManageHook
        , layoutHook         = myLayoutHook
        , logHook            = myLogHook h
        }
