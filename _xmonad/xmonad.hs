import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.Place
import XMonad.Hooks.UrgencyHook
import XMonad.Layout.NoBorders
import XMonad.Util.EZConfig
import XMonad.Util.Paste
import XMonad.Util.Run

myTerminal              = "urxvt -e fish"
myNormalBorderColor     = "#d0d0d0"
myFocusedBorderColor    = "#ffa500"

myLayoutHook            = smartBorders $ avoidStruts $
                          layoutHook defaultConfig


myManageHook = composeAll . concat $
    [ [ isFullscreen --> doFullFloat ]
    , [ appName =? c --> (placeHook chatPlacement <+> doFloat) | c <- myChatApps ]
    , [ manageDocks ]
    ]
  where
    myChatApps =
        [ "crx_nckgahadagoaajjgafhacjanaoiihapd" -- Chrome Hangouts Extension
        ]

chatPlacement = withGaps (0, 0, 22, 0) (inBounds (smart (1, 1)))

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
        , manageHook         = myManageHook <+> manageHook defaultConfig
        , layoutHook         = myLayoutHook
        , logHook            = myLogHook h
        }
        `additionalKeys`
        [ ((0, xK_Insert), pasteSelection)
        ]
