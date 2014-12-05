import Data.Monoid
import GHC.IO.Handle.Types

import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.Place
import XMonad.Hooks.SetWMName
import XMonad.Hooks.UrgencyHook
import XMonad.Layout.NoBorders
import XMonad.Layout.LayoutModifier
import XMonad.Util.EZConfig
import XMonad.Util.Run

myTerminal :: String
myTerminal              = "urxvtc -e fish"

myNormalBorderColor :: String
myNormalBorderColor     = "#393939"

myFocusedBorderColor :: String
myFocusedBorderColor    = "#ffa500"

myLayoutHook :: ModifiedLayout SmartBorder (ModifiedLayout AvoidStruts (Choose Tall (Choose (Mirror Tall) Full))) Window
myLayoutHook            = smartBorders $ avoidStruts $
                          layoutHook defaultConfig

myManageHook :: Query (Endo WindowSet)
myManageHook = composeAll . concat $
    [ [ isFullscreen --> doFullFloat ]
    , [ appName =? c --> (placeHook chatPlacement <+> doFloat) | c <- myChatApps ]
    , [ manageDocks ]
    ]
  where
    myChatApps =
        [ "crx_nckgahadagoaajjgafhacjanaoiihapd" -- Hangouts Chrome Extension
        , "crx_knipolnnllmklapflnccelgolnpehhpl" -- Hangouts Chrome App
        ]

chatPlacement :: Placement
chatPlacement = withGaps (0, 0, 300, 0) (inBounds (smart (1, 1)))

myLogHook :: Handle -> X()
myLogHook h = dynamicLogWithPP $ xmobarPP
    {   ppOutput = hPutStrLn h
    ,   ppOrder = \(ws:_:t:_) -> [ws,t]
    ,   ppUrgent = xmobarColor "yellow" "red"
    }

myUrgentConfig :: UrgencyConfig
myUrgentConfig = UrgencyConfig
    {   suppressWhen = OnScreen
    ,   remindWhen   = Dont
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
        , startupHook        = setWMName "LG3D"
        }
        `additionalKeysP`
        [ ("M-S-l", spawn "xlock")
        , ("M-s", spawn "maim")
        , ("M-S-s", spawn "maim --select")
        , ("M-S-r", spawn "lxrandr")
        ]
