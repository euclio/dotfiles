{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

import Data.Monoid
import GHC.IO.Handle.Types

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

myTerminal :: String
myTerminal              = "urxvt -e fish"

myNormalBorderColor :: String
myNormalBorderColor     = "#d0d0d0"

myFocusedBorderColor :: String
myFocusedBorderColor    = "#ffa500"

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
        [ "crx_nckgahadagoaajjgafhacjanaoiihapd" -- Chrome Hangouts Extension
        ]

chatPlacement :: Placement
chatPlacement = withGaps (0, 20, 20, 0) (inBounds (smart (1, 1)))

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
        }
        `additionalKeysP`
        [ ("M-S l", spawn "xlock")
        ]
