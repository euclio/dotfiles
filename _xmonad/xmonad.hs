import Control.Monad
import Data.Char
import Data.Monoid
import GHC.IO.Handle.Types
import System.FilePath

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

-- Returns the directory where screenshots should be stored. Currently stores
-- screenshots in a subdirectory of the $XDG_PICTURES_DIR.
screenshotDirectory :: IO String
screenshotDirectory = do
    -- TODO: Do this at compile time
    xdgPicturesDir <- liftM rstrip (runProcessWithInput "xdg-user-dir" ["PICTURES"] [])
    return $ xdgPicturesDir </> "screenshot"

-- Retrieves the current date and time to create a filename for a screenshot.
screenshotDateFormat :: IO String
screenshotDateFormat =
    liftM rstrip (runProcessWithInput "date" [dateFormat] [])
  where
    dateFormat :: String
    dateFormat = "+%F-%T"

-- Returns the command to use to create a screenshot in the correct directory.
screenshotCommand :: [String] -> IO String
screenshotCommand extraArgs = do
    dateFormat <- screenshotDateFormat
    directory <- screenshotDirectory
    let fileName = directory </> dateFormat <.> "png"
    return $ "maim" ++ " " ++ unwords extraArgs ++ " " ++ fileName

main :: IO ()
main = do
    h <- spawnPipe "xmobar ~/.xmonad/xmobar.hs"
    xmonad $ ewmh $ withUrgencyHookC NoUrgencyHook myUrgentConfig
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
        , ("M-s", liftIO (screenshotCommand []) >>= spawn)
        , ("M-S-s", liftIO (screenshotCommand ["-s"]) >>= spawn)
        , ("M-S-r", spawn "lxrandr")
        -- Unfortunately, ASUS laptops don't report the screen brightness
        -- function keys to X. Thus, we have to make up new key combinations to
        -- change the screen brightness.
        , ("M-<F5>", spawn "asus-screen-brightness down")
        , ("M-<F6>", spawn "asus-screen-brightness up")
        ]

-- Strips trailing whitespace from a string
rstrip :: String -> String
rstrip = reverse . dropWhile isSpace . reverse
