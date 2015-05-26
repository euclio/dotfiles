import Control.Monad
import Data.Char
import Data.Monoid
import GHC.IO.Handle.Types
import System.FilePath

import XMonad
import XMonad.Actions.SpawnOn
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.Place
import XMonad.Hooks.SetWMName
import XMonad.Hooks.UrgencyHook
import XMonad.Layout.LayoutModifier
import XMonad.Layout.NoBorders
import XMonad.Prompt
import XMonad.Prompt.Shell
import XMonad.StackSet
import XMonad.Util.EZConfig
import XMonad.Util.Run
import XMonad.Util.Scratchpad

myTerminal :: String
myTerminal              = "urxvt -e fish"

myNormalBorderColor :: String
myNormalBorderColor     = "#393939"

myFocusedBorderColor :: String
myFocusedBorderColor    = "#ffa500"

myLayoutHook :: ModifiedLayout SmartBorder (ModifiedLayout AvoidStruts (Choose Tall (Choose (Mirror Tall) Full))) Window
myLayoutHook            = smartBorders $ avoidStruts $
                          layoutHook defaultConfig

myManageHook :: ManageHook
myManageHook = composeAll . concat $
    [ [ isFullscreen --> doFullFloat ]
    , [ appName =? c --> (placeHook chatPlacement <+> doFloat) | c <- myChatApps ]
    , [ manageDocks ]
    , [ manageScratchpad ]
    ]
  where
    myChatApps =
        [ "crx_nckgahadagoaajjgafhacjanaoiihapd" -- Hangouts Chrome Extension
        , "crx_knipolnnllmklapflnccelgolnpehhpl" -- Hangouts Chrome App
        ]

manageScratchpad :: ManageHook
manageScratchpad = scratchpadManageHook (RationalRect l t w h)
  where
    h = 0.1
    w = 1
    t = 0
    l = 1 - w

chatPlacement :: Placement
chatPlacement = withGaps (0, 0, 300, 0) (inBounds (smart (1, 1)))

myLogHook :: Handle -> X()
myLogHook h = dynamicLogWithPP $ xmobarPP
    {   ppOutput = hPutStrLn h
    ,   ppOrder = \(ws:_:t:_) -> [ws,t]
    ,   ppUrgent = xmobarColor "yellow" "red"
    ,   ppHidden = noScratchpad
    }
  where
    noScratchpad ws = if ws == "NSP" then "" else ws

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

myStartupHook :: X ()
myStartupHook = do
    myBrowser <- liftIO getBrowser
    -- Spawn the default browser. However, since browsers are 'factory'
    -- applications, we can't actually specify which workspace it should appear
    -- on. Thus, it will appear on whichever workspace we happen to be on when
    -- the window is created.
    spawnOn "1" myBrowser
    setWMName "LG3D"

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
        , manageHook         = manageHook defaultConfig
                                    <+> manageSpawn
                                    <+> myManageHook
        , layoutHook         = myLayoutHook
        , logHook            = myLogHook h
        , startupHook        = myStartupHook
        }
        `additionalKeysP`
        -- Lock PC
        [ ("M-S-l", safeSpawnProg "xlock")
        -- Take screenshot
        , ("M-s", liftIO (screenshotCommand []) >>= spawn)
        -- Take screenshot (with selection)
        , ("M-S-s", liftIO (screenshotCommand ["-s"]) >>= spawn)
        -- Open screen management
        , ("M-S-r", safeSpawnProg "lxrandr")
        -- Open terminal file manager
        , ("M-f", safeSpawn "urxvt" ["-e", "fish", "-c", "ranger"])
        -- Open graphical file manager
        , ("M-S-f", unsafeSpawn "xdg-open ~")
        -- Open scratchpad terminal
        , ("M-`", scratchpadSpawnActionCustom ("urxvt -name scratchpad -e fish"))
        , ("<XF86KbdBrightnessUp>", safeSpawn "asus-kbd-backlight" ["up"])
        , ("<XF86KbdBrightnessDown>", safeSpawn "asus-kbd-backlight" ["down"])
        ]

-- Strips trailing whitespace from a string
rstrip :: String -> String
rstrip = reverse . dropWhile isSpace . reverse
