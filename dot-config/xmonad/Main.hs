import qualified Codec.Binary.UTF8.String as UTF8
import Data.Maybe
import qualified DBus as D
import qualified DBus.Client as D

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
import XMonad.Prompt.Shell
import qualified XMonad.StackSet as W
import XMonad.Util.EZConfig
import XMonad.Util.Run
import XMonad.Util.Scratchpad

import Lib

myTerminal :: [String]
myTerminal = ["termite", "-e", "zsh"]

myNormalBorderColor :: String
myNormalBorderColor     = "#393939"

myFocusedBorderColor :: String
myFocusedBorderColor    = "#fff700"

myLayoutHook :: ModifiedLayout SmartBorder (ModifiedLayout AvoidStruts (Choose Tall (Choose (Mirror Tall) Full))) Window
myLayoutHook            = smartBorders $ avoidStruts $
                          layoutHook def

-- The `hasProperty` and `isSplash` functions are inspired by
-- https://github.com/fancypantalons/XMonad-Config/blob/master/xmonad.hs

-- Returns a query which checks if the window has the given property.
hasProperty :: String -> Query Bool
hasProperty name = ask >>= \w -> liftX $ withDisplay $ queryFunc w
  where queryFunc window disp = do
          atom <- getAtom name

          prop8 <- io $ getWindowProperty8 disp atom window
          prop16 <- io $ getWindowProperty16 disp atom window
          prop32 <- io $ getWindowProperty32 disp atom window

          return $ isJust prop8 || isJust prop16 || isJust prop32

-- Use EWMH tags to determine if the window type in question is
-- a splash window or not (among others, this works for Gnome Do).
--
-- The second stanza is a special case for the Eclipse splash screen
-- which decided to do things differently...
isSplash :: Query Bool
isSplash =
  (isInProperty "_NET_WM_WINDOW_TYPE" "_NET_WM_WINDOW_TYPE_SPLASH") <||>
  ((hasProperty "_MOTIF_WM_HINTS") <&&> (className =? "Eclipse"))

myManageHook :: ManageHook
myManageHook = composeAll . concat $
    [ [ isFullscreen --> doFullFloat ]
    , [ isSplash --> doIgnore ]
    , [ appName =? c --> (placeHook chatPlacement <+> doFloat) | c <- myChatApps ]
    , [ appName =? c --> doFloat | c <- myFloatingApps ]
    , [ manageScratchpad ]
    ]
  where
    myChatApps =
        [
        ]
    myFloatingApps =
        [ "org.gamefolk.roomfullofcats.RoomFullOfCatsApp"
        ]

manageScratchpad :: ManageHook
manageScratchpad = scratchpadManageHook (W.RationalRect l t w h)
  where
    h = 0.1
    w = 1
    t = 0
    l = 1 - w

chatPlacement :: Placement
chatPlacement = withGaps (0, 0, 300, 0) (inBounds (smart (1, 1)))

myLogHook :: D.Client -> PP
myLogHook dbus = def
    {   ppOutput = dbusOutput dbus
    ,   ppCurrent = polybarColor "#ff0" "" . wrap "[" "]"
    ,   ppTitle = polybarColor "#0f0" ""
    ,   ppVisible = wrap "(" ")"
    ,   ppUrgent = polybarColor "yellow" "red"
    ,   ppOrder = \(ws:_:t:_) -> [ws,t]
    ,   ppHidden = noScratchpad
    }
  where
    noScratchpad ws = if ws == "NSP" then "" else ws

myUrgentConfig :: UrgencyConfig
myUrgentConfig = UrgencyConfig
    {   suppressWhen = OnScreen
    ,   remindWhen   = Dont
    }

-- Use polybar escape codes to output a string with given foreground and
-- background colors.
polybarColor :: String -> String -> String -> String
polybarColor fg bg = wrap t "%{B- F-}"
    where t = concat ["%{F", fg, if null bg then "" else "B" ++ bg, "}"]

dbusOutput :: D.Client -> String -> IO ()
dbusOutput dbus str = do
    let signal = (D.signal objectPath interfaceName memberName) {
            D.signalBody = [D.toVariant $ UTF8.decodeString str]
        }
    D.emit dbus signal
  where
    objectPath = D.objectPath_ "/org/xmonad/Log"
    interfaceName = D.interfaceName_ "org.xmonad.Log"
    memberName = D.memberName_ "Update"

myStartupHook :: X ()
myStartupHook = do
    myBrowser <- liftIO getBrowser
    -- Spawn the default browser. However, since browsers are 'factory'
    -- applications, we can't actually specify which workspace it should appear
    -- on. Thus, it will appear on whichever workspace we happen to be on when
    -- the window is created.
    spawnOn "1" myBrowser
    setWMName "LG3D"

-- By default, XMonad doesn't advertise that it supports fullscreen EMWH hints.
-- This is a modified version of the function that XMonad calls internally, with
-- "_NET_WM_STATE_FULLSCREEN" added to the list.
--
-- Taken from https://github.com/binarin/rc/blob/58cc3b28045eecc41cdc3b4e2051b38a70117717/.xmonad/xmonad.hs#L251-269
-- from this thread: https://www.reddit.com/r/xmonad/comments/77szad/cant_go_fullscreen_in_firefox_even_with_ewmh/
setFullscreenSupported :: X ()
setFullscreenSupported = withDisplay $ \dpy -> do
    r <- asks theRoot
    a <- getAtom "_NET_SUPPORTED"
    c <- getAtom "ATOM"
    supp <- mapM getAtom ["_NET_WM_STATE_HIDDEN"
                         ,"_NET_WM_STATE_FULLSCREEN"
                         ,"_NET_NUMBER_OF_DESKTOPS"
                         ,"_NET_CLIENT_LIST"
                         ,"_NET_CLIENT_LIST_STACKING"
                         ,"_NET_CURRENT_DESKTOP"
                         ,"_NET_DESKTOP_NAMES"
                         ,"_NET_ACTIVE_WINDOW"
                         ,"_NET_WM_DESKTOP"
                         ,"_NET_WM_STRUT"
                         ]
    io $ changeProperty32 dpy r a c propModeReplace (fmap fromIntegral supp)

myWorkspaces :: [String]
myWorkspaces = ["1", "2", "3", "4", "5", "6", "7", "8", "9"]

main :: IO ()
main = do
    dbus <- D.connectSession
    _ <- D.requestName dbus (D.busName_ "org.xmonad.Log")
        [D.nameAllowReplacement, D.nameReplaceExisting, D.nameDoNotQueue]
    xmonad $ docks $ ewmh $ withUrgencyHookC NoUrgencyHook myUrgentConfig
        def
        { terminal           = unwords myTerminal
        , modMask            = mod4Mask
        , workspaces         = myWorkspaces
        , normalBorderColor  = myNormalBorderColor
        , focusedBorderColor = myFocusedBorderColor
        , handleEventHook    = docksEventHook <+> fullscreenEventHook
        , manageHook         = manageHook def
                                    <+> manageSpawn
                                    <+> myManageHook
        , layoutHook         = myLayoutHook
        , logHook            = dynamicLogWithPP (myLogHook dbus)
        , startupHook        = myStartupHook <+> setFullscreenSupported
        }
        `additionalKeysP`
        -- Lock PC
        ([ ("M-S-l", safeSpawnProg "xlock")
        -- Open dmenu
        , ("M-p", safeSpawn "dmenu_run" ["-fn", "terminus (ttf)-9"])
        -- Take screenshot
        , ("M-s", liftIO captureScreen)
        -- Take screenshot (with selection)
        , ("M-S-s", liftIO captureScreenRegion)
        -- Open screen management
        , ("M-S-r", safeSpawnProg "lxrandr")
        -- Open terminal file manager
        , ("M-f", safeSpawn (head myTerminal) ["-e", "ranger"])
        -- Open graphical file manager
        , ("M-S-f", unsafeSpawn "xdg-open ~")
        -- Open browser
        , ("M-b", safeSpawn "xdg-open" ["https://about:blank"])
        -- Open scratchpad terminal
        , ("M-`", scratchpadSpawnActionCustom (unwords $ myTerminal ++ ["--name=scratchpad"]))
        , ("<XF86MonBrightnessUp>", safeSpawn "light" ["-A", "10"])
        , ("<XF86MonBrightnessDown>", safeSpawn "light" ["-U", "10"])
        ] ++
        -- Change multi-monitor "greedy view" to view, which is more intuitive.
        -- This snippet is taken from the Xmonad FAQ.
        [ (otherModMasks ++ "M-" ++ [key], action tag)
            | (tag, key) <- zip myWorkspaces "123456789"
            , (otherModMasks, action) <- [ ("", windows . W.view) -- was W.greedyView
                                            , ("S-", windows . W.shift)]
        ])
