module XMobar (xmobar) where

import Data.List
import Text.Printf

xmobarComParameters :: [String] -> String
xmobarComParameters [] = "[]"
xmobarComParameters c = printf "[\"%s\"]" (intercalate "\", \"" c)

xmobarStdin :: String
xmobarStdin = "Run StdinReader"

xmobarCommands :: [String] -> String
xmobarCommands c = printf "--commands='[%s]'" (intercalate ", " c)

xmobarBattery :: Integer -> String
xmobarBattery refreshRate = unwords
    [ "Run BatteryP"
    , xmobarComParameters [ "BAT0" ]
    , xmobarComParameters [ "--template", "⚡ <left>% (<timeleft>)<acstatus>"
                          , "--Low", show 15
                          , "--low", "red"
                          , "--"
                          , "-f", "AC0/online"
                          , "-i", ""
                          , "-o", ""
                          , "-O", " <fc=yellow>🔌</fc>"
                          ]
    , show refreshRate
    ]

xmobarMemory :: Integer -> String
xmobarMemory refreshRate = unwords
    [ "Run Memory"
    , xmobarComParameters [ "--template", "🗈 <usedratio>%" ]
    , show refreshRate
    ]

xmobarDate :: Integer -> String
xmobarDate refreshRate = "Run Date \"<fc=#ee9a00>%a %b %_d %l:%M</fc>\" \"date\" " ++
                         show refreshRate

xmobarWeather :: String -> Integer -> String
xmobarWeather station refreshRate = unwords
    [ "Run Weather"
    , "\"" ++ station ++ "\""
    , xmobarComParameters [ "--template", "⛅ <tempF>°F"
                          , "--Low", show 64
                          , "--High", show 77
                          , "--normal", "green"
                          , "--high", "red"
                          , "--low", "lightblue"
                          ]
    , show refreshRate
    ]

xmobarCpu :: Integer -> String
xmobarCpu refreshRate = unwords
    [ "Run Cpu"
    , xmobarComParameters [ "--template", "🖳 <total>%"
                          , "--Low", show 3
                          , "--High", show 50
                          , "--normal", "green"
                          , "--high", "red"
                          ]
    , show refreshRate
    ]

xmobarDiskU :: Integer -> String
xmobarDiskU refreshRate = unwords
    [ "Run DiskU"
    , "[(\"/\", \"<used>/<size>\"), (\"/home\", \"<used>/<size>\")]"
    , xmobarComParameters [ "--Low", show 20
                          , "--High", show 50
                          , "-m", show 1
                          , "-p", show 3
                          ]
    , show refreshRate
    ]

xmobarWireless :: String -> Integer -> String
xmobarWireless interface refreshRate = unwords
    [ "Run Wireless"
    , "\"" ++ interface ++ "\""
    , xmobarComParameters [ "--template", "📶 <essid> <quality>" ]
    , show refreshRate
    ]

xmobarVolume :: String -> String -> Integer -> String
xmobarVolume mixer element refreshRate = unwords
    [ "Run Volume"
    , "\"" ++ mixer ++ "\""
    , "\"" ++ element ++ "\""
    , xmobarComParameters [ "--template", "<status> <volume>%"
                          , "--"
                          , "-O", "🔊"
                          , "-o", "🔇"
                          ]
    , show refreshRate
    ]

weatherStation :: String
weatherStation = "KSUS"

defaultCommands :: String -> [String]
defaultCommands wirelessInterface =
    [ xmobarStdin
    , xmobarWireless wirelessInterface 10
    , xmobarVolume "default" "Master" 10
    , xmobarCpu 10
    , xmobarMemory 10
    , xmobarDiskU 600
    , xmobarDate 10
    , xmobarWeather weatherStation 3600
    ]

leftTemplate :: String
leftTemplate = "%StdinReader% }{ "

rightTemplate interface =
    "%" ++ interface ++ "wi% | %default:Master% | %cpu% | %memory% | " ++
    "%disku%     %date% | %" ++ weatherStation ++ "%"

templateParameter :: String -> String
templateParameter template = printf " -t '%s'" template

xmobarTemplate :: String -> String
xmobarTemplate "apollo" =
    xmobarCommands (defaultCommands interface)
    ++ templateParameter (leftTemplate ++ rightTemplate interface)
  where
    interface = "enp0s20u3"

xmobarTemplate "dionysus" =
    xmobarCommands (defaultCommands interface ++ [xmobarBattery 60])
    ++ templateParameter (leftTemplate ++ " %battery% | " ++
                          rightTemplate interface)
  where
    interface = "wlp3s0"

xmobarTemplate _ = ""

xmobarLook :: String
xmobarLook = unwords
    [ "--font='xft:Terminus (TTF),DejaVu Sans Mono,Symbola'"
    , "--bgcolor='#1c1b1a'"
    , "--fgcolor='#ffffff'"
    , "--bottom"
    ]

xmobarParameters :: String -> String
xmobarParameters hostname = unwords [xmobarLook, xmobarTemplate hostname]

xmobar :: String -> String
xmobar hostname =
    unwords ["xmobar", xmobarLook, (xmobarParameters hostname)]
