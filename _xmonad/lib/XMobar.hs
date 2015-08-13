module XMobar (xmobarCommand) where

import Data.List

xmobarComParameters :: [String] -> String
xmobarComParameters [] = " [] "
xmobarComParameters c = " [\"" ++ intercalate "\",\"" c ++ "\"] "

xmobarStdin :: String
xmobarStdin = "Run StdinReader"

xmobarCommands :: [String] -> String
xmobarCommands c = "--commands=\'[" ++ intercalate ", " c ++ "]\'"

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
xmobarDate refreshRate = "Run Date \"%a %b %_d %l:%M\" \"date\" " ++
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

xmobarTemplate :: String -> String
xmobarTemplate _ =
    xmobarCommands [ xmobarStdin
                   , xmobarWireless "wlp3s0" 10
                   , xmobarBattery 60
                   , xmobarVolume "default" "Master" 10
                   , xmobarCpu 10
                   , xmobarMemory 10
                   , xmobarDiskU 600
                   , xmobarDate 10
                   , xmobarWeather "KONT" 3600
                   ]
    ++ " -t \'%StdinReader% }{ %wlp3s0wi% | %battery% | %default:Master% | %cpu% | %memory% | %disku%    <fc=#ee9a00>%date%</fc> | %KONT%\'"

xmobarLook :: String
xmobarLook = unwords
    [ "--font='xft:Terminus (TTF),DejaVu Sans Mono,Symbola'"
    , "--bgcolor='#1c1b1a'"
    , "--fgcolor='#ffffff'"
    , "--bottom"
    ]

xmobarParameters :: String -> String
xmobarParameters hostname = unwords [xmobarLook, xmobarTemplate hostname]

xmobarCommand :: String -> String
xmobarCommand hostname =
    unwords ["xmobar", xmobarLook, (xmobarParameters hostname)]
