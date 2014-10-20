Config { font = "xft:Terminus (TTF),DejaVu Sans Mono,Symbola"
       , bgColor = "#1c1b1a"
       , fgColor = "#ffffff"
       , position = Bottom
       , commands = [ Run Weather "KONT"
                                  ["-t","⛅ <tempF>°F",
                                   "-L", "64", "-H", "77",
                                    "--normal", "green", "--high", "red",
                                    "--low","lightblue"] 36000
                    , Run Cpu ["-t", "🖳 <total>%", " -L", "3", "-H", "50",
                               "--normal", "green", "--high", "red"] 10
                    , Run Memory ["-t", "🗈 <usedratio>%"] 10
                    , Run Date "%a %b %_d %l:%M" "date" 10
                    , Run BatteryP ["BAT0"]
                                   ["-t", "⚡ <left>% (<timeleft>)<acstatus>",
                                   "-L", "15", "-l", "red",
                                   "--", "-f", "AC0/online",
                                   "-O", " <fc=yellow>🔌</fc>",
                                   "-l", "red",
                                   "-i", "", "-o", ""] 10
                    , Run Wireless "wlp3s0" ["-t", "📶 <essid> <quality>"] 10
                    , Run Volume "default" "Master"
                                 ["-t", "<status> <volume>%",
                                  "--", "-O", "🔊", "-o", "🔇"] 10
                    , Run StdinReader
                    ]
       , sepChar = "%"
       , alignSep = "}{"
       , template = "%StdinReader% }{ %wlp3s0wi% | %battery% | %default:Master% | %cpu% | %memory%     <fc=#ee9a00>%date%</fc> | %KONT%"
       }
