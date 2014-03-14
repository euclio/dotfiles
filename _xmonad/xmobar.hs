Config { font = "-*-Fixed-Bold-R-Normal-*-13-*-*-*-*-*-*-*"
       , bgColor = "#1c1b1a"
       , fgColor = "#ffffff"
       , position = Bottom
       , commands = [ Run Weather "KONT" ["-t"," <tempF>F","-L","64","-H","77","--normal","green","--high","red","--low","lightblue"] 36000
                    , Run Cpu ["-L","3","-H","50","--normal","green","--high","red"] 10
                    , Run Memory ["-t","Mem: <usedratio>%"] 10
                    , Run Date "%a %b %_d %l:%M" "date" 10
                    , Run BatteryP ["BAT0"]
                                   ["-t", "Batt: <left>% (<timeleft>)<acstatus>",
                                   "--", "-f", "AC0/online",
                                   "-O", " <fc=yellow>++</fc>",
                                   "-i", "", "-o", ""] 10
                    , Run Com "/bin/bash" ["-c", "~/dotfiles/scripts/get_volume.sh"] "vol" 10
                    , Run StdinReader
                    ]
       , sepChar = "%"
       , alignSep = "}{"
       , template = "%StdinReader% }{ %battery% | %vol% | %cpu% | %memory%     <fc=#ee9a00>%date%</fc> | %KONT%"
       }
