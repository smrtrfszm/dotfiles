Config { font = "JetBrains Mono Nerd Font 10"
       , textOffset = 0
       , bgColor = "#1d1d1d"
       , fgColor = "white"
       , alpha = 255
       , position = BottomSize C 100 24
       , borderColor = "black"
       , lowerOnStart = True
       , pickBroadest = False
       , persistent = True
       , hideOnStart = False
       , iconRoot = "~/.config/xmobar/icons"
       , allDesktops = True
       , overrideRedirect = True
       , commands = [ Run Cpu
       			        [ "-t"
			            , "<fc=#86C1B9>Cpu: <total>%</fc>"
		        	    ] 20
                    , Run Memory
		    	        [ "-t"
			            , "<fc=#A1B56C>Mem: <usedratio>%</fc>"
			            ] 20
                    , Run Date "%a %b %-d %H:%M:%S" "date" 11
		            , Run Com "uname" ["-r"] "" 3600
		            , Run DynNetwork
		    	        [ "-t"
			            , "<fc=#F7CA88>Net:</fc> <fc=#DC9656>↑ <tx> KB/s</fc> <fc=#A16946>↓ <rx> KB/s</fc>"
                        ] 20
		            , Run UnsafeStdinReader
                    , Run Battery
                        [ "-t", "Bat: <left>% / <timeleft>"
                        ] 50
                    ]
       , sepChar = "%"
       , alignSep = "}{"
       , template = " %UnsafeStdinReader%}<fc=#F8F8F8>%date%</fc>{<fc=#BA8BAF>%uname%</fc> <fc=#585858>|</fc> %cpu% <fc=#585858>|</fc> %memory% <fc=#585858>|</fc> %dynnetwork% "
       }
