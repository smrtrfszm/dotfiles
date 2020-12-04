Config { font = "xft:JetBrains Mono:pixelsize=14:antialias=true:hinting=true"
       , textOffset = 16
       , bgColor = "#1d1d1d"
       , fgColor = "white"
       , alpha = 255
       , position = TopSize C 100 20
       , borderColor = "black"
       , borderWidth = 1
       , border = BottomB
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
                    , Run Date "%a %b %_d %H:%M:%S" "date" 10
		            , Run Com "uname" ["-r"] "" 3600
		            , Run Network "enp3s0"
		    	        [ "-t"
			            , "<fc=#F7CA88>Net:</fc> <fc=#DC9656>↑ <tx> KB/s</fc> <fc=#A16946>↓ <rx> KB/s</fc>"
                        ] 20
		            , Run UnsafeStdinReader
                    ]
       , sepChar = "%"
       , alignSep = "}{"
       , template = " %UnsafeStdinReader%}<fn=#F8F8F8>%date%</fn>{<fc=#BA8BAF>%uname%</fc> <fc=#585858>|</fc> %cpu% <fc=#585858>|</fc> %memory% <fc=#585858>|</fc> %enp3s0% "
       }
