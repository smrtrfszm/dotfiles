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
			            , "<fc=#ccc>Cpu:</fc> <total>%"
		        	    ] 20
                    , Run Memory
		    	        [ "-t"
			            , "<fc=#ccc>Mem:</fc> <usedratio>%"
			            ] 20
                    , Run Date "%a %b %_d %H:%M:%S" "date" 10
		            , Run Com "uname" ["-r"] "" 3600
		            , Run Network "enp6s0"
		    	        [ "-t"
			            , "<fc=#ccc>Net:</fc> ↑ <tx> KB/s ↓ <rx> KB/s"
                        ] 20
		            , Run UnsafeStdinReader
                    ]
       , sepChar = "%"
       , alignSep = "}{"
       , template = " %UnsafeStdinReader%}%date%{%uname% | %cpu% | %memory% | %enp6s0% "
       }
