setxkbmap -layout hu -option caps:ctrl_modifier
nitrogen --restore &
dunst &
xmonad --recompile
exec xmonad
