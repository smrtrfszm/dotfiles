setxkbmap -layout hu -option caps:ctrl_modifier
xsetroot -cursor_name left_ptr
nitrogen --restore &
dunst &
stalonetray --window-type normal &
xmonad --recompile
exec xmonad
